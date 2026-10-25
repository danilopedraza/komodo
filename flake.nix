{
  description = "The Komodo programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    crane.url = "github:ipetkov/crane";
    bencher.url = "github:bencherdev/bencher";
    bencher.inputs.nixpkgs.follows = "nixpkgs";
    bencher.inputs.rust-overlay.follows = "rust-overlay";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    typst-packages = {
      url = "github:typst/packages/2cfc68ae23980f656c7971a53667c08e6a6c05df";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, crane, rust-overlay, typst-packages, bencher, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux"
      ];
      perSystem = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ (import rust-overlay) ]; };
          rustToolchain = pkgs.rust-bin.stable.latest.default.override {
            extensions = [ "clippy" "rust-src" "rustfmt" ];
            targets = [ "wasm32-unknown-unknown" ]
              ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.pkgsStatic.stdenv.hostPlatform.rust.rustcTarget;
          };
          craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
          commonArgs = {
            src = craneLib.cleanCargoSource ./core;
            strictDeps = true;
            cargoExtraArgs = "--all-features";
          };
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;
          komodo = craneLib.buildPackage (commonArgs // {
            inherit cargoArtifacts;
            meta.mainProgram = "komodo";
          });
          benchmarkArgs = {
            pname = "komodo-bench-tools";
            version = "0.1.0";
            src = pkgs.lib.cleanSourceWith {
              src = ./.;
              filter = path: type:
                let rel = pkgs.lib.removePrefix (toString ./. + "/") path;
                in (rel == "core" || rel == "bench"
                  || pkgs.lib.hasPrefix "core/" rel || pkgs.lib.hasPrefix "bench/" rel)
                  && craneLib.filterCargoSources path type;
            };
            cargoLock = ./bench/Cargo.lock;
            cargoToml = ./bench/Cargo.toml;
            postUnpack = ''sourceRoot="$sourceRoot/bench"'';
            strictDeps = true;
          };
          benchmarkTools = craneLib.buildPackage (benchmarkArgs // {
            cargoArtifacts = craneLib.buildDepsOnly benchmarkArgs;
          });
          benchmarkSources = pkgs.lib.cleanSourceWith {
            src = ./bench;
            filter = path: type:
              (type == "directory" && path == toString ./bench)
              || (type == "regular" && (pkgs.lib.hasSuffix ".komodo" path
                || builtins.baseNameOf path == "compare.sh"));
          };
          benchmarkInputs = pkgs.runCommand "komodo-benchmark-inputs" { } ''
            mkdir -p $out/bench
            cp ${benchmarkSources}/*.komodo $out/bench/
            cp ${./bench/compare.sh} $out/bench/compare.sh
            cp -r ${./std} $out/std
          '';
          # Release downloads must also run on Linux machines without /nix/store.
          releaseArgs = commonArgs // {
            CARGO_BUILD_TARGET = pkgs.pkgsStatic.stdenv.hostPlatform.rust.rustcTarget;
            # Keep build scripts on the native GNU host; only the output targets musl.
            "CARGO_TARGET_${pkgs.lib.toUpper (builtins.replaceStrings [ "-" ] [ "_" ] pkgs.pkgsStatic.stdenv.hostPlatform.rust.rustcTarget)}_LINKER" =
              "${pkgs.pkgsStatic.stdenv.cc}/bin/${pkgs.pkgsStatic.stdenv.cc.targetPrefix}cc";
          };
          komodo-release = if pkgs.stdenv.isLinux then
            craneLib.buildPackage (releaseArgs // {
              cargoArtifacts = craneLib.buildDepsOnly releaseArgs;
              meta.mainProgram = "komodo";
            }) else komodo;
          # Preserve the relative path dependency from core-browser to core.
          browserArgs = {
            src = pkgs.lib.cleanSourceWith {
              src = ./.;
              filter = path: type:
                let rel = pkgs.lib.removePrefix (toString ./. + "/") path;
                in (rel == "core" || rel == "core-browser"
                  || pkgs.lib.hasPrefix "core/" rel || pkgs.lib.hasPrefix "core-browser/" rel)
                  && craneLib.filterCargoSources path type;
            };
            cargoLock = ./core-browser/Cargo.lock;
            cargoToml = ./core-browser/Cargo.toml;
            postUnpack = ''sourceRoot="$sourceRoot/core-browser"'';
            strictDeps = true;
          };
          browserArtifacts = craneLib.buildDepsOnly browserArgs;
          wasmArgs = browserArgs // {
            CARGO_BUILD_TARGET = "wasm32-unknown-unknown";
            doCheck = false;
          };
          playground = craneLib.mkCargoDerivation (wasmArgs // {
            pname = "komodo-playground";
            cargoArtifacts = craneLib.buildDepsOnly wasmArgs;
            nativeBuildInputs = [ pkgs.wasm-bindgen-cli ];
            buildPhaseCargoCommand = "cargo build --release --locked --target wasm32-unknown-unknown";
            installPhaseCommand = ''
              mkdir -p $out/komodo
              wasm-bindgen --target web --out-dir $out/komodo \
                target/wasm32-unknown-unknown/release/komodo_browser.wasm
              cp ${./core-browser}/index.{html,css,js} $out/
            '';
          });
          book = pkgs.callPackage ./book { };
          report = pkgs.callPackage ./report {
            buildTypstDocument = pkgs.callPackage ./report/build-typst-document.nix { };
            packagesRepo = typst-packages;
          };
          website = pkgs.runCommand "komodo-website" { } ''
            mkdir -p $out
            cp ${./website}/index.{html,css} $out/
            cp -r ${playground} $out/play
            cp -r ${book}/book $out/book
            cp ${report}/report.pdf $out/
            cp ${./installer/linux/install.sh} $out/install.sh
          '';
          ovsx = pkgs.buildNpmPackage {
            pname = "komodo-extension-publisher";
            version = "1.0.0";
            src = ./nix/ovsx;
            npmDeps = pkgs.importNpmLock { npmRoot = ./nix/ovsx; };
            npmConfigHook = pkgs.importNpmLock.npmConfigHook;
            # Upload an existing VSIX; no native keychain or signing add-ons are needed.
            npmFlags = [ "--ignore-scripts" ];
            dontNpmBuild = true;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            installPhase = ''
              mkdir -p $out/lib $out/bin
              cp -r node_modules $out/lib/
              makeWrapper ${pkgs.nodejs}/bin/node $out/bin/ovsx \
                --add-flags $out/lib/node_modules/ovsx/bin/ovsx
            '';
          };
          extension = pkgs.stdenvNoCC.mkDerivation {
            pname = "komodo-analyzer";
            version = (builtins.fromJSON (builtins.readFile ./vsc-extension/package.json)).version;
            src = ./vsc-extension;
            nativeBuildInputs = [ pkgs.vsce ];
            # ZIP timestamps must not depend on the time of the CI run.
            buildPhase = ''
              export HOME=$TMPDIR
              find . -type f -exec touch -t 198001010000 {} +
              vsce package --no-dependencies --out extension.vsix
            '';
            installPhase = ''mkdir -p $out; cp extension.vsix $out/'';
          };
          checks = {
            inherit komodo komodo-release playground book report website extension;
            benchmark-tools = benchmarkTools;
            benchmark-formatting = craneLib.cargoFmt benchmarkArgs;
            benchmark-clippy = craneLib.cargoClippy (benchmarkArgs // {
              cargoArtifacts = craneLib.buildDepsOnly benchmarkArgs;
              cargoClippyExtraArgs = "--all-targets -- -D warnings";
            });
            benchmark-workloads = pkgs.runCommand "benchmark-workloads" { } ''
              export KOMODO_STD=${benchmarkInputs}/std
              for workload in ${benchmarkInputs}/bench/*.komodo; do
                ${komodo}/bin/komodo "$workload"
              done
              touch $out
            '';
            ci-workflows = pkgs.runCommand "ci-workflows" { nativeBuildInputs = [ pkgs.actionlint ]; } ''
              actionlint -shellcheck= ${./.github/workflows}/*.yml
              touch $out
            '';
            benchmark-script = pkgs.runCommand "benchmark-script" { nativeBuildInputs = [ pkgs.shellcheck ]; } ''
              shellcheck -s bash ${./nix/bench.sh}
              shellcheck -s bash ${./bench/compare.sh}
              touch $out
            '';
            extension-publisher = pkgs.runCommand "extension-publisher-check" { } ''
              ${ovsx}/bin/ovsx --version
              touch $out
            '';
            cargo-test = craneLib.cargoTest (commonArgs // { inherit cargoArtifacts; });
            clippy = craneLib.cargoClippy (commonArgs // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets";
            });
            formatting = craneLib.cargoFmt { inherit (commonArgs) src; };
            browser-test = craneLib.cargoTest (browserArgs // {
              cargoArtifacts = browserArtifacts;
              # Browser tests share the interpreter's global input/output buffers.
              cargoTestExtraArgs = "-- --test-threads=1";
            });
            browser-clippy = craneLib.cargoClippy (browserArgs // {
              cargoArtifacts = browserArtifacts;
              cargoClippyExtraArgs = "--all-targets";
            });
            browser-formatting = craneLib.cargoFmt browserArgs;
            examples = pkgs.runCommand "komodo-examples" { nativeBuildInputs = [ komodo ]; } ''
              export KOMODO_STD=${./std}/
              while IFS= read -r -d "" file; do
                case "$file" in */sort-types.komodo) continue ;; esac
                echo "Running $file"
                komodo "$file"
              done < <(find ${./examples} -type f -name '*.komodo' -print0)
              touch $out
            '';
            smoke = pkgs.runCommand "komodo-smoke" { } ''
              echo 'println("Smoke test!")' > smoke.komodo
              ${komodo-release}/bin/komodo smoke.komodo | grep -Fx 'Smoke test!'
              touch $out
            '';
          };
          checkApps = pkgs.lib.mapAttrs (name: check: {
            type = "app";
            meta.description = "Run the ${name} check";
            program = "${pkgs.writeShellScript "check-${name}" ''
              echo "Passed: ${name} (${check})"
            ''}";
          }) checks;
        in {
          packages = {
            inherit komodo komodo-release playground book report website extension;
            benchmark-tools = benchmarkTools;
            extension-publisher = ovsx;
            default = komodo;
          };
          inherit checks;
          apps = checkApps // {
            default = { type = "app"; meta.description = "Run Komodo"; program = "${komodo}/bin/komodo"; };
            komodo = { type = "app"; meta.description = "Run Komodo"; program = "${komodo}/bin/komodo"; };
            runtime-layout = {
              type = "app";
              meta.description = "Measure interpreter data structure sizes";
              program = "${benchmarkTools}/bin/runtime-layout";
            };
            bench-compare = {
              type = "app";
              meta.description = "Compare two packaged interpreters against the benchmark suite";
              program = "${pkgs.writeShellApplication {
                name = "komodo-bench-compare";
                runtimeInputs = [ pkgs.bash pkgs.hyperfine ];
                text = ''exec bash ${benchmarkInputs}/bench/compare.sh "$@"'';
              }}/bin/komodo-bench-compare";
            };
            bench-memory = {
              type = "app";
              meta.description = "Sample benchmark peak RSS on Linux";
              program = "${pkgs.writeShellApplication {
                name = "komodo-bench-memory";
                text = ''
                  export KOMODO_BENCH_ROOT=${benchmarkInputs}
                  exec ${benchmarkTools}/bin/memory "$@"
                '';
              }}/bin/komodo-bench-memory";
            };
            bench = {
              type = "app";
              meta.description = "Compare interpreter benchmarks against a previous commit";
              program = "${pkgs.writeShellApplication {
                name = "komodo-bench";
                runtimeInputs = [ pkgs.nix pkgs.git pkgs.bash pkgs.hyperfine pkgs.jq bencher.packages.${system}.bencher pkgs.coreutils pkgs.gnutar ];
                text = builtins.readFile ./nix/bench.sh;
              }}/bin/komodo-bench";
            };
            publish-extension = {
              type = "app";
              meta.description = "Publish the packaged extension to both registries";
              program = "${pkgs.writeShellApplication {
                name = "publish-extension";
                runtimeInputs = [ pkgs.vsce ovsx ];
                text = ''
                  vsce publish --packagePath ${extension}/extension.vsix --skip-duplicate
                  ovsx publish ${extension}/extension.vsix --skip-duplicate
                '';
              }}/bin/publish-extension";
            };
            record-benchmark = {
              type = "app";
              meta.description = "Record the benchmarked commit in GitHub Actions";
              program = "${pkgs.writeShellApplication {
                name = "record-benchmark";
                runtimeInputs = [ pkgs.gh ];
                text = ''gh variable set SECOND_TO_LAST_PUSH_HASH --body "''${GITHUB_SHA:?}"'';
              }}/bin/record-benchmark";
            };
          };
          devShells.default = craneLib.devShell {
            inherit checks;
            packages = with pkgs; [
              bashInteractive gnumake hyperfine mdbook nil nodejs_22 python3 rust-analyzer
              rustToolchain tinymist typst vsce wasm-bindgen-cli wasm-pack
            ] ++ lib.optional stdenv.isLinux docker-client;
          };
        });
    in {
      packages = forAllSystems (system: perSystem.${system}.packages);
      checks = forAllSystems (system: perSystem.${system}.checks);
      apps = forAllSystems (system: perSystem.${system}.apps);
      devShells = forAllSystems (system: perSystem.${system}.devShells);
    };
}
