# Taken from https://mediocregopher.com/posts/x-compiling-rust-with-nix
{
  inputs = {
    fenix.url = "github:nix-community/fenix";
    naersk.url = "github:nix-community/naersk/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs, naersk, fenix }:
    let
      buildTargets = {
        "x86_64-linux" = {
          crossSystemConfig = "x86_64-unknown-linux-musl";
          rustTarget = "x86_64-unknown-linux-musl";
        };

        # "i686-linux" = {
        #   crossSystemConfig = "i686-unknown-linux-musl";
        #   rustTarget = "i686-unknown-linux-musl";
        # };

        "aarch64-linux" = {
          crossSystemConfig = "aarch64-unknown-linux-musl";
          rustTarget = "aarch64-unknown-linux-musl";
        };

        "x86_64-windows" = {
          crossSystemConfig = "x86_64-w64-mingw32";
          rustTarget = "x86_64-pc-windows-gnu";
          makeBuildPackageAttrs = pkgsCross: {
            depsBuildBuild = [
              pkgsCross.stdenv.cc
              pkgsCross.windows.pthreads
            ];
          };
        };
      };

      # eachSystem [system] (system: ...)
      #
      # Returns an attrset with a key for every system in the given array, with
      # the key's value being the result of calling the callback with that key.
      eachSystem = supportedSystems: callback: builtins.foldl'
        (overall: system: overall // { ${system} = callback system; })
        {}
        supportedSystems;

      # eachCrossSystem [system] (buildSystem: targetSystem: ...)
      #
      # Returns an attrset with a key "$buildSystem.cross-$targetSystem" for
      # every combination of the elements of the array of system strings. The
      # value of the attrs will be the result of calling the callback with each
      # combination.
      #
      # There will also be keys "$system.default", which are aliases of
      # "$system.cross-$system" for every system.
      #
      eachCrossSystem = supportedSystems: callback:
        eachSystem supportedSystems (buildSystem: builtins.foldl'
            (inner: targetSystem: inner // {
              "cross-${targetSystem}" = callback buildSystem targetSystem;
            })
            { default = callback buildSystem buildSystem; }
            supportedSystems
        );

      mkPkgs = buildSystem: targetSystem: import nixpkgs ({
        system = buildSystem;
      } // (if targetSystem == null then {} else {
        # The nixpkgs cache doesn't have any packages where cross-compiling has
        # been enabled, even if the target platform is actually the same as the
        # build platform (and therefore it's not really cross-compiling). So we
        # only set up the cross-compiling config if the target platform is
        # different.
        crossSystem.config = buildTargets.${targetSystem}.crossSystemConfig;
      }));

    in {
      packages = eachCrossSystem
        (builtins.attrNames buildTargets)
        (buildSystem: targetSystem: let
          pkgs = mkPkgs buildSystem null;
          pkgsCross = mkPkgs buildSystem targetSystem;
          rustTarget = buildTargets.${targetSystem}.rustTarget;

          # TODO I'd prefer to use the toolchain file
          # https://github.com/nix-community/fenix/issues/123
          #toolchain = fenix.packages.${buildSystem}.fromToolchainFile {
          #  file = ./rust-toolchain.toml;
          #  sha256 = "sha256-LU4C/i+maIOqBZagUaXpFyWZyOVfQ3Ah5/JTz7v6CG4=";
          #};

          fenixPkgs = fenix.packages.${buildSystem};

          mkToolchain = fenixPkgs: fenixPkgs.toolchainOf {
            channel = "nightly";
            date = "2023-07-23";
            sha256 = "sha256-LU4C/i+maIOqBZagUaXpFyWZyOVfQ3Ah5/JTz7v6CG4=";
          };

          toolchain = fenixPkgs.combine [
            (mkToolchain fenixPkgs).rustc
            (mkToolchain fenixPkgs).cargo
            (mkToolchain fenixPkgs.targets.${rustTarget}).rust-std
          ];

          buildPackageAttrs = if
            builtins.hasAttr "makeBuildPackageAttrs" buildTargets.${targetSystem}
          then
            buildTargets.${targetSystem}.makeBuildPackageAttrs pkgsCross
          else
            {};

          naersk-lib = pkgs.callPackage naersk {
            cargo = toolchain;
            rustc = toolchain;
          };

        in
          naersk-lib.buildPackage (buildPackageAttrs // rec {
            src = ./.;
            strictDeps = true;
            doCheck = false;
            cargoBuildOptions = default: default ++ [ "--all-features" ];

            # Required because ring crate is special. This also seems to have
            # fixed some issues with the x86_64-windows cross-compile :shrug:
            TARGET_CC = "${pkgsCross.stdenv.cc}/bin/${pkgsCross.stdenv.cc.targetPrefix}cc";

            CARGO_BUILD_TARGET = rustTarget;
            CARGO_BUILD_RUSTFLAGS = [
              "-C" "target-feature=+crt-static"

              # -latomic is required to build openssl-sys for armv6l-linux, but
              # it doesn't seem to hurt any other builds.
              "-C" "link-args=-static -latomic"

              # https://github.com/rust-lang/cargo/issues/4133
              "-C" "linker=${TARGET_CC}"
            ];
          })
        );
    };
}
