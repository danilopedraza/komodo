{
  description = "The Komodo programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    crane.url = "github:ipetkov/crane";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    typst-packages = {
      url = "github:typst/packages/2cfc68ae23980f656c7971a53667c08e6a6c05df";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      crane,
      rust-overlay,
      typst-packages,
      ...
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import rust-overlay) ];
          };
          rustToolchain = pkgs.rust-bin.stable.latest.default.override {
            extensions = [
              "clippy"
              "rust-src"
              "rustfmt"
            ];
            targets = [ "wasm32-unknown-unknown" ];
          };
          craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
          src = craneLib.cleanCargoSource ./core;
          commonArgs = {
            inherit src;
            strictDeps = true;
            cargoExtraArgs = "--all-features";
          };
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;
          komodo = craneLib.buildPackage (
            commonArgs
            // {
              inherit cargoArtifacts;
              meta.mainProgram = "komodo";
            }
          );
        in
        {
          inherit komodo;
          default = komodo;

          book = pkgs.callPackage ./book { };

          report = pkgs.callPackage ./report {
            buildTypstDocument = pkgs.callPackage ./report/build-typst-document.nix { };
            packagesRepo = typst-packages;
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import rust-overlay) ];
          };
          rustToolchain = pkgs.rust-bin.stable.latest.default.override {
            extensions = [
              "clippy"
              "rust-src"
              "rustfmt"
            ];
          };
          craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
          src = craneLib.cleanCargoSource ./core;
          commonArgs = {
            inherit src;
            strictDeps = true;
            cargoExtraArgs = "--all-features";
          };
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;
        in
        {
          inherit (self.packages.${system}) book komodo report;

          cargo-test = craneLib.cargoTest (
            commonArgs
            // {
              inherit cargoArtifacts;
            }
          );

          clippy = craneLib.cargoClippy (
            commonArgs
            // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets";
            }
          );

          formatting = craneLib.cargoFmt {
            inherit src;
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import rust-overlay) ];
          };
          rustToolchain = pkgs.rust-bin.stable.latest.default.override {
            extensions = [
              "clippy"
              "rust-src"
              "rustfmt"
            ];
            targets = [ "wasm32-unknown-unknown" ];
          };
          craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;
        in
        {
          default = craneLib.devShell {
            checks = self.checks.${system};
            packages =
              with pkgs;
              [
                bashInteractive
                gnumake
                hyperfine
                mdbook
                nil
                nodejs_22
                rust-analyzer
                rustToolchain
                tinymist
                typst
                vsce
                wasm-bindgen-cli
                wasm-pack
              ]
              ++ lib.optional stdenv.isLinux docker-client;
          };
        }
      );
    };
}
