{
  description = "An independent marketplate for MouseHunt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    mkdocs-material.url = "github:sekunho/mkdocs-material";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    fenix.url = "github:nix-community/fenix";
    naersk.url = "github:nix-community/naersk";
  };

  outputs =
    { self
    , nixpkgs
    , mkdocs-material
    , flake-utils
    , pre-commit-hooks
    , fenix
    , naersk
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      mkdocs-material-insiders =
        mkdocs-material.packages.${system}.mkdocs-material-insiders;

      overlays = [ ];

      pkgs = import nixpkgs { inherit system overlays; };

      fenix' = fenix.packages.${system};

      toolchain = with fenix'; combine [
        stable.rustc
        stable.cargo
        targets.x86_64-unknown-linux-musl.stable.rust-std
      ];

      naersk' = naersk.lib.${system}.override {
        cargo = toolchain;
        rustc = toolchain;
      };
    in
    {
      packages = rec {
        default = gnawex;

        gnawex = naersk'.buildPackage {
          pname = "gnawex";
          version = "0.1.0";
          src = ./.;
          doCheck = false;
          nativeBuildInputs = with pkgs; [ openssl pkg-config ];
        };
      };

      devShells = {
        default =
          let
            rustPackages = with fenix'.stable; [
              rustc
              cargo
              clippy
              rustfmt
              pkgs.rust-analyzer
              pkgs.cargo-flamegraph
            ];
          in
          pkgs.mkShell {
            buildInputs = with pkgs; [
              nil
              nixpkgs-fmt
            ] ++ rustPackages;
          };

        ci = pkgs.mkShell { buildInputs = [ mkdocs-material-insiders ]; };

        ci-db = pkgs.mkShell {
          buildInputs = with pkgs; [
            sqitchPg
            perl534Packages.TAPParserSourceHandlerpgTAP
          ];
        };
      };
    });
}
