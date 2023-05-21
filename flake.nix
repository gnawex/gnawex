{
  description = "An independent marketplate for MouseHunt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    mkdocs-material.url = "github:sekunho/mkdocs-material";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    fenix.url = "github:nix-community/fenix";
    naersk.url = "github:nix-community/naersk";
    devenv.url = "github:cachix/devenv";
  };

  outputs =
    { self
    , nixpkgs
    , mkdocs-material
    , flake-utils
    , pre-commit-hooks
    , fenix
    , naersk
    , devenv
    } @ inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      mkdocs' =
        mkdocs-material.packages.${system};

      rustOverlay = self: super: {
        rustc = toolchain;
        cargo = toolchain;
        clippy = toolchain;
        rustfmt = toolchain;
        rust-src = toolchain;
      };

      overlays = [ rustOverlay ];
      pkgs = import nixpkgs { inherit system overlays; };
      fenix' = fenix.packages.${system};

      toolchain = with fenix'; combine [
        stable.rustc
        stable.cargo
        stable.clippy
        stable.rustfmt
        stable.rust-analyzer
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
        default = devenv.lib.mkShell {
          inherit inputs pkgs;

          modules = [
            ({ pkgs, ... }: {
              packages = with pkgs; [
                nil
                nixpkgs-fmt
                cargo-flamegraph
                mkdocs'.mkdocs-material-insiders
              ];
              languages.nix.enable = true;

              languages.rust = {
                packages = {
                  inherit (pkgs)
                    cargo
                    rustc
                    rust-analyzer
                    clippy
                    rustfmt
                    rust-src;
                };
                enable = true;
              };
            })

          ];
        };

        ci = pkgs.mkShell { buildInputs = [ mkdocs'.mkdocs-material-insiders ]; };

        ci-db = pkgs.mkShell {
          buildInputs = with pkgs; [
            sqitchPg
            perl534Packages.TAPParserSourceHandlerpgTAP
          ];
        };
      };
    });
}
