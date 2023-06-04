{
  description = "An independent marketplate for MouseHunt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    mkdocs-material.url = "github:sekunho/mkdocs-material/update-to-9";
    flake-utils.url = "github:numtide/flake-utils";
    fenix.url = "github:nix-community/fenix";
    naersk.url = "github:nix-community/naersk";
    devenv.url = "github:cachix/devenv";
  };

  outputs =
    { self
    , nixpkgs
    , mkdocs-material
    , flake-utils
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
        rust-analyzer = toolchain;
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
        stable.rust-src
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
            ({ pkgs, config, ... }: {
              # TODO: Can't add mkdocs-material to shell packages because of:
              # https://github.com/cachix/devenv/issues/601
              # The workaround is to use the CI shell to run `mkdocs serve`.
              packages = with pkgs; [
                nil
                nixpkgs-fmt
                cargo-flamegraph
                sqitchPg
              ];

              pre-commit.hooks = {
                cargo-check.enable = true;
                clippy.enable = true;
                rustfmt.enable = true;
                nixpkgs-fmt.enable = true;
                shellcheck.enable = true;
                statix.enable = true;
                taplo.enable = true;
              };

              services = {
                postgres = {
                  enable = true;
                  package = pkgs.postgresql_15;
                  listen_addresses = "127.0.0.1";

                  initialScript = ''
                    CREATE USER gnawex SUPERUSER LOGIN PASSWORD 'gnawex';
                  '';

                  initialDatabases = [
                    { name = "gnawex_development"; }
                    { name = "gnawex_test"; }
                  ];
                };
              };

              languages = {
                nix.enable = true;

                rust = {
                  enable = true;

                  packages = {
                    inherit (pkgs)
                      cargo
                      rustc
                      rust-analyzer
                      clippy
                      rustfmt
                      rust-src;
                  };
                };
              };
            })
          ];
        };

        ci = pkgs.mkShell { buildInputs = [ mkdocs'.mkdocs-material-insiders ]; };

        ci-db = pkgs.mkShell {
          buildInputs = with pkgs; [
            fenix'.stable.rustc
            fenix'.stable.cargo
            sqitchPg
            perl534Packages.TAPParserSourceHandlerpgTAP
          ];
        };
      };
    });
}
