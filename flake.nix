{
  description = "An independent marketplate for MouseHunt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-22-11.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    fenix.url = "github:nix-community/fenix";
    naersk.url = "github:nix-community/naersk";
    devenv.url = "github:cachix/devenv/v0.6.3";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs-22-11";
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22-11
    , flake-utils
    , fenix
    , naersk
    , devenv
    , nixos-generators
    } @ inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
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

        gnawex-static = naersk'.buildPackage {
          src = ./.;
          doCheck = false;
          nativeBuildInputs = with pkgs; [ pkgsStatic.stdenv.cc ];
          CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
          CARGO_BUILD_RUSTFLAGS = "-C target-feature=+crt-static";
        };

        gce = nixos-generators.nixosGenerate {
          system = "x86_64-linux";
          format = "gce";
        };
      };

      nixosModules.default = import ./nix/modules/gnawex.nix;

      devShells = {
        default = devenv.lib.mkShell {
          inherit inputs pkgs;

          modules = [
            ({ pkgs, config, ... }: {
              packages = with pkgs; [
                nil
                nixpkgs-fmt
                cargo-flamegraph
                cargo-watch
                sqitchPg
                esbuild
                scc
                google-cloud-sdk
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
                  extensions = extensions: [ extensions.pg_cron ];

                  settings = {
                    "cron.database_name" = "gnawex_development";
                    "shared_preload_libraries " = "pg_cron";
                  };

                  initialScript = ''
                    CREATE USER gnawex SUPERUSER LOGIN PASSWORD 'gnawex';
                  '';

                  initialDatabases = [
                    { name = "gnawex_development"; }
                    { name = "gnawex_test"; }
                  ];
                };
              };

              env = {
                GX__PORT = "3000";
                GX__DB_NAME = "gnawex_development";
                GX__DB_HOST = "127.0.0.1";
                GX__DB_PORT = "5432";
                GX__DB_USER = "gnawex";
                GX__DB_PASSWORD = "gnawex";
                GX__DB_POOL_SIZE = "10";
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
