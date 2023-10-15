{
  description = "An independent marketplate for MouseHunt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-22-11.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    fenix.url = "github:nix-community/fenix";
    naersk.url = "github:nix-community/naersk";
    devenv.url = "github:cachix/devenv";

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

      gnawex = import ./nix/packages/gnawex.nix {
        inherit pkgs;
        inherit (naersk') buildPackage;
      };
    in
    {
      packages = {
        default = gnawex;
        gnawex-unwrapped = gnawex;

        gnawex = pkgs.symlinkJoin {
          name = "gnawex";
          paths = [ gnawex ];
          buildInputs = [ pkgs.makeWrapper ];

          postBuild = ''
            wrapProgram $out/bin/gnawex \
              --set GX_SERVER__STATIC_ASSETS_PATH "${gnawex}/dist"
          '';
        };

        gce = nixos-generators.nixosGenerate {
          system = "x86_64-linux";
          format = "gce";
        };

        gnawexDocker = pkgs.dockerTools.buildImage {
          name = "gnawex";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ pkgs.bashInteractive ];
            pathsToLink = [ "/bin" ];
          };

          config = {
            Cmd = [ "${self.packages.${system}.gnawex}/bin/gnawex" ];
            WorkingDir = "/app";
            Env = [ ];

            ExposedPorts = {
              "3000/tcp" = { };
            };
          };
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
                openssl
                nodePackages.typescript-language-server
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
                    CREATE USER gnawex LOGIN PASSWORD 'gnawex';
                  '';

                  initialDatabases = [
                    { name = "gnawex_development"; }
                    { name = "gnawex_test"; }
                  ];
                };
              };

              env = {
                GX_SERVER__PORT = "3000";
                GX_SERVER__ENV = "Dev";
                GX_SERVER__SECRET_KEY = "y2T-YcKjJ9WsntIGRPafygHddsoppeduokao0NZZBXPyUlouchBFNPeOScJ0q-mi-JnyunWL-YK7Uc4Djqp4sw";
                GX_SERVER__STATIC_ASSETS_PATH = "dist";
                GX_DB__NAME = "gnawex_development";
                GX_DB__HOST = "127.0.0.1";
                GX_DB__PORT = "5432";
                GX_DB__USER = "gnawex";
                GX_DB__PASSWORD = "gnawex";
                GX_DB__POOL_SIZE = "10";
              };

              languages = {
                nix.enable = true;
                typescript.enable = true;

                rust = {
                  enable = true;

                  toolchain = {
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
