{
  description = "An independent marketplate for MouseHunt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    mkdocs-material.url = "github:sekunho/mkdocs-material";
    flake-utils.url = "github:numtide/flake-utils";
    feedback.url = "github:NorfairKing/feedback";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , mkdocs-material
    , feedback
    , pre-commit-hooks
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      libs = with pkgs; [ postgresql zlib ];
      fourmolu = pkgs.haskell.packages.ghc925.fourmolu;

      mkdocs-material-insiders =
        mkdocs-material.packages.${system}.mkdocs-material-insiders;

      srcPaths =
        builtins.concatStringsSep
          " "
          [ "muridae/muridae" "muridae/muridae-server" ];
    in
    {
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;

          hooks = {
            nixpkgs-fmt.enable = true;

            haskell-fmt = {
              # FIXME: Investigate disparity between HLS format and fourmolu CLI
              enable = false;
              name = "Format Haskell";
              files = "\\.(lhs|hs)$";
              entry = "${fourmolu}/bin/fourmolu --mode check " + srcPaths;
            };
          };
        };
      };

      devShells = {
        default = pkgs.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          buildInputs = with pkgs; [
            # Dev tools
            feedback.packages.${system}.default

            # Haskell
            cabal-install
            haskell.compiler.ghc925
            haskell.packages.ghc925.haskell-language-server
            haskell.packages.ghc925.hlint
            haskell.packages.ghc925.implicit-hie
            haskell.packages.ghc925.fourmolu
            haskell.packages.ghc925.cabal-fmt

            # Nix
            nil
            nixpkgs-fmt

            # Postgres
            pgformatter
            sqitchPg
            perl534Packages.TAPParserSourceHandlerpgTAP

            # Need the full `postgresql` because of the include `libpq-fe.h`
            postgresql
            pkg-config

            # Docs
            mkdocs-material-insiders
          ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
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
