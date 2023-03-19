{
  description = "An independent marketplate for MouseHunt";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    mkdocs-material.url = "github:sekunho/mkdocs-material";
    flake-utils.url = "github:numtide/flake-utils";
    feedback.url = "github:NorfairKing/feedback";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , haskellNix
    , nixpkgs
    , mkdocs-material
    , flake-utils
    , feedback
    , pre-commit-hooks
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      mkdocs-material-insiders =
        mkdocs-material.packages.${system}.mkdocs-material-insiders;

      overlays = [
        haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          muridaeProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc925";

              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
                fourmolu = { };
                cabal-fmt = { };
              };

              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                haskellPackages.implicit-hie
                # Nix
                nil
                nixpkgs-fmt
                postgresql.lib
                pgformatter
                sqitchPg
                perl534Packages.TAPParserSourceHandlerpgTAP
                mkdocs-material-insiders
                feedback.packages.${system}.default
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.muridaeProject.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:muridae:exe:muridae`
        # crossPlatforms = p: [p.ghcjs];
      };
    in
    flake // rec {
      packages = rec {
        default = muridae-server;
        muridae-server = flake.packages."muridae:exe:muridae-server";
        muridae = flake.packages."muridae:lib:muridae";
        muridae-db = flake.packages."muridae:lib:muridae-db";
        muridae-json = flake.packages."muridae:lib:muridae-json";
        muridae-api = flake.packages."muridae:lib:muridae-api";
        muridae-test = flake.packages."muridae:test:muridae-test";
      };

      apps.default = {
        type = "app";
        program = "${packages.muridae-server}/bin/muridae-server";
      };

      devShells = {
        default = flake.devShells.default;
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
