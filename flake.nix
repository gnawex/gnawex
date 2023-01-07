{
  description = "An independent marketplate for MouseHunt";

  inputs = {
    haskellNix.url      = "github:input-output-hk/haskell.nix";
    nixpkgs.follows     = "haskellNix/nixpkgs-unstable";
    mkdocs-material.url = "github:sekunho/mkdocs-material";
    flake-utils.url     = "github:numtide/flake-utils";
    feedback.url        = "github:NorfairKing/feedback";
  };

  outputs = { self, nixpkgs, flake-utils, mkdocs-material, haskellNix, feedback }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      mkdocs-material-insiders = mkdocs-material.packages.${system}.mkdocs-material-insiders;

      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          gnawex =
            final.haskell-nix.project' {
              src = ./muridae;
              compiler-nix-name = "ghc925";

              shell.tools = {
                cabal = {};
                hlint = {};
                # haskell-language-server = "1.8.0.0";
                fourmolu = {};
                ghcid = {};
              };

              shell.buildInputs = with pkgs; [
                haskellPackages.implicit-hie

                postgresql.lib
                pgformatter
                sqitchPg
                perl534Packages.TAPParserSourceHandlerpgTAP
                mkdocs-material-insiders
                feedback.packages.${system}.default
              ];
            };
        })
      ];

      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

      flake = pkgs.gnawex.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      packages = rec {
        default = muridae-server;
        muridae-server = flake.packages."muridae:exe:muridae-server";
        muridae = flake.packages."muridae:lib:muridae";

        devShells.${system} = {
          default = flake.devShells.default;
          ci = pkgs.mkShell { buildInputs = [ mkdocs-material-insiders ]; };

          ci-db = pkgs.mkShell {
            buildInputs = with pkgs; [
              sqitchPg
              perl534Packages.TAPParserSourceHandlerpgTAP
            ];
          };
        };
      };
    });
}
