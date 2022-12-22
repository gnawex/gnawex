{
  description = "An independent marketplate for MouseHunt";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
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
                haskell-language-server = {};
                fourmolu = {};
              };

              shell.buildInputs = with pkgs; [
                haskellPackages.implicit-hie

                postgresql.lib
                pgformatter
                sqitchPg
                perl534Packages.TAPParserSourceHandlerpgTAP
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
        muridae-server = flake.packages."muridae:exe:muridae-server";
        muridae-db = flake.packages."muridae:lib:muridae-db";
        muridae = flake.packages."muridae:lib:muridae";
        default = muridae-server;
      };
    });
}
