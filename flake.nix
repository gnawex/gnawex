{
  description = "An independent MouseHunt marketplace";

  inputs = {
     nixpkgs.url = "github:NixOS/nixpkgs";
     masterpkgs.url = "github:NixOS/nixpkgs/master";
     flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, masterpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          lib =  nixpkgs.lib;
      in {
        devShell = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            sqitchPg
            insomnia
            perl534Packages.TAPParserSourceHandlerpgTAP

            cabal-install
            haskell.compiler.ghc8107
            haskell-language-server

            # Code formatters
            pgformatter
          ];
        };
      });
}
