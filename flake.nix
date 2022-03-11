{
  description = "An independent MouseHunt marketplace";

  inputs = {
     nixpkgs.url = "github:NixOS/nixpkgs";
     masterpkgs.url = "github:NixOS/nixpkgs/master";
     postgrestPkg.url = "path:./nix/postgrest";
     flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, masterpkgs, postgrestPkg, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          postgrest = postgrestPkg.defaultPackage.${system};
          lib =  nixpkgs.lib;
      in {
        devShell = pkgs.mkShell rec {
          buildInputs = [
            pkgs.sqitchPg
            postgrest
            masterpkgs.legacyPackages.${system}.pgadmin4
            pkgs.insomnia
            pkgs.perl534Packages.TAPParserSourceHandlerpgTAP
          ];

          shellHook = ''
          source bin/seed
          '';
        };
      });
}
