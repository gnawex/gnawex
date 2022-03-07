{
  description = "An independent MouseHunt marketplace";

  inputs = {
     nixpkgs.url = "github:NixOS/nixpkgs";
     masterpkgs.url = "github:NixOS/nixpkgs/master";
     postgrestPkg.url = "path:./nix/postgrest";
     flywayPkg.url = "path:./nix/flyway";
     flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, masterpkgs, postgrestPkg, flywayPkg, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          postgrest = postgrestPkg.defaultPackage.${system};
          flyway = flywayPkg.defaultPackage.${system};
          lib =  nixpkgs.lib;
      in {
        devShell = pkgs.mkShell rec {
          buildInputs = [
            postgrest
            flyway
            masterpkgs.legacyPackages.${system}.pgadmin4
          ];
        };
      });
}
