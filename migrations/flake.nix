{
  description = "An independent MouseHunt marketplace";

  inputs = {
     nixpkgs.url     = "github:NixOS/nixpkgs";
     flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          lib =  nixpkgs.lib;
      in {
        devShell = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            erlang
            elixir
            elixir_ls
          ];

          shellHook = ''
          export LANG="en_US.UTF-8"
          export LC_ALL="en_US.UTF-8"
          '';
        };
      });
}
