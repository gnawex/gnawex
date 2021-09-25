{
  description = "An independent MouseHunt marketplace";

  inputs = {
     nixpkgs.url = "github:NixOS/nixpkgs";
     flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          lib =  nixpkgs.lib;
          erlang = pkgs.beam.interpreters.erlangR24;
          elixir = pkgs.beam.packages.erlangR24.elixir_1_12;
      in {
        devShell = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            # compilers
            elixir
            erlang
            gleam

            # dev tools
            elixir_ls

            cacert
            inotify-tools

          ];

          # So that EVM doesn't complain about incorrect locale.
          LANG = "C.UTF-8";
          LC_TYPE = "C.UTF-8";
        };
      });
}
