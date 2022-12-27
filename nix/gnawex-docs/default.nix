{ mkDerivation
, mkdocs-material-insiders
}:

mkDerivation {
  name = "gnawex-docs";
  src = ../..;

  buildInputs = [ mkdocs-material-insiders ];

  buildPhase = ''
    mkdocs build --config-file mkdocs.yml
  '';

  installPhase = ''
  '';
}
