{ pkgs, buildPackage }:
(buildPackage {
  pname = "gnawex";
  version = "0.1.0";
  src = ../../.;
  doCheck = false;
  nativeBuildInputs = with pkgs; [ pkg-config ];
  buildInputs = with pkgs; [ openssl ];
}).overrideAttrs (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.esbuild ];

  buildPhase = old.buildPhase + ''
    esbuild \
      --bundle assets/app.css \
      --outfile=dist/style.css \
      --target=chrome58,edge16,firefox57,node12,safari11 \
      --minify
  '';

  installPhase = old.installPhase + ''
    mv dist $out/dist
  '';
})
