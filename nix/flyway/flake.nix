{
  # Flake-ified `flyway` package from
  # https://github.com/NixOS/nixpkgs/blob/02dfbde63938f077e21d7880781132ea141b0b21/pkgs/development/tools/flyway/default.nix
  description = "Evolve your Database Schema easily and reliably across all your instances";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = {self, nixpkgs}: {
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };

      stdenv.mkDerivation rec {
        name = "flyway-${version}";
        version = "8.5.2";

        src = fetchurl {
          url = "mirror://maven/org/flywaydb/flyway-commandline/${version}/flyway-commandline-${version}.tar.gz";
          sha256 = "sha256-sUCa9lbZ2c5WqAjEswQxV4AyfTfYFATH892CIkV11FM=";
        };

        sourceRoot = ".";
        nativeBuildInputs = [ pkgs.makeWrapper ];
        dontBuild = true;
        dontStrip = true;
        installPhase = ''
          mkdir -p $out/bin $out/share/flyway
          cd flyway-${version}
          ls -la
          cp -r sql jars drivers conf $out/share/flyway
          install -Dt $out/share/flyway/lib lib/community/*.jar lib/*.jar
          makeWrapper "${pkgs.jre_headless}/bin/java" $out/bin/flyway \
            --add-flags "-Djava.security.egd=file:/dev/../dev/urandom" \
            --add-flags "-classpath '$out/share/flyway/lib/*:$out/share/flyway/drivers/*'" \
            --add-flags "org.flywaydb.commandline.Main" \
            --add-flags "-jarDirs='$out/share/flyway/jars'"
        '';

        meta = with lib; {
          description = "Evolve your Database Schema easily and reliably across all your instances";
          longDescription = ''
            The Flyway command-line tool is a standalone Flyway distribution.
            It is primarily meant for users who wish to migrate their database from the command-line
            without having to integrate Flyway into their applications nor having to install a build tool.
            This package is only the Community Edition of the Flyway command-line tool.
          '';
          homepage = "https://flywaydb.org/";
          license = licenses.asl20;
          platforms = platforms.unix;
        };
      };
  };
}
