{ config, pkgs, lib, gnawex, ... }:
let
  cfg = config.services.gnawex;
in
with lib; {
  options = {
    services.gnawex = {
      enable = mkOption {
        default = false;
        type = with types; bool;
        description = "Enable gnawex service";
      };

      port = {
        default = "3000";
        type = with types; str;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.gnawex = {
      wantedBy = "multi-user.target";
      after = "network.target";
      description = "gnawex service";

      serviceConfig = {
        Type = "simple";
        ExecStart = "${gnawex}/bin/gnawex";
      };

      environment = mkMerge [
        {
          GX__PORT = "${cfg.port}";
          GX__DB_NAME = "${cfg.db_name}";
          GX__DB_HOST = "${cfg.db_host}";
          GX__DB_PORT = "${cfg.db_port}";
          GX__DB_USER = "${cfg.db_user}";
          GX__DB_PASSWORD_FILE = "${cfg.db_password_file}";
          GX__DB_POOL_SIZE = "${cfg.db_pool_size}";
        }

        (mkIf ("${cfg.db_ca_cert_file}" != "") {
          GX__DB_CA_CERT_FILE = "${cfg.db_ca_cert_file}";
        })
      ];
    };
  };

  environment.systemPackages = [ gnawex ];
}
