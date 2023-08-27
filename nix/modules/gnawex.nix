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

      server_port = {
        default = "3000";
        type = with types; str;
        description = "Port number of the gnawex server";
      };

      server_env = {
        default = "Prod";
        type = with types; str;
        description = "What environment gnawex's server will run as";
      };

      server_secret_key = {
        type = with types; str;
        description = "Secret key of gnawex";
      };

      db_name = {
        type = with types; str;
        description = "DB name of gnawex";
      };

      db_host = {
        type = with types; str;
        description = "DB host of gnawex";
      };

      db_port = {
        default = "5432";
        type = with types; str;
        description = "DB port of gnawex";
      };

      db_user = {
        type = with types; str;
        description = "DB user of gnawex";
      };

      db_password_file = {
        type = with types; str;
        description = "Non-relative path to password of DB user";
      };

      db_ca_cert_file = {
        type = with types; str;
        description = "Non-relative path to DB server CA cert file";
      };

      db_client_cert_file = {
        type = with types; str;
        description = "Non-relative path to DB client cert file";
      };

      db_client_key_file = {
        type = with types; str;
        description = "Non-relative path to DB client key file";
      };

      db_pool_size = {
        default = "10";
        type = with types; str;
        description = "Pool size of DB";
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.gnawex = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      description = "gnawex service";

      serviceConfig = {
        Type = "simple";
        ExecStart = "${gnawex}/bin/gnawex";
      };

      environment = {
        GX_SERVER__PORT = "${cfg.server_port}";
        GX_SERVER__ENV = "${cfg.server_env}";
        GX_SERVER__SECRET_KEY = "${cfg.server_secret_key}";
        GX_DB__NAME = "${cfg.db_name}";
        GX_DB__HOST = "${cfg.db_host}";
        GX_DB__PORT = "${cfg.db_port}";
        GX_DB__USER = "${cfg.db_user}";
        GX_DB__PASSWORD_FILE = "${cfg.db_password_file}";
        GX_DB__POOL_SIZE = "${cfg.db_pool_size}";
        GX_DB__CA_CERT_FILE = "${cfg.db_ca_cert_file}";
        GX_DB__CLIENT_CERT_FILE = "${cfg.db_client_cert_file}";
        GX_DB__CLIENT_KEY_FILE = "${cfg.db_client_key_file}";
      };
    };

    environment.systemPackages = [ gnawex ];
  };
}
