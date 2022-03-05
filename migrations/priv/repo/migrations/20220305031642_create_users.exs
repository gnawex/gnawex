defmodule Migrations.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  import Migrations

  def up do
    execute_file "./sql/20220222233600_create_users.sql"
  end

  def down do
    execute_each """
    DROP TABLE users;

    DROP TYPE ROLE;

    DROP ROLE authenticator;

    REVOKE USAGE ON SCHEMA public FROM anon;

    DROP ROLE anon;
    """
  end
end
