defmodule Migrations.Repo.Migrations.CreateRole do
  use Ecto.Migration

  import Migrations

  def up do
    execute_file "./sql/20220301143724_create_role.sql"
  end

  def down do
    execute_each """
    DROP POLICY txn_read ON transactions;

    DROP POLICY txn_new ON transactions;

    ALTER TABLE transactions DISABLE ROW LEVEL SECURITY;

    DROP POLICY listing_update ON listings;

    DROP POLICY listing_new ON listings;

    DROP POLICY listing_read ON listings;

    ALTER TABLE listings DISABLE ROW LEVEL SECURITY;

    REVOKE EXECUTE ON FUNCTION match() FROM valid_user;

    REVOKE SELECT (id, is_banned, is_verified) ON users FROM valid_user;

    REVOKE SELECT, INSERT ON transactions FROM valid_user;

    REVOKE SELECT, UPDATE ON listings_id_seq FROM valid_user;

    REVOKE SELECT, INSERT, UPDATE ON listings FROM valid_user;

    REVOKE SELECT ON items FROM valid_user;

    REVOKE valid_user FROM sekun;

    DROP USER sekun;

    DROP ROLE valid_user;
    """
  end
end
