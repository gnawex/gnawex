defmodule Migrations.Repo.Migrations.CreateListings do
  use Ecto.Migration

  import Migrations

  def up do
    execute_file "./sql/20220222234300_create_listings.sql"
  end

  def down do
    execute_each """
    DROP TABLE IF EXISTS transactions;

    REVOKE SELECT ON listings FROM anon;

    DROP INDEX IF EXISTS active_id;

    DROP TABLE IF EXISTS listings;

    DROP TYPE IF EXISTS LISTING_TYPE;
    """
  end
end
