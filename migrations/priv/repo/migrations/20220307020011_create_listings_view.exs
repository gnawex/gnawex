defmodule Migrations.Repo.Migrations.CreateListingsView do
  use Ecto.Migration

  import Migrations

  def up do
    execute_file "./sql/20220307020011_create_listings_view.sql"
  end

  def down do
    execute_each """
    -- REVOKE SELECT ON item_listings FROM anon;

    -- DROP VIEW item_listings;
    """
  end
end
