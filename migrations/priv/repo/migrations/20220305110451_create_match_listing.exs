defmodule Migrations.Repo.Migrations.CreateMatchListing do
  use Ecto.Migration

  import Migrations

  def up do
    execute_file "./sql/20220228153327_create_match_listing.sql"
  end

  def down do
    execute_each """
    REVOKE SELECT, UPDATE ON listings FROM gnawex_merchant;
    
    REVOKE INSERT ON transactions FROM gnawex_merchant;

    REVOKE anon FROM gnawex_merchant;

    DROP ROLE gnawex_merchant;

    DROP TRIGGER match_listings ON listings;

    DROP FUNCTION match();
    """
  end
end
