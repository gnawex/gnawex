defmodule Migrations.Repo.Migrations.CreateItems do
  use Ecto.Migration

  import Migrations

  def up do
    execute_file "./sql/20220222234200_create_items.sql"
  end

  def down do
    execute "DROP TABLE items";
  end
end

