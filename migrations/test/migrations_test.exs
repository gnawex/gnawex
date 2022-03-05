defmodule MigrationsTest do
  use ExUnit.Case
  doctest Migrations

  test "greets the world" do
    assert Migrations.hello() == :world
  end
end
