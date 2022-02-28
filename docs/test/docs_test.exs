defmodule DocsTest do
  use ExUnit.Case
  doctest Docs

  test "greets the world" do
    assert Docs.hello() == :world
  end
end
