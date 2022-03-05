defmodule Migrations do
  import Ecto.Migration.Runner

  @moduledoc false

  @doc """
  Executes multiple statements in a file that are delimited with two newlines.
  Here's an example file that would work:

  \"""
  CREATE ROLE anon;

  CREATE ROLE authenticator;
  \"""

  While this won't:

  \"""
  CREATE ROLE anon;
  CREATE ROLE authenticator;
  \"""

  Since the former will be split into two separate strings that get passed to
  `execute/1`, while the latter is treated as one string. This would of course
  depend on the statement itself since some cannot be combined into one.
  """
  def execute_file(filepath) do
    filepath
    |> File.read!()
    |> execute_each()
  end

  @doc """
  Splits the SQL script based on the double newline, and executes each.
  """
  def execute_each(content) do
    content
    |> String.split("\n\n")
    |> Enum.each(&execute/1)
  end
end
