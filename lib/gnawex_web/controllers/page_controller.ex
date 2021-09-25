defmodule GnawexWeb.PageController do
  use GnawexWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
