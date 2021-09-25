defmodule GnawexWeb.PageLive do
  use Surface.LiveView

  @impl true
  def mount(_params, _session, socket) do
	  {:ok, assign(socket, hey: :hello.hello())}
  end
end
