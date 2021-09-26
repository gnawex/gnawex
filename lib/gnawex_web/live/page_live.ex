defmodule GnawexWeb.PageLive do
  use Surface.LiveView

  @impl true
  def mount(_params, _session, socket) do
    IO.inspect(:user.mk_trader({:user, "Sample Name", "sample@email.com", true, true, :trader_role}))

	  {:ok, assign(socket, hey: "Hello!")}
  end
end
