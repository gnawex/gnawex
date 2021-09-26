defmodule GnawexWeb.PageLive do
  use Surface.LiveView

  import Exceptional.Normalize

  @impl true
  def mount(_params, _session, socket) do
    :user.mk_trader({:user, "Sample Name", "sample@email.com", true, true, :trader_role})
    |> normalize()
    |> IO.inspect() # %ErlangError{original: :banned_user}


	  {:ok, assign(socket, hey: "Hello!")}
  end
end
