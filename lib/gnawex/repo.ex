defmodule Gnawex.Repo do
  use Ecto.Repo,
    otp_app: :gnawex,
    adapter: Ecto.Adapters.Postgres
end
