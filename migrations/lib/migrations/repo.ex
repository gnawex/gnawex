defmodule Migrations.Repo do
  use Ecto.Repo,
    otp_app: :migrations,
    adapter: Ecto.Adapters.Postgres
end
