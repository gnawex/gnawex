import Config

config :migrations, ecto_repos: [Migrations.Repo]

config :migrations, Migrations.Repo,
  database: "gnawex_db",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"
