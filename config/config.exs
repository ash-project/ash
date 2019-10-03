use Mix.Config

config :ash,
  ecto_repos: [Ash.Repo],
  migration_primary_key: [name: :id, type: :binary_id]

config :ash, Ash.Repo,
  database: "ash",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"
