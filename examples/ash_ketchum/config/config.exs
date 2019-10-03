# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

# Configures the endpoint
config :ash_ketchum, AshKetchumWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "G+4gHrvzHoJcQa87gYSCpsPbDkGaCxV5K4X4VYVhYDxhURNC0bmn+uVoiDCyRbk/",
  render_errors: [view: AshKetchumWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: AshKetchum.PubSub, adapter: Phoenix.PubSub.PG2]

config :ash_ketchum,
  ecto_repos: [Ash.Repo]

config :ash,
  resources: [
    AshKetchum.Pokemon
  ],
  database_name: "ash_ketchum",
  database_username: "postgres",
  database_password: "postgres",
  database_hostname: "localhost",
  otp_app: :ash_ketchum

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
