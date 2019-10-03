defmodule Ash.Repo do
  use Ecto.Repo,
    otp_app: :ash,
    adapter: Ecto.Adapters.Postgres
end
