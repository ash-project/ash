defmodule Ash.Repo do
  use Ecto.Repo,
    #TODO: Is this wrong? Maybe not? Maybe better to only configure priv/other things
    otp_app: Application.get_env(:ash, :otp_app),
    adapter: Ecto.Adapters.Postgres


  def init(_type, config) do
    database_name = Application.fetch_env!(:ash, :database_name)
    database_username = Application.fetch_env!(:ash, :database_username)
    database_password = Application.fetch_env!(:ash, :database_password)
    database_hostname = Application.fetch_env!(:ash, :database_hostname)
    migration_primary_key = [name: :id, type: :binary_id] # TODO configurable

    new_config =
      config
      |> Keyword.put(:database, database_name)
      |> Keyword.put(:username, database_username)
      |> Keyword.put(:password, database_password)
      |> Keyword.put(:hostname, database_hostname)
      |> Keyword.put(:migration_primary_key, migration_primary_key)

    {:ok, new_config}
  end
end
