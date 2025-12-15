# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Setup do
  @moduledoc """
  Runs all setup tasks for any extension on any resource/domain in your application.
  """
  use Mix.Task

  @shortdoc "Runs all setup tasks for any extension on any resource/domain in your application."
  @doc @shortdoc
  def run(argv) do
    Mix.Task.run("compile")
    extensions = Ash.Mix.Tasks.Helpers.extensions!(argv)
    {ash_postgres, extensions} = Enum.split_with(extensions, &(&1 == AshPostgres.DataLayer))

    extensions
    |> check_postgres()
    |> Enum.map(fn extension ->
      if function_exported?(extension, :setup, 1) do
        name =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end

        Mix.shell().info("Running setup for #{name}...")
        extension.setup(argv)
      end
    end)
  end

  defp check_postgres([], extensions), do: extensions

  defp check_postgres(ash_postgres, extensions) do
    if check_postgres_connectivity(argv) do
      extensions ++ [ash_postgres]
    else
      Mix.shell().error(
        "Unable to connect to PostgreSQL database. Please ensure your database is running and accessible. Skipping AshPostgres.DataLayer setup."
      )

      extensions
    end
  end

  defp check_postgres_connectivity(argv) do
    argv
    |> Ash.Mix.Tasks.Helpers.domains!()
    |> Enum.flat_map(fn domain ->
      domain
      |> Ash.Domain.Info.resources()
      |> Enum.filter(fn resource ->
        Ash.DataLayer.data_layer(resource) == AshPostgres.DataLayer
      end)
      |> Enum.map(fn resource ->
        AshPostgres.DataLayer.Info.repo(resource)
      end)
    end)
    |> Enum.uniq()
    |> Enum.all?(&can_connect?/1)
  end

  defp can_connect?(repo) do
    try do
      {:ok, _} = Ecto.Adapters.SQL.query(repo, "SELECT 1", [])
      true
    rescue
      DBConnection.ConnectionError -> false
    end
  end
end
