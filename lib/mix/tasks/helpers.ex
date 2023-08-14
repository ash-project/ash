defmodule Ash.Mix.Tasks.Helpers do
  @moduledoc """
  Helpers for Ash Mix tasks.
  """

  @doc """
  Gets all extensions in use by the current project's apis and resources
  """
  def extensions!(argv \\ []) do
    apis = Ash.Mix.Tasks.Helpers.apis!(argv)

    resource_extensions =
      apis
      |> Enum.flat_map(&Ash.Api.Info.resources/1)
      |> all_extensions()

    Enum.uniq(all_extensions(apis) ++ resource_extensions)
  end

  @doc """
  Get all apis for the current project and ensure they are compiled.
  """
  def apis!(argv \\ []) do
    {opts, _} = OptionParser.parse!(argv, strict: [apis: :string])

    apis =
      if opts[:apis] && opts[:apis] != "" do
        opts[:apis]
        |> Kernel.||("")
        |> String.split(",")
        |> Enum.flat_map(fn
          "" ->
            []

          api ->
            [Module.concat([api])]
        end)
      else
        apps =
          if apps_paths = Mix.Project.apps_paths() do
            apps_paths |> Map.keys() |> Enum.sort()
          else
            [Mix.Project.config()[:app]]
          end

        Enum.flat_map(apps, &Application.get_env(&1, :ash_apis, []))
      end

    apis
    |> Enum.map(&ensure_compiled(&1, argv))
    |> case do
      [] ->
        raise "must supply the --apis argument, or set `config :my_app, ash_apis: [...]` in config"

      apis ->
        apis
    end
  end

  defp all_extensions(modules) do
    modules
    |> Enum.flat_map(&Spark.extensions/1)
    |> Enum.uniq()
  end

  defp ensure_compiled(api, args) do
    if Code.ensure_loaded?(Mix.Tasks.App.Config) do
      Mix.Task.run("app.config", args)
    else
      Mix.Task.run("loadpaths", args)
      "--no-compile" not in args && Mix.Task.run("compile", args)
    end

    case Code.ensure_compiled(api) do
      {:module, _} ->
        # TODO: We shouldn't need to make sure that the resources are compiled
        api
        |> Ash.Api.Info.resources()
        |> Enum.each(&Code.ensure_compiled/1)

        api

      {:error, error} ->
        Mix.raise("Could not load #{inspect(api)}, error: #{inspect(error)}. ")
    end
  end
end
