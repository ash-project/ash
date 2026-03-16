# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Mix.Tasks.Helpers do
  @moduledoc """
  Helpers for Ash Mix tasks.
  """

  @doc """
  Gets all extensions in use by the current project's domains and resources
  """
  @spec extensions!(argv :: [String.t()], opts :: [in_use?: boolean()]) :: [module()]
  def extensions!(argv, opts \\ []) do
    extensions =
      if opts[:in_use?] do
        Mix.shell().info("Getting extensions in use by resources in current project...")
        domains = Ash.Mix.Tasks.Helpers.domains!(argv)

        extensions =
          Enum.flat_map(
            domains,
            &Ash.Domain.Info.extensions(&1, include_resource_extensions?: true)
          )

        if extensions == [] do
          Mix.shell().info("No extensions in use by resources in current project...")
        end

        extensions
      else
        Mix.shell().info("Getting extensions in current project...")

        apps =
          if Code.ensure_loaded?(Mix.Project) do
            if apps_paths = Mix.Project.apps_paths() do
              apps_paths |> Map.keys() |> Enum.sort()
            else
              [Mix.Project.config()[:app]]
            end
          else
            []
          end

        apps()
        |> Stream.concat(apps)
        |> Stream.uniq()
        |> Task.async_stream(&Ash.Info.defined_extensions/1, timeout: :infinity, ordered: false)
        |> Stream.map(&elem(&1, 1))
        |> Stream.flat_map(& &1)
        |> Stream.uniq()
        |> Enum.to_list()
      end

    sort_extensions(extensions)
  end

  @doc """
  Topologically sorts extensions based on their declared dependencies.

  Extensions can implement `dependencies/0` to return a list of extension modules
  that must run before them. Dependencies not present in the given list are ignored.

  Raises if a circular dependency is detected.
  """
  @spec sort_extensions([module()]) :: [module()]
  def sort_extensions(extensions) do
    extension_set = MapSet.new(extensions)

    # Build dependency graph: extension -> list of extensions it depends on (filtered to present ones)
    graph =
      Map.new(extensions, fn ext ->
        deps =
          if function_exported?(ext, :dependencies, 0) do
            ext.dependencies() |> Enum.filter(&MapSet.member?(extension_set, &1))
          else
            []
          end

        {ext, deps}
      end)

    topological_sort(graph)
  end

  defp topological_sort(graph) do
    nodes = Map.keys(graph)

    in_degrees = Map.new(graph, fn {node, deps} -> {node, length(deps)} end)

    dependents =
      Enum.reduce(graph, Map.new(nodes, &{&1, []}), fn {node, deps}, acc ->
        Enum.reduce(deps, acc, fn dep, acc ->
          Map.update!(acc, dep, &[node | &1])
        end)
      end)

    queue = :queue.from_list(for {node, 0} <- in_degrees, do: node)

    do_topological_sort(queue, in_degrees, dependents, [])
  end

  defp do_topological_sort(queue, in_degrees, dependents, result) do
    case :queue.out(queue) do
      {:empty, _} ->
        sorted = Enum.reverse(result)

        if length(sorted) != map_size(in_degrees) do
          remaining =
            in_degrees
            |> Enum.reject(fn {node, _} -> node in sorted end)
            |> Enum.map(&elem(&1, 0))

          raise """
          Circular dependency detected among extensions: #{Enum.map_join(remaining, ", ", &inspect/1)}

          Please check the `dependencies/0` callbacks on these extensions.
          """
        end

        sorted

      {{:value, node}, queue} ->
        {queue, in_degrees} =
          Enum.reduce(
            Map.get(dependents, node, []),
            {queue, in_degrees},
            fn dependent, {queue, in_degrees} ->
              new_degree = in_degrees[dependent] - 1
              in_degrees = Map.put(in_degrees, dependent, new_degree)
              queue = if new_degree == 0, do: :queue.in(dependent, queue), else: queue
              {queue, in_degrees}
            end
          )

        do_topological_sort(queue, in_degrees, dependents, [node | result])
    end
  end

  Code.ensure_loaded!(Mix.Project)

  if function_exported?(Mix.Project, :deps_tree, 0) do
    # for our app, and all dependency apps, we want to find extensions
    # the benefit of not just getting all loaded applications is that this
    # is actually a surprisingly expensive thing to do for every single built
    # in application for elixir/erlang. Instead we get anything w/ a dependency on ash or spark
    # this could miss things, but its unlikely. And if it misses things, it actually should be
    # fixed in the dependency that is relying on a transitive dependency :)
    defp apps do
      Mix.Project.deps_tree()
      |> Stream.filter(fn {_, nested_deps} ->
        Enum.any?(nested_deps, &(&1 == :spark || &1 == :ash))
      end)
      |> Stream.map(&elem(&1, 0))
    end
  else
    defp apps do
      Logger.warning(
        "Mix.Project.deps_tree/0 not available, falling back to loaded_applications/0. Upgrade to Elixir 1.15+ to make this *much* faster."
      )

      :application.loaded_applications()
      |> Stream.map(&elem(&1, 0))
    end
  end

  @doc """
  Get all domains for the current project and ensure they are compiled.
  """
  def domains!(argv) do
    {opts, _} = OptionParser.parse!(argv, strict: [domains: :string])

    domains =
      if opts[:domains] && opts[:domains] != "" do
        opts[:domains]
        |> Kernel.||("")
        |> String.split(",")
        |> Enum.flat_map(fn
          "" ->
            []

          domain ->
            [Module.concat([domain])]
        end)
      else
        apps =
          if Code.ensure_loaded?(Mix.Project) do
            if apps_paths = Mix.Project.apps_paths() do
              apps_paths |> Map.keys() |> Enum.sort()
            else
              [Mix.Project.config()[:app]]
            end
          else
            []
          end

        Enum.flat_map(apps, &Application.get_env(&1, :ash_domains, []))
      end

    domains
    |> Enum.map(&ensure_compiled(&1, argv))
    |> case do
      [] ->
        raise "must supply the --domains argument, or set `config :my_app, ash_domains: [...]` in config"

      domains ->
        domains
    end
  end

  defp ensure_compiled(domain, args) do
    if Code.ensure_loaded?(Mix.Tasks.App.Config) do
      Mix.Task.run("app.config", args)
    else
      Mix.Task.run("loadpaths", args)
      "--no-compile" not in args && Mix.Task.run("compile", args)
    end

    case Code.ensure_compiled(domain) do
      {:module, _} ->
        # TODO: We shouldn't need to make sure that the resources are compiled
        domain
        |> Ash.Domain.Info.resources()
        |> Enum.each(&Code.ensure_compiled/1)

        domain

      {:error, error} ->
        Mix.raise("Could not load #{inspect(domain)}, error: #{inspect(error)}. ")
    end
  end
end
