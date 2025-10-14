# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Extend do
    @example "mix ash.extend My.Domain.Resource postgres,Ash.Policy.Authorizer"
    @moduledoc """
    Adds an extension or extensions to the domain/resource

    Extensions can either be a fully qualified module name, or one of the following list, based on the thing being extended

    ### Ash.Domain

    - `json_api` - `AshJsonApi.Domain`
    - `graphql` - `AshGraphql.Domain`

    ### Ash.Resource

    - `postgres` - `AshPostgres.DataLayer`
    - `sqlite` - `AshSqlite.DataLayer`
    - `mysql` - `AshMysql.DataLayer`
    - `ets` - `Ash.DataLayer.Ets`
    - `mnesia` - `Ash.DataLayer.Mnesia`
    - `embedded` - `data_layer: :embedded`
    - `json_api` - `AshJsonApi.Resource`
    - `graphql` - `AshGraphql.Resource`

    ## Example

    ```bash
    #{@example}
    ```
    """
    @shortdoc "Adds an extension or extensions to the given domain/resource"
    require Igniter.Code.Common
    use Igniter.Mix.Task

    @impl Igniter.Mix.Task
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        positional: [
          :subject,
          extensions: [
            rest: true
          ]
        ],
        schema: [
          sat_solver: :string
        ],
        aliases: [],
        example: @example
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      Mix.Task.run("compile")
      subject = igniter.args.positional.subject
      extensions = igniter.args.positional.extensions

      opts =
        [
          subjects: Enum.uniq(String.split(subject, ",", trim: true)),
          extensions: Enum.uniq(String.split(Enum.join(extensions, ","), ",", trim: true))
        ]

      extensions = opts[:extensions]

      resource_mods = Ash.Resource.Igniter.resource_mods(igniter)

      Enum.reduce(opts[:subjects], igniter, fn subject, igniter ->
        subject = Igniter.Project.Module.parse(subject)

        case Igniter.Project.Module.find_module(igniter, subject) do
          {:error, igniter} ->
            Igniter.add_issue(igniter, "Could not find module to extend: #{subject}")

          {:ok, {igniter, source, zipper}} ->
            case kind_of_thing(zipper, resource_mods) do
              {:ok, kind_of_thing, uses} ->
                {igniter, patchers, _install} =
                  Enum.reduce(extensions, {igniter, [], []}, fn extension,
                                                                {igniter, patchers, install} ->
                    case patcher(
                           kind_of_thing,
                           uses,
                           subject,
                           extension,
                           source.path,
                           igniter.args.argv_flags
                         ) do
                      {fun, new_install} when is_function(fun, 1) ->
                        {igniter, [fun | patchers], install ++ new_install}

                      {:error, error} ->
                        {Igniter.add_issue(igniter, error), patchers, install}
                    end
                  end)

                Enum.reduce(patchers, igniter, fn patcher, igniter ->
                  patcher.(igniter)
                end)

              v when v in [nil, :error] ->
                Igniter.add_issue(
                  igniter,
                  "Could not determine whether #{inspect(subject)} is an `Ash.Resource` or an `Ash.Domain`."
                )
            end
        end
      end)
    end

    defp kind_of_thing(zipper, resource_mods) do
      case Igniter.Code.Common.move_to_do_block(zipper) do
        {:ok, zipper} ->
          resource_mods
          |> Enum.map(&{Ash.Resource, &1})
          |> then(&Enum.concat([{Ash.Domain, Ash.Domain}], &1))
          |> Enum.find_value(fn {type, using} ->
            case Igniter.Code.Module.move_to_use(zipper, using) do
              {:ok, _} -> {:ok, type, using}
              _ -> nil
            end
          end)

        _ ->
          :error
      end
    end

    defp patcher(kind_of_thing, uses, module, extension, path, argv) do
      original_request = extension

      {install, extension} =
        case {kind_of_thing, String.trim_leading(String.downcase(extension), "ash_"), extension} do
          {Ash.Resource, "postgres", _} ->
            {[:ash_postgres], AshPostgres.DataLayer}

          {Ash.Resource, "sqlite", _} ->
            {[:ash_sqlite], AshSqlite.DataLayer}

          {Ash.Resource, "mysql", _} ->
            {[:mysql], AshMysql.DataLayer}

          {Ash.Resource, "ets", _} ->
            {[], Ash.DataLayer.Ets}

          {Ash.Resource, "mnesia", _} ->
            {[], Ash.DataLayer.Mnesia}

          {Ash.Resource, "embedded", _} ->
            {[], &embedded_patcher(&1, module, uses)}

          {Ash.Resource, "json_api", _} ->
            {[:ash_json_api], AshJsonApi.Resource}

          {Ash.Resource, "graphql", _} ->
            {[:ash_graphql], AshGraphql.Resource}

          {Ash.Domain, "json_api", _} ->
            {[:ash_json_api], AshJsonApi.Domain}

          {Ash.Domain, "graphql", _} ->
            {[:ash_graphql], AshGraphql.Domain}

          {_kind_of_thing, _, extension} ->
            {[], extension}
        end

      if is_function(extension) do
        {extension, install}
      else
        extension = Module.concat([extension])

        if Code.ensure_loaded?(extension) do
          fun =
            if function_exported?(extension, :install, 5) do
              fn igniter ->
                extension.install(igniter, module, kind_of_thing, path, argv)
                |> simple_add_extension(kind_of_thing, module, uses, extension)
              end
            else
              &simple_add_extension(&1, kind_of_thing, module, uses, extension)
            end

          {fun, install}
        else
          extensions = Enum.map(Ash.Mix.Tasks.Helpers.extensions!([]), &inspect/1)

          short_codes = [
            {AshJsonApi.Resource, "json_api"},
            {AshPostgres.DataLayer, "postgres"},
            {AshGraphql.Resource, "graphql"},
            {AshMySql.DataLayer, "mysql"},
            {AshSqlite.DataLayer, "sqlite"},
            "ets",
            "mnesia",
            "embedded"
          ]

          installable =
            short_codes
            |> Enum.concat(extensions)
            |> Enum.flat_map(fn
              {dependency, name} ->
                if Code.ensure_loaded?(dependency) do
                  [" * #{name}"]
                else
                  []
                end

              dependency ->
                [" * #{dependency}"]
            end)
            |> Enum.join("\n")

          {:error,
           """
           Could not find extension #{original_request}.

           Possible values for extensions

           #{installable}
           """}
        end
      end
    end

    defp embedded_patcher(igniter, resource, uses) do
      domain =
        resource
        |> Module.split()
        |> :lists.droplast()
        |> Module.concat()

      igniter
      |> remove_domain_option(resource)
      |> Spark.Igniter.add_extension(resource, uses, :data_layer, :embedded, true)
      |> Ash.Domain.Igniter.remove_resource_reference(domain, resource)
      |> Spark.Igniter.update_dsl(
        resource,
        [{:section, :actions}, {:option, :defaults}],
        [:read, :destroy, create: :*, update: :*],
        fn x -> {:ok, x} end
      )
    end

    defp remove_domain_option(igniter, module) do
      Igniter.Project.Module.find_and_update_module!(igniter, module, fn zipper ->
        with {:ok, zipper} <- Igniter.Code.Module.move_to_use(zipper, Ash.Resource),
             {:ok, zipper} <-
               Igniter.Code.Function.update_nth_argument(zipper, 1, fn values_zipper ->
                 Igniter.Code.Keyword.remove_keyword_key(values_zipper, :domain)
               end) do
          {:ok, zipper}
        else
          _ ->
            {:ok, zipper}
        end
      end)
    end

    defp simple_add_extension(igniter, Ash.Resource, module, uses, extension) do
      cond do
        Spark.implements_behaviour?(extension, Ash.DataLayer) ->
          Spark.Igniter.add_extension(igniter, module, uses, :data_layer, extension, true)

        Spark.implements_behaviour?(extension, Ash.Notifier) ->
          Spark.Igniter.add_extension(igniter, module, uses, :notifiers, extension)

        Spark.implements_behaviour?(extension, Ash.Authorizer) ->
          Spark.Igniter.add_extension(igniter, module, uses, :authorizers, extension)

        true ->
          Spark.Igniter.add_extension(igniter, module, uses, :extensions, extension)
      end
    end

    defp simple_add_extension(igniter, _type, module, uses, extension) do
      Spark.Igniter.add_extension(igniter, module, uses, :extensions, extension)
    end
  end
else
  defmodule Mix.Tasks.Ash.Extend do
    @example "mix ash.extend My.Domain.Resource postgres,Ash.Policy.Authorizer"
    @moduledoc """
    Adds an extension or extensions to the domain/resource

    ## Example

    ```bash
    #{@example}
    ```
    """
    @shortdoc "Adds an extension or extensions to the given domain/resource"

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.extend' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end
