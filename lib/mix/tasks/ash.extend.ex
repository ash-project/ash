defmodule Mix.Tasks.Ash.Extend do
  require Igniter.Code.Common
  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def igniter(igniter, [subject, extensions | argv]) do
    opts =
      [
        subjects: subject,
        extensions: extensions
      ]
      |> Ash.Igniter.csv_option(:extensions)
      |> Ash.Igniter.csv_option(:subjects)

    extensions = opts[:extensions]

    Enum.reduce(opts[:subjects], igniter, fn subject, igniter ->
      subject = Igniter.Code.Module.parse(subject)
      path_to_thing = Igniter.Code.Module.proper_location(subject)

      kind_of_thing = kind_of_thing(igniter, path_to_thing)

      # we currently require that the packages required are already installed
      # probably pretty low hanging fruit to adjust that
      {igniter, patchers, _install} =
        Enum.reduce(extensions, {igniter, [], []}, fn extension, {igniter, patchers, install} ->
          case patcher(kind_of_thing, subject, extension, path_to_thing, argv) do
            {fun, new_install} when is_function(fun, 1) ->
              {igniter, [fun | patchers], install ++ new_install}

            {:error, error} ->
              {Igniter.add_issue(igniter, error), patchers, install}
          end
        end)

      # unless Enum.empty?(install) do
      #   Mix.Shell.info("""
      #   Before proceeding, we must install the following packages:
      #   """)
      #   Igniter.Install.install(install, argv)
      # end

      Enum.reduce(patchers, igniter, fn patcher, igniter ->
        patcher.(igniter)
      end)
    end)
  end

  defp kind_of_thing(igniter, path) do
    igniter = Igniter.include_existing_elixir_file(igniter, path)

    zipper =
      igniter.rewrite
      |> Rewrite.source!(path)
      |> Rewrite.Source.get(:quoted)
      |> Sourceror.Zipper.zip()

    with {_, :error} <-
           {Ash.Resource, Igniter.Code.Module.move_to_module_using(zipper, Ash.Resource)},
         {_, :error} <- {Ash.Domain, Igniter.Code.Module.move_to_module_using(zipper, Ash.Domain)} do
      raise ArgumentError, """
      Could not determine whether the thing at #{path} is an `Ash.Resource` or an `Ash.Domain`.

      It is a current limitation of `mix ash.extend` that it requires the module in question to be
      defined at the "standard" path.

      For example:

      `YourApp.Foo.Bar` -> `lib/your_app/foo/bar.ex`
      """
    else
      {kind_of_thing, {:ok, _}} ->
        kind_of_thing
    end
  end

  defp patcher(kind_of_thing, module, extension, path, argv) do
    original_request = extension

    {install, extension} =
      case {kind_of_thing, String.trim_leading(String.downcase(extension), "ash_"), extension} do
        {Ash.Resource, "postgres", _} ->
          {[:ash_postgres], AshPostgres.DataLayer}

        {Ash.Resource, "sqlite", _} ->
          {[:ash_sqlite], AshMysql.DataLayer}

        {Ash.Resource, "mysql", _} ->
          {[:mysql], AshPostgres.DataLayer}

        {Ash.Resource, "ets", _} ->
          {[], Ash.DataLayer.Ets}

        {Ash.Resource, "mnesia", _} ->
          {[], Ash.DataLayer.Mnesia}

        {Ash.Resource, "embedded", _} ->
          {[], &embedded_patcher(&1, module, path)}

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
      Module.concat([extension])

      if Code.ensure_loaded?(extension) do
        fun =
          if function_exported?(extension, :install, 4) do
            fn igniter ->
              extension.install(igniter, module, kind_of_thing, path, argv)
              |> simple_add_extension(kind_of_thing, path, extension)
            end
          else
            &simple_add_extension(&1, kind_of_thing, path, extension)
          end

        {fun, install}
      else
        extensions = Enum.map(Ash.Mix.Tasks.Helpers.extensions!([]), &inspect/1)

        short_codes = [
          "json_api",
          "postgres",
          "graphql",
          "mysql",
          "sqlite",
          "ets",
          "mnesia",
          "embedded"
        ]

        installable =
          short_codes
          |> Enum.concat(extensions)
          |> Enum.map_join("\n", &" * #{&1}")

        {:error,
         """
         Could not find extension #{original_request}.

         Possible values for extensions

         #{installable}
         """}
      end
    end
  end

  defp embedded_patcher(igniter, resource, path) do
    domain =
      resource
      |> Module.split()
      |> :lists.droplast()
      |> Module.concat()

    igniter
    |> remove_domain_option(path)
    |> Spark.Igniter.add_extension(path, Ash.Resource, :data_layer, :embedded, true)
    |> Ash.Domain.Igniter.remove_resource_reference(domain, resource)
    |> Spark.Igniter.update_dsl(
      Ash.Resource,
      path,
      [{:section, :actions}, {:option, :defaults}],
      [:read, :destroy, create: [], update: []],
      fn x -> x end
    )
  end

  defp remove_domain_option(igniter, path) do
    Igniter.update_elixir_file(igniter, path, fn zipper ->
      with {:ok, zipper} <- Igniter.Code.Module.move_to_module_using(zipper, Ash.Resource),
           {:ok, zipper} <- Igniter.Code.Module.move_to_use(zipper, Ash.Resource),
           {:ok, zipper} <-
             Igniter.Code.Function.update_nth_argument(zipper, 1, fn values_zipper ->
               values_zipper
               |> Igniter.Code.Keyword.remove_keyword_key(:domain)
             end) do
        zipper
      else
        _ ->
          zipper
      end
    end)
  end

  defp simple_add_extension(igniter, Ash.Resource, path, extension) do
    cond do
      Spark.implements_behaviour?(extension, Ash.DataLayer) ->
        Spark.Igniter.add_extension(igniter, path, Ash.Resource, :data_layer, extension, true)

      Spark.implements_behaviour?(extension, Ash.Notifier) ->
        Spark.Igniter.add_extension(igniter, path, Ash.Resource, :notifiers, extension)

      Spark.implements_behaviour?(extension, Ash.Authorizer) ->
        Spark.Igniter.add_extension(igniter, path, Ash.Resource, :authorizers, extension)

      true ->
        igniter
    end
  end

  defp simple_add_extension(igniter, type, path, extension) do
    Spark.Igniter.add_extension(igniter, path, type, :extensions, extension)
  end
end
