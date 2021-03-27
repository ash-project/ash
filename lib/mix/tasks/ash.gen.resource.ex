defmodule Mix.Tasks.Ash.Gen.Resource do
  @moduledoc "Initializes new resource in lib/resources/__.ex with default content"

  use Mix.Task
  alias Mix.Tasks.Ash.Helpers

  @template_path Path.join(:code.priv_dir(:ash), "resource.ex.eex")
  @valid_attributes ~w(atom binary boolean cistring date decimal float function integer ) ++
                      ~w(interval map string term uuid utcdatetime utcdatetimeusec)
  @switches [
    api: :string,
    csv: :boolean,
    debug: :boolean,
    ets: :boolean,
    graphql: :boolean,
    guides: :boolean,
    json_api: :boolean,
    mnesia: :boolean,
    phoenix: :boolean,
    policy_authorizer: :boolean,
    postgres: :boolean,
    timestamps: :boolean,
    table_name: :string
  ]
  @default_opts [timestamps: true, guides: true]
  @require_package ~w(json_api graphql postgres policy_authorizer phoenix csv)a

  @impl Mix.Task
  @shortdoc @moduledoc
  @doc """
  Initializes new resource in lib/resources/__.ex with default content

  run as mix ash.resource user first_name last_name password email --postgres

  current valid switches:
   --api :string 
    if specified controls the context folder where the file will be created 
   --table_name :string
    custom database table name
   --csv 
    enables csv data layer
   --debug 
    just writes content of file to terminal
   --ets 
    enables ets data layer
   --graphql 
    enables graphql entries
   --guides 
    enables verbose comments in the generated file - defaults to true - to disable use --no-guides
   --json-api 
    enables json entries
   --mnesia 
    enables mnesia data layer
   --phoenix 
    enables phoenix integation
   --policy_authorizer 
    enables policy authorizer entries
   --postgres 
    enables postgres data layer
   --timestamps 
    enables timestamps generated file - defaults to true - to disable use --no-timestamps
  """
  def run(args) do
    OptionParser.parse(args,
      switches: @switches
    )
    |> case do
      {_, [], _} ->
        print_resource_name_missing_info()

      {switches, [resource | attributes], invalid} ->
        if Enum.count(invalid) > 0 do
          print_invalid_param_info(invalid)
        end

        switches =
          @default_opts
          |> Keyword.merge(switches)

        file_content =
          EEx.eval_file(@template_path,
            module_name: Helpers.capitalize(resource),
            project_name: Helpers.project_module_name(),
            name: resource,
            switches: switches,
            attributes: parse_attributes(attributes)
          )
          |> Code.format_string!()

        if Keyword.get(switches, :debug) do
          file_content
          |> IO.iodata_to_binary()
          |> IO.puts
        else
        # Helpers.write_resource_file(file_content, resource, false)
        end
        print_resource_info(resource)
        print_info(switches)
    end
  end

  def parse_attributes(_, parsed \\ [])
  def parse_attributes([], parsed), do: Enum.reverse(parsed)

  def parse_attributes([attribute, attr_type | rest], parsed)
      when attr_type in @valid_attributes do
    parse_attributes(
      rest,
      [{attribute, String.to_atom(attr_type)} | parsed]
    )
  end

  def parse_attributes([attribute | rest], parsed) do
    parse_attributes(
      rest,
      [{attribute, :string} | parsed]
    )
  end

  def print_info(switches) do
    switches = switches |> Enum.filter(fn {_, bool} -> bool == true end) |> Enum.map(&elem(&1, 0))
    print_deps(switches)
    print_changes(switches)
  end

  defp print_resource_name_missing_info() do
    IO.puts(
      "please specify resource name eg.\n mix ash.gen.resource users name age integer born date"
    )
  end

  defp print_invalid_param_info(list_of_invalid_params) do
    IO.puts("""
    You entered invalid params:
    #{list_of_invalid_params |> Enum.map(&inspect/1) |> Enum.join("\n")}
    remember to use '-' instead of '_'
    """)
  end

  defp print_resource_info(_) do
    IO.puts("""
    Please add your resource to #{Helpers.api_file_name("api")}
    """)
  end
  
  def print_changes(switches) do
    IO.puts("""
    Ensure you made these changes to your app:

    #{Enum.join(Enum.map(switches, &get_info_for/1) |> Enum.filter(&Kernel.!=(&1, nil)), "\n ###### \n")}

    """)
  end

  defp print_deps(switches) do
    IO.puts("""
    Ensure you've added dependencies to your mix.exs file.
    def deps do
      [
         ...

        #{Enum.join(Enum.map(switches, &get_deps_info_for/1) |> Enum.filter(&Kernel.!=(&1, nil)), ",\n    ")}
      ]
    end
    """)
  end

  defp get_deps_info_for(dependency) when dependency in @require_package do
    ~s({:ash_#{dependency}, "~> x.y.z"})
  end

  defp get_deps_info_for(_) do
    nil
  end

  defp get_info_for(:graphql) do
    """
    You can add graphgl configuration to your main context file

    graphql do
      authorize? false # To skip authorization for this API
    end
    """
  end

  defp get_info_for(:postgres) do
    """
    Make sure you ran migrations

    mix ash_postgres.generate_migrations
    """
  end
  defp get_info_for(:json_api) do
    """
    add json api extension to your api file

    defmodule MyApp.Api do
      use Ash.Api, extensions: [AshJsonApi.Api]

      ...

      json_api do
        prefix "/json_api"
        serve_schema? true
        log_errors? true
      end
    end

    This configuration is required to support working with the JSON:API custom mime type.
    Edit your config/config.exs
    config :mime, :types, %{
      "application/vnd.api+json" => ["json"]
    }

    Add the routes from your API module(s)
    In your router, use AshJsonApi.forward/2.

    For example:

    scope "/json_api" do
      pipe_through(:api)

      AshJsonApi.forward("/helpdesk", Helpdesk.Helpdesk.Api)
    end
    """
  end

  defp get_info_for(_), do: nil 
  end
