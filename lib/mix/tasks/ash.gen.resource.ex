defmodule Mix.Tasks.Ash.Gen.Resource do
  @moduledoc "Initializes new resource in lib/resources/__.ex with default content"

  use Mix.Task
  alias Mix.Tasks.Ash.{Helpers, Templates}

  @template_path Path.join(:code.priv_dir(:ash), "resource.ex.eex")
  @valid_attributes ~w(atom binary boolean cistring date decimal float function integer ) ++
                      ~w(interval map string term uuid utcdatetime utcdatetimeusec)
  @cmd_switches [
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

  def require_package, do: @require_package
  def valid_attributes, do: @valid_attributes

  @impl Mix.Task
  @shortdoc @moduledoc
  @doc """
  Initializes new resource in lib/resources/__.ex with default content

  run as mix ash.resource user first_name last_name password email --postgres

  current valid cmd_switches:
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
  @spec run(any()) :: nil
  def run(args) do
    options =
      OptionParser.parse(args,
        switches: @cmd_switches
      )

    with {file_content, cmd_switches, resource, context_name} <- generate_file(options) do
      if Keyword.get(cmd_switches, :debug) do
        file_content
        |> Mix.shell().info()
      else
        Helpers.write_resource_file(file_content, resource, context_name)
      end

      Templates.print_resource_info(resource, context_name)
      Templates.print_info(cmd_switches)
    end
  end

  @spec generate_file(OptionParser.t()) :: {String.t(), list(), String.t(), String.t()}
  def generate_file({_, [], _}), do: Templates.print_resource_name_missing_info()

  def generate_file({_, _, invalid}) when length(invalid) > 0,
    do: Templates.print_invalid_param_info(invalid)

  def generate_file({_, [_resource], _}), do: Templates.print_missing_attributes()

  def generate_file({cmd_switches, [resource_name | attributes], _}) do
    cmd_switches = Keyword.merge(@default_opts, cmd_switches)
    context_name = Keyword.get(cmd_switches, :api, nil)

    assigns = %{
      module_name: Helpers.capitalize(resource_name),
      project_name: Helpers.project_module_name(),
      name: resource_name,
      table_name: Keyword.get(cmd_switches, :table_name, resource_name),
      cmd_switches: cmd_switches,
      attributes: parse_attributes(attributes),
      context_name: Helpers.capitalize(context_name),
      templates: nil,
      guides: nil
    }

    templates = get_template_strings(cmd_switches, :resource_template, assigns)
    guides = get_template_strings(cmd_switches, :guide_template, assigns)

    file_content =
      EEx.eval_file(@template_path,
        assigns: %{assigns | templates: templates, guides: guides}
      )
      |> Code.format_string!()
      |> IO.iodata_to_binary()

    {file_content, cmd_switches, resource_name, context_name}
  end

  @spec gen_module_name(atom()) :: atom()
  def gen_module_name(name) do
    ("Elixir.Ash" <>
       (name |> Atom.to_string() |> Helpers.capitalize()) <>
       ".Templates")
    |> String.to_existing_atom()
  end

  @spec get_template_strings(list, atom, map()) :: map
  def get_template_strings(cmd_switches, fname, assigns) do
    cmd_switches
    |> Enum.filter(fn {_name, val} -> val == true end)
    |> Enum.map(fn {name, _} ->
      {name,
       if name in @require_package do
         try do
           apply(gen_module_name(name), fname, [assigns])
         rescue
           _ -> apply(Mix.Tasks.Ash.Templates, fname, [name, assigns])
         end
       else
         true
       end}
    end)
    |> Map.new()
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
end
