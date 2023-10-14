defmodule Mix.Tasks.Ash.Gen.Resource do
  use Mix.Task

  @shortdoc "Generates an Ash resource and add it to the API."

  @moduledoc """
  This task generates a definition of an Ash resource and adds it to the internal API.

  ## Arguments

  * `api_name` - The API (e.g. "Shop").
  * `resource_name` - The resource (e.g. "Product").
  * `table_name` - The table name (e.g. "products").
  * `attributes` - The list of attributes (e.g. "name:string" or "name").

  If the attribute type is not provided, it defaults to `string`.

  ## Switches

  * `--no-code-interface` - Disables the generation of the `code_interface` block.

  ## Data Layer

  When `:ash_postgres` is in the dependencies, the used data layer is
  `AshPostgres.DataLayer` otherwise it defaults to `Ash.DataLayer.Ets`.

  ## Example

  mix ash.gen.resource Shop Product products name:string price:integer --no-code-interface
  """

  def run(args) do
    args
    |> parse_args()
    |> case do
      {:ok, api_name, resource_name, table_name, attributes, no_code_interface?} ->
        generate_files(api_name, resource_name, table_name, attributes, no_code_interface?)

      :error ->
        display_documentation()
    end
  end

  defp parse_args(args) do
    {opts, positional_args, _} = OptionParser.parse(args, switches: [no_code_interface: :boolean])

    case positional_args do
      [api_name, resource_name, table_name | attribute_args] ->
        attributes = parse_attributes(attribute_args)
        {:ok, api_name, resource_name, table_name, attributes, opts[:no_code_interface] || false}

      _ ->
        :error
    end
  end

  defp parse_attributes(attribute_args) do
    Enum.map(attribute_args, fn attr ->
      parts = String.split(attr, ":")
      parse_attribute(parts)
    end)
  end

  defp parse_attribute([name]) do
    {String.to_atom(name), "string"}
  end

  defp parse_attribute([name, type]) do
    {String.to_atom(name), type}
  end

  defp display_documentation do
    Mix.shell().info("""
    #{@shortdoc}

    #{@moduledoc}
    """)
  end

  defp generate_files(api_name, resource_name, table_name, attributes, no_code_interface?) do
    app_name = app_name()
    camelized_api_name = Macro.camelize(api_name)
    api_module_name = "#{app_name}.#{camelized_api_name}"
    camelized_resource_name = Macro.camelize(resource_name)
    resource_module_name = "#{api_module_name}.#{camelized_resource_name}"

    attribute_definitions =
      Enum.map_join(attributes, "\n    ", fn {name, type} ->
        "attribute :#{name}, :#{type}"
      end)

    code_interface_content = """
    code_interface do
      define_for #{api_module_name}
      define :create
      define :read
      define :by_id, get_by: [:id], action: :read
      define :update
      define :destroy
    end
    """

    data_layer_content =
      if uses_ash_postgres?() do
        """
        use Ash.Resource,
          data_layer: AshPostgres.DataLayer

        postgres do
          table "#{table_name}"
          repo #{app_name}.Repo
        end
        """
      else
        """
        use Ash.Resource, data_layer: Ash.DataLayer.Ets
        """
      end

    resource_file_content = """
    defmodule #{resource_module_name} do
      #{data_layer_content}

      attributes do
        uuid_primary_key :id
        #{attribute_definitions}
      end

      actions do
        defaults [:create, :read, :update, :destroy]
      end

      #{unless no_code_interface?, do: code_interface_content}
    end
    """

    api_dir_path = "lib/#{String.replace(api_module_name, ".", "/") |> String.downcase()}"
    resource_dir_path = "#{api_dir_path}/resources"

    # Ensure directories exist
    File.mkdir_p!(api_dir_path)
    File.mkdir_p!(resource_dir_path)

    api_file_path = "#{api_dir_path}.ex"
    resource_file_path = "#{resource_dir_path}/#{String.downcase(resource_name)}.ex"

    if File.exists?(api_file_path) do
      content = File.read!(api_file_path)

      unless String.contains?(content, "resource #{resource_module_name}") do
        # Append the new resource to the existing API file
        parts = String.split(content, "\n")
        index = Enum.find_index(parts, fn line -> String.trim(line) == "end" end)

        if index do
          parts = List.insert_at(parts, index, "    resource #{resource_module_name}")
          File.write!(api_file_path, Enum.join(parts, "\n"))
          IO.puts("Modified #{api_file_path}")
        end
      end
    else
      File.write!(api_file_path, """
      defmodule #{api_module_name} do
        use Ash.Api

        resources do
          resource #{resource_module_name}
        end
      end
      """)

      IO.puts("Generated #{api_file_path}")
    end

    # Check if the API is listed in the configuration of the current application
    unless api_listed?(api_module_name) do
      IO.puts("""
      \e[31m
      WARNING:
      #{api_module_name} is not yet listed in the application's configuration.\e[0m
      Make sure to add it to the configuration. Here is an example:

      ** config/config.exs **
      import Config

      config :#{Macro.underscore(app_name)}, :ash_apis, [#{api_module_name}]

      """)
    end

    if File.exists?(resource_file_path) do
      IO.puts("File #{resource_file_path} already exists. Replace? [Yn]")
      response = IO.gets("> ") |> String.trim()

      if String.downcase(response) not in ["y", "yes", ""] do
        IO.puts("Aborted!")
        exit(:normal)
      end
    end

    File.write!(resource_file_path, resource_file_content)
    IO.puts("Generated #{resource_file_path}")

    if uses_ash_postgres?() do
      IO.puts("\nYou may need to run 'mix ash.codegen' and 'mix ash_postgres.migrate'.")
    end

    # Format the generated resource
    Mix.Tasks.Format.run([api_file_path, resource_file_path])
  end

  defp app_name do
    app_name_atom = Mix.Project.config()[:app]
    Macro.camelize(Atom.to_string(app_name_atom))
  end

  defp uses_ash_postgres? do
    Code.ensure_loaded?(AshPostgres)
  end

  defp api_listed?(api_module_name) do
    Application.get_all_env(Mix.Project.config()[:app])
    |> Enum.filter(fn {key, _value} -> key == :ash_apis end)
    |> case do
      [{:ash_apis, api_list}] -> api_list
      _ -> []
    end
    |> Enum.member?(Module.concat([api_module_name]))
  end
end
