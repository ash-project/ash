defmodule Mix.Tasks.Ash.Gen.Resource do
  use Mix.Task

  @shortdoc "Generates an Ash API and resource file."

  @moduledoc """
  This task generates an internal API and a definition of a resource for Ash.

  ## Arguments

  * `api_name` - The API (e.g. "Shop").
  * `resource_name` - The resource (e.g. "Product").
  * `table_name` - The table name (e.g. "products").
  * `attributes` - The list of attributes (e.g. "name:string" or "name").

  If the attribute type is not provided, it defaults to `string`.

  ## Switches

  * `--no-code-interface` - Disables the generation of the `code_interface` block.

  ## Example

  mix ash.gen.resource Shop Product products name:string price:integer --no-code-interface
  """

  def run(args) do
    {opts, positional_args, _} =
      OptionParser.parse(args,
        switches: [no_code_interface: :boolean]
      )

    case positional_args do
      [api_name, resource_name, table_name | attribute_args] ->
        no_code_interface? = opts[:no_code_interface] || false

        attributes =
          for attr <- attribute_args do
            parts = String.split(attr, ":")

            case parts do
              [name] ->
                {String.to_atom(name), "string"}

              [name, type] ->
                {String.to_atom(name), type}
            end
          end

        generate_files(api_name, resource_name, table_name, attributes, no_code_interface?)

      _ ->
        Mix.shell().info("""
        #{@shortdoc}

        #{@moduledoc}
        """)
    end
  end

  defp generate_files(api_name, resource_name, table_name, attributes, no_code_interface?) do
    app_name = app_name()
    api_module_name = "#{app_name}.#{api_name}"
    resource_module_name = "#{api_module_name}.#{resource_name}"

    attribute_definitions =
      Enum.map(attributes, fn {name, type} ->
        "attribute :#{name}, :#{type}"
      end)
      |> Enum.join("\n    ")

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

    resource_file_content = """
    defmodule #{resource_module_name} do
      use Ash.Resource,
        data_layer: AshPostgres.DataLayer

      postgres do
        table "#{table_name}"
        repo #{app_name}.Repo
      end

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
  end

  defp app_name do
    app_name_atom = Mix.Project.config()[:app]
    Macro.camelize(Atom.to_string(app_name_atom))
  end
end
