defmodule Mix.Tasks.Ash.Gen.Resource do
  use Mix.Task

  @shortdoc "Generates an Ash resource and related API files."

  @moduledoc """
  Generates an Ash resource and updates or creates the corresponding API files.

  ## Arguments

  - API Name:      The name of the internal API (e.g. "Shop").
  - Resource Name: The name of the resource (e.g. "Product").
  - Table Name:    The name of the database table (e.g. "products").
  - Attributes:    A list of attributes in the format name:type (e.g. "name:string").

  ## Example

  mix ash.gen.resource Shop Product products name:string price:integer
  """

  def run([]) do
    Mix.shell().info("""
    #{Mix.Task.shortdoc(__MODULE__)}

    #{Mix.Task.moduledoc(__MODULE__)}
    """)
  end

  def run([api_name, resource_name, table_name | attribute_args]) do
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

    generate_files(api_name, resource_name, table_name, attributes)
  end

  defp generate_files(api_name, resource_name, table_name, attributes) do
    app_name = app_name()

    api_module_name = "#{app_name}.#{api_name}"
    resource_module_name = "#{app_name}.#{api_name}.#{resource_name}"

    api_file_content = """
    defmodule #{api_module_name} do
      use Ash.Api

      resources do
        resource #{resource_module_name}
      end
    end
    """

    attribute_definitions =
      Enum.map(attributes, fn {key, type} ->
        "attribute :#{key}, :#{type}"
      end)
      |> Enum.join("\n    ")

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

      code_interface do
        define_for #{api_module_name}
        define :create
        define :read
        define :by_id, get_by: [:id], action: :read
        define :update
        define :destroy
      end
    end
    """

    api_dir_path = "lib/#{String.replace(api_module_name, ".", "/") |> String.downcase()}"
    resource_dir_path = "#{api_dir_path}/resources"

    # Ensure directories exist
    File.mkdir_p!(api_dir_path)
    File.mkdir_p!(resource_dir_path)

    api_file_path = "#{api_dir_path}.ex"
    resource_file_path = "#{resource_dir_path}/#{String.downcase(resource_name)}.ex"

    # Check and update the API file
    if File.exists?(api_file_path) do
      current_content = File.read!(api_file_path)

      unless String.contains?(current_content, "resource #{resource_module_name}") do
        updated_content =
          String.replace(
            current_content,
            "resources do",
            "resources do\n    resource #{resource_module_name}"
          )

        File.write!(api_file_path, updated_content)
      end
    else
      File.write!(api_file_path, api_file_content)
    end

    # Check and decide on the Resource file
    if File.exists?(resource_file_path) do
      IO.puts("Resource file #{resource_file_path} already exists.")
      action = IO.gets("Do you want to replace it? (y/n): ")

      case String.trim(action) do
        "y" ->
          File.write!(resource_file_path, resource_file_content)

        _ ->
          IO.puts("Aborted.")
      end
    else
      File.write!(resource_file_path, resource_file_content)
    end
  end

  defp app_name do
    app_name_atom = Mix.Project.config()[:app]
    Macro.camelize(Atom.to_string(app_name_atom))
  end
end
