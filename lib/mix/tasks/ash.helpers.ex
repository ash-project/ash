defmodule Mix.Tasks.Ash.Helpers do
  def write_resource_file(file_content, file_name, context \\ nil) do
    if context do
      context |> context_folder() |> maybe_create_folder()
      context |> resources_folder() |> maybe_create_folder()
    else
      context |> resources_folder() |> maybe_create_folder()
    end

    File.write!(resource_file_name(file_name, context), file_content)
  end

  def write_api_file(file_content, file_name, has_context) do
    file_name |> context_folder() |> maybe_create_folder()

    File.write!(api_file_name(file_name, has_context), file_content)
  end

  def maybe_create_folder(folder) do
    if not File.exists?(folder) do
      File.mkdir!(folder)
    end
  end

  def capitalize(""), do: ""

  def capitalize(name) do
    with <<first::utf8, rest::binary>> <- name, do: String.upcase(<<first::utf8>>) <> rest
  end

  def project_module_name() do
    String.split(app_name(), "_") |> Enum.map(&capitalize/1) |> Enum.join()
  end

  def app_name() do
    Mix.Project.config()[:app] |> Atom.to_string()
  end

  def lib_folder(), do: "lib/" <> app_name()
  def api_file_name(context, has_context \\ false)
  def api_file_name(context, true), do: "#{context_folder(context)}/#{context}.ex"
  def api_file_name(context, false), do: "#{lib_folder()}/#{context}.ex"
  def resources_folder(context \\ nil)
  def resources_folder(nil), do: "#{lib_folder()}/resources"
  def resources_folder(context), do: "#{context_folder(context)}/resources"
  def context_folder("api"), do: "#{lib_folder()}"
  def context_folder(context), do: "#{lib_folder()}/#{context}"
  def resource_file_name(name, context \\ nil)
  def resource_file_name(name, nil), do: "#{resources_folder()}/#{name}.ex"
  def resource_file_name(name, context), do: "#{resources_folder(context)}/#{name}.ex"
end
