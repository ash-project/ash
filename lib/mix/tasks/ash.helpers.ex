defmodule Mix.Tasks.Ash.Helpers do
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
  def api_file_name(context, is_context \\ false)
  def api_file_name(context, true), do: "#{context_folder(context)}/#{context}.ex"
  def api_file_name(context, false), do: "#{lib_folder()}/#{context}.ex"
  def resources_folder(context, is_context \\ false)
  def resources_folder(context, true), do: "#{context_folder(context)}/resources"
  def resources_folder(_context, false), do: "#{lib_folder()}/resources"
  def resources_folder(), do: resources_folder(nil)
  def context_folder("api"), do: "#{lib_folder()}"
  def context_folder(context), do: "#{lib_folder()}/#{context}"
  def resource_file_name(name, is_context \\ false)
  def resource_file_name(name, true), do: "#{resources_folder(name, true)}/#{name}.ex"
  def resource_file_name(name, false), do: "#{resources_folder()}/#{name}.ex"
end
