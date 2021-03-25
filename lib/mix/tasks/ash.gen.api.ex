defmodule Mix.Tasks.Ash.Gen.Api do
  alias Mix.Tasks.Ash.Helpers
  @template_path Path.join(:code.priv_dir(:ash), "api.ex.eex")


  @shortdoc """
  Initializes new api file lib/api.ex with default content
  """
  @doc """
  Initializes new api file lib/api.ex with default content

  run as mix ash.gen api 
  with optional context name
  """

  def run([]), do: run(["api"])
  def run([file_name | _]) do
    has_context = file_name != "api"

    initial_data =
      EEx.eval_file(@template_path,
        module_name: Helpers.capitalize(file_name),
        project_module_name: Helpers.project_module_name(),
        has_context: has_context
      )


    IO.inspect(Helpers.api_file_name(file_name, has_context))
    IO.inspect(initial_data)
    if not File.exists?(Helpers.context_folder(file_name)) do
      File.mkdir!(Helpers.context_folder(file_name))
    end
    File.write!(Helpers.api_file_name(file_name, has_context), initial_data)
  end
end

