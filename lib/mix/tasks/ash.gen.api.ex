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
    file_content = generate_file(file_name, has_context)

    Helpers.write_file(file_name, file_content, has_context)
  end

  def generate_file(file_name, has_context) do
    EEx.eval_file(@template_path,
      module_name: Helpers.capitalize(file_name),
      project_module_name: Helpers.project_module_name(),
      has_context: has_context
    )
  end
end
