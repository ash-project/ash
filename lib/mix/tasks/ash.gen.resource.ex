defmodule Mix.Tasks.Ash.Gen.Resource do
  use Mix.Task
  alias Mix.Tasks.Ash.Helpers

  @template_path Path.join(:code.priv_dir(:ash), "resource.ex.eex")

  @impl Mix.Task
  @shortdoc """
  Initializes new resource in lib/resources/__.ex with default content
  """
  @doc """
  Initializes new resource in lib/resources/__.ex with default content

  run as mix ash.resource user first_name last_name password email --data_layer postgres
  """
  def run([]), do: IO.puts("Name of resource needs to be specified")

  def run(args) do
    {switches, [resource | attributes], _invalid} =
      OptionParser.parse(args,
        aliases: [d: :data_layer],
        strict: [debug: :boolean, data_layer: :string]
      )

    data_layer = Keyword.get(switches, :data_layer, "postgres")

    initial_data =
      EEx.eval_file(@template_path,
        module_name: Helpers.capitalize(resource),
        data_layer: data_layer,
        project_name: Helpers.project_module_name(),
        name: resource,
        attributes: attributes
      )

    IO.inspect(initial_data)
    #if not File.exists?(Helpers.resources_folder()), do: File.mkdir!(Helpers.resources_folder())
    #File.write!(Helpers.resource_file_name(resource), initial_data)
    data_layer_info(data_layer)
    resource_info(resource)
  end

  defp resource_info(resource) do
    IO.puts("""
    First, ensure you've added ash_postgres to your mix.exs file.
    Please add your resource to #{Helpers.api_file_name("api")}
    """)
  end

  defp data_layer_info("postgres") do
    IO.puts("""
    First, ensure you've added ash_postgres to your mix.exs file.

      {:ash_postgres, "~> x.y.z"}

    and run

    mix ash_postgres.generate_migrations
    """)
  end
end
