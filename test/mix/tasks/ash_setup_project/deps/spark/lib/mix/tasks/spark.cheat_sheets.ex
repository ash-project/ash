# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Spark.CheatSheets.Docs do
  @moduledoc false

  def short_doc do
    "Creates cheat sheets for each Extension provided. Useful for CI with `--check` flag."
  end

  def example do
    "mix spark.cheat_sheets --extensions MyApp.Foo,MyApp.Bar"
  end

  def long_doc do
    """
    #{short_doc()}

    ## Example

    ```bash
    #{example()}
    ```

    ## Options

    * `--extensions` - The list of extensions to generate cheat sheets for
    """
  end
end

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Spark.CheatSheets do
    @shortdoc "#{__MODULE__.Docs.short_doc()}"

    @moduledoc __MODULE__.Docs.long_doc()

    use Igniter.Mix.Task

    @impl Igniter.Mix.Task
    def info(_argv, _composing_task) do
      %Igniter.Mix.Task.Info{
        group: :spark,
        schema: [extensions: :csv],
        required: [:extensions]
      }
    end

    def run(argv) do
      super(argv ++ ["--yes"])
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      igniter.args.options[:extensions]
      |> Enum.map(&Igniter.Project.Module.parse/1)
      |> Enum.uniq()
      |> Enum.reduce(igniter, fn extension, igniter ->
        cheat_sheet = Spark.CheatSheet.cheat_sheet(extension)

        extension_name = Spark.Mix.Helpers.extension_name(extension, [])

        filename = "documentation/dsls/DSL-#{extension_name}.md"

        Igniter.create_or_update_file(igniter, filename, cheat_sheet, fn source ->
          Rewrite.Source.update(source, :content, cheat_sheet)
        end)
      end)
    end
  end
else
  defmodule Mix.Tasks.Spark.CheatSheets do
    @shortdoc "#{__MODULE__.Docs.short_doc()} | Install `igniter` to use"

    @moduledoc __MODULE__.Docs.long_doc()

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'spark.cheat_sheets' requires igniter. Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter/readme.html#installation
      """)

      exit({:shutdown, 1})
    end
  end
end
