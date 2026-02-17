# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Sourceror) do
  defmodule Mix.Tasks.Spark.Formatter do
    @shortdoc "Manages a variable called `spark_locals_without_parens` in the .formatter.exs from a list of DSL extensions."
    @moduledoc @shortdoc
    use Mix.Task

    @spec run(term) :: no_return
    def run(opts) do
      # Compile if safe to do so (not at umbrella root)
      if not Mix.Project.umbrella?() do
        Mix.Task.run("compile")
      end

      {opts, []} = OptionParser.parse!(opts, strict: [check: :boolean, extensions: :string])

      unless opts[:extensions] do
        raise "Must supply a comma separated list of extensions to generate a .formatter.exs for"
      end

      extensions =
        opts[:extensions]
        |> String.split(",")
        |> Enum.map(&Module.concat([&1]))

      locals_without_parens =
        Enum.flat_map(extensions, fn extension_mod ->
          case Code.ensure_compiled(extension_mod) do
            {:module, _module} ->
              :ok

            other ->
              error_msg =
                if Mix.Project.umbrella?() do
                  """
                  Error ensuring extension compiled: #{inspect(other)}

                  You are running this task from an umbrella project root.
                  Try running this task from within a sub-app directory, or compile the project first:
                    cd apps/<your_app> && mix spark.formatter --extensions #{opts[:extensions]}
                  """
                else
                  "Error ensuring extension compiled #{inspect(other)}"
                end

              raise error_msg
          end

          all_entity_builders_everywhere(
            extension_mod.sections(),
            extension_mod.dsl_patches(),
            extensions
          )
        end)
        |> Enum.uniq()
        |> Enum.sort()

      contents = File.read!(".formatter.exs")

      {_ast, spark_locals_without_parens} =
        contents
        |> Sourceror.parse_string!()
        |> Macro.prewalk(
          nil,
          fn
            {:=, _,
             [
               {:spark_locals_without_parens, _, _},
               right
             ]} = ast,
            _acc ->
              {ast, right}

            ast, acc ->
              {ast, acc}
          end
        )

      if !spark_locals_without_parens do
        raise "Add `spark_locals_without_parens = []` to your .formatter.exs and run this again to populate the list."
      end

      new_contents =
        contents
        |> Sourceror.patch_string([
          Sourceror.Patch.new(
            Sourceror.get_range(spark_locals_without_parens, include_comments: true),
            Sourceror.to_string(locals_without_parens, opts)
          )
        ])
        |> Code.format_string!()

      contents_with_newline = [new_contents, "\n"]

      if opts[:check] do
        if contents != IO.iodata_to_binary(contents_with_newline) do
          raise """
          .formatter.exs is not up to date!

          Run the following command and commit the result:

          mix spark.formatter --extensions #{opts[:extensions]}
          """
        else
          IO.puts("The current .formatter.exs is correct")
        end
      else
        File.write!(".formatter.exs", contents_with_newline)
      end
    end

    def all_entity_builders_everywhere(sections, dsl_patches, extensions, path \\ []) do
      patch_builders =
        dsl_patches
        |> Enum.filter(fn
          %Spark.Dsl.Patch.AddEntity{} ->
            true

          _ ->
            false
        end)
        |> Enum.map(& &1.entity)
        |> Enum.flat_map(fn entity ->
          Enum.concat([
            Spark.Formatter.entity_option_builders(entity),
            Spark.Formatter.entity_builders(entity)
          ])
        end)

      sections
      |> Enum.flat_map(fn section ->
        all_entity_builders_everywhere(
          section.sections,
          [],
          extensions,
          path ++ [section.name]
        )
      end)
      |> Enum.concat(Spark.Formatter.all_entity_builders(sections, extensions, path))
      |> Enum.concat(patch_builders)
    end
  end
else
  defmodule Mix.Tasks.Spark.Formatter do
    @shortdoc "Manages a variable called `spark_locals_without_parens` in the .formatter.exs from a list of DSL extensions."
    @moduledoc @shortdoc
    use Mix.Task

    def run(_opts) do
      raise "This task requires sourceror to run. Please add it as a dev/test dependency"
    end
  end
end
