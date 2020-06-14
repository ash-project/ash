defmodule Mix.Tasks.Ash.Formatter do
  @moduledoc "Generates a .formatter.exs from a list of extensions, and writes it."
  use Mix.Task

  @formatter_exs_template """
    # THIS FILE IS AUTOGENERATED USING `mix ash.formatter`
    # DONT MODIFY IT BY HAND
    locals_without_parens = __replace_me__

    [
      inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
      locals_without_parens: locals_without_parens,
      export: [
        locals_without_parens: locals_without_parens
      ]
    ]

  """

  @shortdoc @moduledoc
  def run(opts) do
    {opts, []} = OptionParser.parse!(opts, strict: [check: :boolean, extensions: :string])

    unless opts[:extensions] do
      raise "Must supply a comma separated list of extensions to generate a .formatter.exs for"
    end

    locals_without_parens =
      opts[:extensions]
      |> String.split(",")
      |> Enum.flat_map(fn extension ->
        extension_mod = Module.concat([extension])

        all_entity_builders(extension_mod.sections())
      end)
      |> Enum.uniq()
      |> Enum.sort()

    contents =
      @formatter_exs_template
      |> String.replace("__replace_me__", inspect(locals_without_parens))
      |> Code.format_string!()

    # |> IO.puts()

    contents_with_newline = [contents, "\n"]

    if opts[:check] do
      if File.read!(".formatter.exs") != IO.iodata_to_binary(contents_with_newline) do
        raise """
        .formatter.exs is not up to date!

        Run the following command and commit the result:

        mix ash.formatter --extensions #{opts[:extensions]}
        """
      else
        IO.puts("The current .formatter.exs is correct")
      end
    else
      File.write!(".formatter.exs", contents_with_newline)
    end
  end

  defp all_entity_builders(sections) do
    Enum.flat_map(sections, fn section ->
      Enum.concat([
        entity_option_builders(section),
        section_option_builders(section),
        entity_builders(section)
      ])
    end)
  end

  defp entity_builders(section) do
    Enum.flat_map(section.entities, fn entity ->
      arg_count = Enum.count(entity.args)
      [{entity.name, arg_count}, {entity.name, arg_count + 1}]
    end) ++ all_entity_builders(section.sections())
  end

  defp entity_option_builders(section) do
    Enum.flat_map(section.entities, fn entity ->
      entity.schema
      |> Keyword.drop(entity.args)
      |> Enum.map(fn {key, _schema} ->
        {key, 1}
      end)
    end)
  end

  defp section_option_builders(section) do
    Enum.map(section.schema, fn {key, _} ->
      {key, 1}
    end)
  end
end
