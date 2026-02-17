# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Sourceror) do
  defmodule Spark.Formatter do
    @moduledoc """
    Formats Spark modules.

    Currently, it is very simple, and will only reorder the outermost sections according to some rules.

    # Plugin

    Include the plugin into your `.formatter.exs` like so `plugins: [Spark.Formatter]`.

    If no configuration is provided, it will sort all top level DSL sections *alphabetically*.

    # Section Order

    To provide a custom section order, add configuration to your app, for example:

    ```elixir
    config :spark, :formatter,
      remove_parens?: true,
      "Ash.Resource": [
        section_order: [
          :resource,
          :postgres,
          :attributes,
          :relationships,
          :aggregates,
          :calculations
        ]
      ],
      "MyApp.Resource": [
        # Use this if you use a module that is not the spark DSL itself.
        # For example, you might have a "base" that you use instead that sets some simple defaults.

        # This tells us what the actual thing is so we know what extensions are included automatically.
        type: Ash.Resource,

        # Tell us what extensions might be added under the hood
        extensions: [MyApp.ResourceExtension],
        section_order: [...]
      ]
    ```

    Any sections found that aren't in that list will be left in the order that they were in, the sections
    in the list will be sorted "around" those sections. E.g the following list: `[:code_interface, :attributes]` can be interpreted as
    "ensure that code_interface comes before attributes, and don't change the rest".
    """
    @behaviour Mix.Tasks.Format

    require Logger

    def features(_opts) do
      [extensions: [".ex", ".exs"]]
    end

    def format(contents, opts) do
      config =
        :spark
        |> Application.get_env(:formatter, [])
        |> Enum.map(fn
          {key, value} when key in [:remove_parens?] ->
            {key, value}

          {key, value} ->
            {Module.concat([key]), value}
        end)

      parse_result =
        try do
          {:ok, Sourceror.parse_string!(contents)}
        rescue
          _ ->
            :error
        end

      case parse_result do
        {:ok, parsed} ->
          parsed
          |> format_resources(opts, config)
          |> then(fn patches ->
            Sourceror.patch_string(contents, patches)
          end)
          |> Code.format_string!(opts_without_plugin(opts))
          |> then(fn iodata ->
            [iodata, ?\n]
          end)
          |> IO.iodata_to_binary()

        :error ->
          contents
      end
    end

    defp format_resources(parsed, opts, config) do
      {_, patches} =
        Spark.CodeHelpers.prewalk(parsed, [], false, fn
          {:defmodule, _, [_, [{{:__block__, _, [:do]}, {:__block__, _, body}}]]} = expr,
          patches,
          false ->
            case get_extensions(body, config) do
              {:ok, extensions, type, using} ->
                replacement = format_resource(body, extensions, config, type, using)

                patches =
                  body
                  |> Enum.zip(replacement)
                  |> Enum.reduce(patches, fn {body_section, replacement_section}, patches ->
                    if body_section == replacement_section do
                      patches
                    else
                      [
                        %{
                          range: Sourceror.get_range(body_section, include_comments: true),
                          change: Sourceror.to_string(replacement_section, opts)
                        }
                        | patches
                      ]
                    end
                  end)

                {expr, patches, true}

              _ ->
                {expr, patches, true}
            end

          expr, patches, branch_acc ->
            {expr, patches, branch_acc}
        end)

      patches
    end

    defp format_resource(body, extensions, config, _type, using) do
      sections =
        extensions
        |> Enum.flat_map(fn extension ->
          Enum.map(extension.sections(), fn section ->
            {extension, section}
          end)
        end)
        |> sort_sections(config[using][:section_order])

      section_names = Enum.map(sections, fn {_, section} -> section.name end)

      {section_exprs, non_section_exprs} =
        body
        |> Enum.with_index()
        |> Enum.split_with(fn {{name, _, _}, _index} ->
          name in section_names
        end)

      new_sections =
        if config[using][:section_order] && config[using][:section_order] != [] do
          Enum.sort_by(section_exprs, fn {{name, _, _}, _} ->
            Enum.find_index(section_names, &(&1 == name))
          end)
        else
          section_exprs
        end

      new_section_indexes =
        section_exprs
        |> Enum.map(&elem(&1, 1))
        |> Enum.sort()

      new_sections =
        Enum.zip_with(new_sections, new_section_indexes, fn {new_section, _}, index ->
          {new_section, index}
        end)

      non_section_exprs
      |> Enum.concat(new_sections)
      |> Enum.sort_by(&elem(&1, 1))
      |> Enum.map(&elem(&1, 0))
      |> then(fn sections ->
        if config[:remove_parens?] do
          de_paren(sections, Enum.flat_map(extensions, & &1.sections()), extensions)
        else
          sections
        end
      end)
    end

    defp de_paren(actual_sections, dsl_sections, extensions) do
      actual_sections
      |> Enum.map(fn
        {name, meta, body} ->
          case Enum.find(dsl_sections, &(&1.name == name)) do
            nil ->
              {name, meta, body}

            section ->
              {name, meta, de_paren_section(body, section, extensions)}
          end

        other ->
          other
      end)
    end

    defp de_paren_section(body, section, extensions) do
      builders = all_entity_builders([section], extensions)

      Macro.prewalk(body, fn
        {func, meta, body} = node when is_atom(func) ->
          count = Enum.count(List.wrap(body))

          builders = Keyword.get_values(builders, func)

          if Enum.any?(builders, &(&1 in [count, count - 1])) &&
               Keyword.keyword?(meta) &&
               meta[:closing] do
            {func, Keyword.delete(meta, :closing), body}
          else
            node
          end

        node ->
          node
      end)
    end

    defp sort_sections(sections, nil), do: sections
    defp sort_sections(sections, []), do: sections

    defp sort_sections(sections, section_order) do
      {ordered, unordered} =
        sections
        |> Enum.with_index()
        |> Enum.split_with(fn {{_, section}, _} ->
          section.name in section_order
        end)

      reordered =
        ordered
        |> Enum.map(&elem(&1, 0))
        |> Enum.sort_by(fn {_, section} ->
          Enum.find_index(section_order, &(&1 == section.name))
        end)

      Enum.reduce(unordered, reordered, fn {{extension, section}, i}, acc ->
        List.insert_at(acc, i, {extension, section})
      end)
    end

    defp get_extensions(body, config) do
      Enum.find_value(body, :error, fn
        {:use, _, using} ->
          [using, opts] =
            case Spark.Dsl.Extension.expand_alias(using, __ENV__) do
              [using] ->
                [using, []]

              [using, opts] ->
                [using, opts]
            end

          if Keyword.has_key?(config, using) do
            type = config[using][:type] || using
            {:ok, parse_extensions(opts, config[using], type), type, using}
          end

        _ ->
          nil
      end)
    end

    defp parse_extensions(blocks, config, type) do
      blocks
      |> Enum.flat_map(fn {{:__block__, _, _}, extensions} ->
        extensions
        |> case do
          {:__block__, _, [extensions]} ->
            extensions

          extension when is_atom(extension) ->
            extension

          _ ->
            []
        end
        |> List.wrap()
        |> Enum.flat_map(fn extension ->
          case is_atom(extension) and Code.ensure_compiled(extension) do
            {:module, module} ->
              if Spark.implements_behaviour?(module, Spark.Dsl.Extension) do
                [module]
              else
                []
              end

            _ ->
              []
          end
        end)
      end)
      |> Enum.concat(config[:extensions] || [])
      |> Enum.concat(safe_get_default_extensions(type))
      |> Enum.flat_map(fn extension ->
        try do
          [extension | extension.add_extensions()]
        rescue
          _ -> [extension]
        end
      end)
    end

    defp safe_get_default_extensions(type, retry? \\ false) do
      type.default_extensions() || []
    rescue
      error ->
        if Mix.Project.umbrella?() || retry? do
          Logger.warning("""
          Spark.Formatter: Could not load default_extensions for #{inspect(type)}.
          This can happen in umbrella projects when running format from the root.
          Try running format from within the sub-app, or compile first with: mix compile

          Error: #{inspect(error)}
          """)
        else
          Mix.Task.reenable("compile")
          Mix.Task.reenable("loadpaths")
          Mix.Task.run("compile")
          Mix.Task.run("loadpaths")
          safe_get_default_extensions(type, true)
        end

        []
    end

    defp opts_without_plugin(opts) do
      Keyword.update(opts, :plugins, [], &(&1 -- [__MODULE__]))
    end

    @doc false
    def all_entity_builders(sections, extensions, path \\ []) do
      Enum.flat_map(sections, fn section ->
        Enum.concat([
          all_entity_option_builders(section),
          all_patch_entity_builders(extensions, section, path),
          section_option_builders(section),
          section_entity_builders(section, extensions, path)
        ])
      end)
      |> Enum.uniq()
      |> Enum.sort()
    end

    defp all_patch_entity_builders(extensions, section, path) do
      match_path = path ++ [section.name]

      extensions
      |> Enum.flat_map(& &1.dsl_patches())
      |> tap(fn patches ->
        Enum.map(patches, & &1.section_path)
      end)
      |> Enum.filter(fn
        %Spark.Dsl.Patch.AddEntity{section_path: ^match_path} ->
          true

        _ ->
          false
      end)
      |> Enum.map(& &1.entity)
      |> Enum.flat_map(&entity_option_builders/1)
    end

    defp section_entity_builders(section, extensions, path) do
      match_path = path ++ [section.name]

      mixed_in_entities =
        extensions
        |> Enum.flat_map(& &1.dsl_patches())
        |> Enum.filter(fn
          %Spark.Dsl.Patch.AddEntity{section_path: ^match_path} ->
            true

          _ ->
            false
        end)
        |> Enum.map(& &1.entity)

      Enum.flat_map(section.entities ++ mixed_in_entities, fn entity ->
        entity_builders(entity)
      end) ++ all_entity_builders(section.sections, extensions, path ++ [section.name])
    end

    def entity_builders(entity) do
      arg_count = Enum.count(entity.args)
      non_optional_arg_count = Enum.count(entity.args, &is_atom/1)

      non_optional_arg_count..arg_count
      |> Enum.flat_map(&[{entity.name, &1}, {entity.name, &1 + 1}])
      |> Enum.concat(flat_map_nested_entities(entity, &entity_builders/1))
    end

    defp all_entity_option_builders(section) do
      Enum.flat_map(section.entities, fn entity ->
        entity_option_builders(entity)
      end)
    end

    @doc false
    def entity_option_builders(entity) do
      entity_args_to_drop = Spark.Dsl.Entity.required_arg_names(entity)

      entity.schema
      |> Keyword.drop(entity_args_to_drop)
      |> Enum.map(fn {key, _schema} ->
        {key, 1}
      end)
      |> Kernel.++(flat_map_nested_entities(entity, &entity_option_builders/1))
    end

    defp section_option_builders(section) do
      Enum.map(section.schema, fn {key, _} ->
        {key, 1}
      end)
    end

    defp flat_map_nested_entities(entity, mapper) do
      Enum.flat_map(entity.entities, fn {_, nested_entities} ->
        nested_entities
        |> List.wrap()
        |> Enum.flat_map(fn nested_entity ->
          mapper.(nested_entity)
        end)
      end)
    end
  end
else
  defmodule Spark.Formatter do
    @moduledoc """
    Formats Spark modules.
    """
    @behaviour Mix.Tasks.Format

    require Logger

    def features(_opts) do
      [extensions: [".ex", ".exs"]]
    end

    def format(_content, _opts) do
      raise """
      #{inspect(__MODULE__)} requires sourceror to run. Please add it as a dev/test dependency

        defp deps do
          [
            ...,
            {:sourceror, "~> 1.7", only: [:dev, :test]}
          ]

        end
      """
    end
  end
end
