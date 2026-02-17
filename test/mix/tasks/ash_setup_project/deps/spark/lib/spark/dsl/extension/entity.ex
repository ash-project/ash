# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Extension.Entity do
  @moduledoc false

  def setup(module, recursive_as, nested_key, opts, arg_values, anno) do
    parent_recursive_as = Process.get(:parent_recursive_as)
    original_nested_entity_path = Process.get(:recursive_builder_path)

    nested_entity_path =
      if is_nil(original_nested_entity_path) do
        Process.put(:recursive_builder_path, [])
        []
      else
        unless recursive_as || nested_key || parent_recursive_as do
          raise "Somehow got a nested entity without a `recursive_as` or `nested_key`"
        end

        path =
          (original_nested_entity_path || []) ++
            [recursive_as || nested_key || parent_recursive_as]

        Process.put(
          :recursive_builder_path,
          path
        )

        path
      end

    if recursive_as do
      Process.put(:parent_recursive_as, recursive_as)
    end

    current_sections = Process.get({module, :spark_sections}, [])

    keyword_opts =
      Keyword.merge(
        opts,
        arg_values,
        fn key, _, _ ->
          raise Spark.Error.DslError,
            module: module,
            message: "Multiple values for key `#{inspect(key)}`",
            path: nested_entity_path,
            location: anno
        end
      )

    Process.put(
      {:builder_opts, nested_entity_path},
      keyword_opts
    )

    # Store annotation for entity options error reporting
    Process.put({:builder_anno, nested_entity_path}, anno)

    {original_nested_entity_path, parent_recursive_as, nested_entity_path, current_sections, anno}
  end

  def handle(
        module,
        section_path,
        nested_entity_keys,
        entity_builder,
        extension,
        {original_nested_entity_path, parent_recursive_as, nested_entity_path, current_sections,
         anno}
      ) do
    Process.put(:recursive_builder_path, original_nested_entity_path)
    Process.put(:parent_recursive_as, parent_recursive_as)

    current_config =
      Process.get(
        {module, :spark, section_path ++ nested_entity_path},
        Spark.Dsl.Extension.default_section_config()
      )

    opts = Process.delete({:builder_opts, nested_entity_path})
    opts_anno = Process.delete({:builder_opts_anno, nested_entity_path})
    Process.delete({:builder_anno, nested_entity_path})

    nested_entities =
      nested_entity_keys
      |> Enum.reduce(%{}, fn key, acc ->
        nested_path = section_path ++ nested_entity_path ++ [key]

        entities =
          {module, :spark, nested_path}
          |> Process.get(%{entities: []})
          |> Map.get(:entities, [])

        Process.delete({module, :spark, nested_path})

        Map.update(acc, key, entities, fn current_nested_entities ->
          (current_nested_entities || []) ++ entities
        end)
      end)

    built = entity_builder.__build__(module, opts, nested_entities, anno, opts_anno)

    new_config = %{current_config | entities: current_config.entities ++ [built]}

    unless {extension, section_path} in current_sections do
      Process.put({module, :spark_sections}, [
        {extension, section_path} | current_sections
      ])
    end

    Process.put(
      {module, :spark, section_path ++ nested_entity_path},
      new_config
    )
  end
end
