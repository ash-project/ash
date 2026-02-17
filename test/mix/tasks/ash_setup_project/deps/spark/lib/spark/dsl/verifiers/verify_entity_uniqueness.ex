# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Verifiers.VerifyEntityUniqueness do
  @moduledoc """
  Verifies that each entity that has an identifier is unique at each path.
  """

  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier

  def verify(dsl_state) do
    module = Verifier.get_persisted(dsl_state, :module)

    dsl_state
    |> Verifier.get_persisted(:extensions)
    |> Enum.each(fn extension ->
      Enum.each(extension.sections(), fn section ->
        verify_entity_uniqueness(module, section, dsl_state)
      end)
    end)

    :ok
  end

  defp verify_entity_uniqueness(module, section, dsl_state, path \\ []) do
    section_path = path ++ [section.name]

    section.entities
    |> Enum.each(fn entity ->
      do_verify_entity_uniqueness(module, entity, section_path, dsl_state)
    end)

    Enum.each(section.sections, fn section ->
      verify_entity_uniqueness(module, section, dsl_state, section_path)
    end)

    section.entities
    |> Enum.each(fn entity ->
      entities_to_check = Verifier.get_entities(dsl_state, section_path)

      entity.entities
      |> Enum.flat_map(fn {key, nested_entities} ->
        Enum.map(nested_entities, &{key, &1})
      end)
      |> Enum.each(fn {key, nested_entity} ->
        verify_nested_entity_uniqueness(
          module,
          nested_entity,
          section_path,
          entities_to_check,
          [key]
        )
      end)
    end)
  end

  defp verify_nested_entity_uniqueness(
         module,
         nested_entity,
         section_path,
         entities_to_check,
         nested_entity_path
       ) do
    unique_entities_or_error(
      entities_to_check,
      nested_entity.identifier,
      module,
      section_path ++ nested_entity_path
    )

    entities_to_check
    |> Enum.each(fn entity_to_check ->
      nested_entity.entities
      |> Enum.flat_map(fn {key, nested_entities} ->
        Enum.map(nested_entities, &{key, &1})
      end)
      |> Enum.filter(fn {_, nested_entity} ->
        nested_entity.identifier
      end)
      |> Enum.each(fn {key, nested_entity} ->
        nested_entities_to_check =
          entity_to_check
          |> Map.get(key)
          |> List.wrap()

        verify_nested_entity_uniqueness(
          module,
          nested_entity,
          section_path,
          nested_entities_to_check,
          nested_entity_path ++ [key]
        )
      end)
    end)
  end

  defp do_verify_entity_uniqueness(module, entity, section_path, dsl_state) do
    dsl_state
    |> Verifier.get_entities(section_path)
    |> Enum.filter(&(&1.__struct__ == entity.target))
    |> unique_entities_or_error(entity.identifier, module, section_path)
  end

  defp unique_entities_or_error(_, nil, _, _), do: :ok

  defp unique_entities_or_error(entities_to_check, identifier, module, path) do
    entities_to_check
    |> Enum.group_by(&{get_identifier(&1, identifier), &1.__struct__})
    |> Enum.find(&match?({_, [_first, _second | _]}, &1))
    |> case do
      nil ->
        :ok

      {{identifier, target}, [duplicate_entity | _rest]} ->
        location = Spark.Dsl.Entity.anno(duplicate_entity)

        raise Spark.Error.DslError,
          module: module,
          path: path ++ [identifier],
          message: """
          Got duplicate #{inspect(target)}: #{identifier}
          """,
          location: location
    end
  end

  defp get_identifier(record, {:auto, _}), do: record.__identifier__
  defp get_identifier(record, identifier), do: Map.get(record, identifier)
end
