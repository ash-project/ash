# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Docs do
  @moduledoc """
  Tools for generating docs & search data for extras.
  """

  @doc """
  Builds a redirects map for a DSL from mod/funs to their respective DSL docs.

  This is useful for redirecting links to private DSL modules (like `Ash.Resource.Dsl.Actions.Create`)
  to their corresponding documentation pages (like `dsl-ash-resource#actions-create`).

  ## Example

      redirects_for([Ash.Resource.Dsl])
      # => %{
      #   "Ash.Resource.Dsl.Actions.Create" => "dsl-ash-resource#actions-create",
      #   ...
      # }

  These redirects can be passed to ex_doc's `:redirects` option to handle links
  to private DSL modules.
  """
  def redirects_for(dsls, existing_redirects \\ %{}) do
    dsls
    |> Enum.reduce(existing_redirects, fn dsl, acc ->
      module_prefix = dsl.module_prefix()
      extension_name = extension_name(dsl)
      doc_filename = dsl_doc_filename(extension_name)

      acc
      |> add_section_redirects(dsl.sections(), module_prefix, doc_filename, [])
      |> add_patch_redirects(dsl.dsl_patches(), module_prefix, doc_filename)
    end)
  end

  defp extension_name(dsl) do
    dsl
    |> inspect()
    |> String.trim_trailing(".Dsl")
  end

  defp dsl_doc_filename(extension_name) do
    name =
      extension_name
      |> String.split(".")
      |> Enum.map_join("-", &String.downcase/1)

    "dsl-#{name}"
  end

  defp add_section_redirects(redirects, sections, module_prefix, doc_filename, path) do
    Enum.reduce(sections, redirects, fn section, acc ->
      section_path = path ++ [section.name]

      acc
      |> add_entity_redirects(section.entities, module_prefix, doc_filename, section_path, [])
      |> add_section_redirects(section.sections, module_prefix, doc_filename, section_path)
    end)
  end

  defp add_entity_redirects(redirects, entities, module_prefix, doc_filename, section_path, nested_entity_path) do
    Enum.reduce(entities, redirects, fn entity, acc ->
      entity_module = Spark.Dsl.Extension.entity_mod_name(
        module_prefix,
        nested_entity_path,
        section_path,
        entity
      )

      anchor = anchor(section_path ++ nested_entity_path ++ [entity.name])
      redirect_target = "#{doc_filename}##{anchor}"

      acc
      |> Map.put(inspect(entity_module), redirect_target)
      |> add_nested_entity_redirects(
        entity.entities,
        module_prefix,
        doc_filename,
        section_path,
        nested_entity_path ++ [entity.name]
      )
    end)
  end

  defp add_nested_entity_redirects(redirects, nested_entities, module_prefix, doc_filename, section_path, nested_entity_path) do
    nested_entities
    |> Enum.flat_map(fn {_key, entities} -> List.wrap(entities) end)
    |> Enum.reduce(redirects, fn nested_entity, acc ->
      add_entity_redirects(
        acc,
        [nested_entity],
        module_prefix,
        doc_filename,
        section_path,
        nested_entity_path
      )
    end)
  end

  defp add_patch_redirects(redirects, patches, module_prefix, doc_filename) do
    Enum.reduce(patches, redirects, fn
      %Spark.Dsl.Patch.AddEntity{section_path: section_path, entity: entity}, acc ->
        add_entity_redirects(acc, [entity], module_prefix, doc_filename, section_path, [])
    end)
  end

  @doc """
  Generates searchable documentation suitable for ex_doc
  """
  def search_data_for(dsl) do
    dsl.sections()
    |> Enum.flat_map(fn section ->
      section_search_data(section)
    end)
    |> Enum.concat(
      Enum.flat_map(dsl.dsl_patches(), fn %Spark.Dsl.Patch.AddEntity{
                                            section_path: section_path,
                                            entity: entity
                                          } ->
        entity_search_data(entity, section_path)
      end)
    )
  end

  defp section_search_data(section, path \\ []) do
    schema_path =
      if section.top_level? do
        path
      else
        path ++ [section.name]
      end

    Enum.concat([
      [
        %{
          anchor: anchor(schema_path),
          body: section.describe,
          title: title(schema_path),
          type: "DSL"
        }
      ],
      schema_search_data(section.schema, schema_path),
      Enum.flat_map(section.entities, &entity_search_data(&1, schema_path)),
      Enum.flat_map(section.sections, &section_search_data(&1, schema_path))
    ])
    |> Enum.map(fn search_item ->
      if section.top_level? do
        %{search_item | anchor: "#{section.name}-#{search_item.anchor}"}
      else
        search_item
      end
    end)
  end

  defp entity_search_data(entity, path) do
    Enum.concat([
      [
        %{
          anchor: anchor(path ++ [entity.name]),
          body: entity.describe,
          title: title(path ++ [entity.name]),
          type: "DSL"
        }
      ],
      schema_search_data(entity.schema, path ++ [entity.name]),
      Enum.flat_map(entity.entities, fn {_key, entities} ->
        Enum.flat_map(entities, fn nested_entity ->
          entity_search_data(nested_entity, path ++ [entity.name])
        end)
      end)
    ])
  end

  defp schema_search_data(schema, path) do
    Enum.flat_map(schema, fn {key, config} ->
      if config[:hide] do
        []
      else
        [
          %{
            anchor: anchor(path ++ [key]),
            body: config[:doc] || "",
            title: title(path ++ [key]),
            type: "DSL"
          }
        ]
      end
    end)
  end

  defp anchor(list), do: Enum.join(list, "-")
  defp title(list), do: Enum.join(list, ".")
end
