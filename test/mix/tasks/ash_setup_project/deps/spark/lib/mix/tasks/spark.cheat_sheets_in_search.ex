# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Jason) do
  defmodule Mix.Tasks.Spark.CheatSheetsInSearch do
    @shortdoc "Includes generated cheat sheets in the search bar"
    @moduledoc @shortdoc
    use Mix.Task

    def run(opts) do
      Spark.Warning.warn_deprecated(
        "mix spark.cheat_sheets_in_search task",
        """
        You should switch to using `Spark.Docs.search_data_for(dsl_module)` instead of this task.

        i.e

          {"documentation/dsls/DSL-Ash.Resource.md",
           search_data: Spark.Docs.search_data_for(Ash.Resource.Dsl)},
        """
      )

      Mix.Task.run("compile")

      {opts, _} =
        OptionParser.parse!(opts,
          switches: [strip_prefix: :string, check: :boolean, extensions: :string]
        )

      unless opts[:extensions] do
        raise "Must supply a comma separated list of extensions to generate a .formatter.exs for"
      end

      extensions =
        opts[:extensions]
        |> String.split(",")
        |> Enum.map(&Module.concat([&1]))
        |> Enum.uniq()

      with {:ok, search_data_file, search_data} <- search_data_file(),
           {:ok, sidebar_items_file, sidebar_items} <- sidebar_items_file() do
        {search_data, sidebar_items} =
          Enum.reduce(extensions, {search_data, sidebar_items}, fn extension, acc ->
            add_extension_to_search_data(extension, acc, opts)
          end)

        File.write!(search_data_file, "searchData=" <> Jason.encode!(search_data))
        File.write!(sidebar_items_file, "sidebarNodes=" <> Jason.encode!(sidebar_items))
      else
        {:error, error} -> raise error
      end
    end

    defp search_data_file do
      "doc/dist/search_data-*.js"
      |> Path.wildcard()
      |> Enum.at(0)
      |> case do
        nil ->
          {:error, "No search_data file found"}

        file ->
          case File.read!(file) do
            "searchData=" <> contents ->
              {:ok, file, Jason.decode!(contents)}

            _ ->
              {:error, "search data js file was malformed"}
          end
      end
    end

    defp sidebar_items_file do
      "doc/dist/sidebar_items-*.js"
      |> Path.wildcard()
      |> Enum.at(0)
      |> case do
        nil ->
          {:error, "No sidebar_items file found"}

        file ->
          case File.read!(file) do
            "sidebarNodes=" <> contents ->
              {:ok, file, Jason.decode!(contents)}

            _ ->
              {:error, "sidebar items js file was malformed"}
          end
      end
    end

    defp add_extension_to_search_data(extension, acc, opts) do
      extension_name = Spark.Mix.Helpers.extension_name(extension, opts)

      acc =
        Enum.reduce(extension.sections(), acc, fn section, acc ->
          add_section_to_search_data(extension_name, section, acc)
        end)

      Enum.reduce(
        extension.dsl_patches(),
        acc,
        fn %Spark.Dsl.Patch.AddEntity{
             section_path: section_path,
             entity: entity
           },
           acc ->
          add_entity_to_search_data(
            extension_name,
            entity,
            acc,
            section_path
          )
        end
      )
    end

    defp add_section_to_search_data(
           extension_name,
           section,
           {search_data, sidebar_items},
           path \\ []
         ) do
      search_data =
        add_search_item(
          search_data,
          %{
            "doc" => section.describe,
            "ref" =>
              "#{dsl_search_name(extension_name)}.html##{Enum.join(path ++ [section.name], "-")}",
            "title" => "#{extension_name}.#{Enum.join(path ++ [section.name], ".")}",
            "type" => "DSL"
          }
        )

      search_data =
        add_schema_to_search_data(
          search_data,
          extension_name,
          section.schema,
          path ++ [section.name]
        )

      # sidebar_items =
      #   add_schema_to_sidebar_items(
      #     sidebar_items,
      #     extension_name,
      #     to_string(Enum.at(path, 0) || section.name),
      #     section.schema,
      #     section.deprecations,
      #     path ++ [section.name]
      #   )

      acc =
        Enum.reduce(
          section.sections,
          {search_data, sidebar_items},
          &add_section_to_search_data(extension_name, &1, &2, path ++ [section.name])
        )

      Enum.reduce(
        section.entities,
        acc,
        &add_entity_to_search_data(extension_name, &1, &2, path ++ [section.name])
      )
    end

    defp add_entity_to_search_data(extension_name, entity, {search_data, sidebar_items}, path) do
      path = path ++ [entity.name]
      dot_path = Enum.join(path, ".")
      dash_path = Enum.join(path, "-")

      search_data =
        add_search_item(
          search_data,
          %{
            "doc" => entity.describe,
            "ref" => "#{dsl_search_name(extension_name)}.html##{dash_path}",
            "title" => "#{extension_name}.#{dot_path}",
            "type" => "DSL"
          }
        )

      # tail_path = Enum.join(tl(path), ".")

      # sidebar_items =
      #   add_sidebar_item(
      #     sidebar_items,
      #     extension_name,
      #     to_string(Enum.at(path, 0)),
      #     %{
      #       "anchor" => dash_path,
      #       "deprecated" => false,
      #       "label" => "DSL Entity",
      #       "id" => "#{dot_path}/#{Enum.count(entity.args)}",
      #       "header" => "#{tail_path}/#{Enum.count(entity.args)}",
      #       "title" => "#{dot_path}/#{Enum.count(entity.args)}"
      #     }
      #   )

      search_data =
        add_schema_to_search_data(
          search_data,
          extension_name,
          entity.schema,
          path
        )

      # sidebar_items =
      #   add_schema_to_sidebar_items(
      #     sidebar_items,
      #     extension_name,
      #     to_string(Enum.at(path, 0)),
      #     entity.schema,
      #     entity.deprecations,
      #     path
      #   )

      entity.entities
      |> Enum.flat_map(&List.wrap(elem(&1, 1)))
      |> Enum.reduce(
        {search_data, sidebar_items},
        &add_entity_to_search_data(extension_name, &1, &2, path)
      )
    end

    # defp add_schema_to_sidebar_items(
    #        sidebar_items,
    #        extension_name,
    #        node_group_name,
    #        schema,
    #        deprecations,
    #        path
    #      ) do
    #   Enum.reduce(schema || [], sidebar_items, fn {key, _config}, sidebar_items ->
    #     path = path ++ [key]
    #     dash_path = Enum.join(path, "-")
    #     dot_path = Enum.join(path, ".")
    #
    #     add_sidebar_item(
    #       sidebar_items,
    #       extension_name,
    #       node_group_name,
    #       %{
    #         "anchor" => dash_path,
    #         "deprecated" => Keyword.has_key?(deprecations, key),
    #         "label" => "DSL Option",
    #         "id" => dot_path,
    #         "hidden" => true,
    #         "title" => dot_path
    #       }
    #     )
    #   end)
    # end

    # defp add_sidebar_item(sidebar_items, extension_name, node_group_name, item) do
    #   sidebar_items
    #   |> Map.put_new("extras", [])
    #   |> Map.update!("extras", fn group ->
    #     group_id = dsl_search_name(extension_name)
    #
    #     group
    #     |> Enum.map(fn group ->
    #       if group["id"] == group_id do
    #         group
    #         |> Map.delete("headers")
    #         |> Map.put("sections", [])
    #         |> Map.put_new("nodeGroups", [])
    #         |> Map.update!("nodeGroups", fn node_groups ->
    #           node_groups
    #           |> ensure_node_group(node_group_name)
    #           |> Enum.map(fn node_group ->
    #             if node_group["name"] == node_group_name do
    #               node_group
    #               |> Map.put_new("nodes", [])
    #               |> Map.update!("nodes", fn nodes ->
    #                 nodes ++ [item]
    #               end)
    #             else
    #               node_group
    #             end
    #           end)
    #         end)
    #       else
    #         group
    #       end
    #     end)
    #   end)
    # end

    # defp ensure_node_group(node_groups, node_group_name) do
    #   if Enum.any?(node_groups, &(&1["name"] == node_group_name)) do
    #     node_groups
    #   else
    #     node_groups ++
    #       [
    #         %{
    #           "key" => node_group_name,
    #           "name" => node_group_name,
    #           "nodes" => []
    #         }
    #       ]
    #   end
    # end

    defp add_schema_to_search_data(
           search_data,
           extension_name,
           schema,
           path
         ) do
      Enum.reduce(schema || [], search_data, fn {key, config}, search_data ->
        add_search_item(
          search_data,
          %{
            "doc" => config[:doc] || "",
            "ref" => "#{dsl_search_name(extension_name)}.html##{Enum.join(path ++ [key], "-")}",
            "title" => "#{extension_name}.#{Enum.join(path ++ [key], ".")}",
            "type" => "DSL"
          }
        )
      end)
    end

    defp dsl_search_name(extension_name) do
      ("dsl-" <> extension_name) |> String.split(".") |> Enum.map_join("-", &String.downcase/1)
    end

    defp add_search_item(search_data, item) do
      item = Map.update!(item, "title", &String.trim(&1 || ""))
      Map.update!(search_data, "items", &Enum.uniq([item | &1]))
    end
  end
else
  defmodule Mix.Tasks.Spark.CheatSheetsInSearch do
    @shortdoc "Includes generated cheat sheets in the search bar"
    @moduledoc @shortdoc
    use Mix.Task

    def run(_opts) do
      raise "#{inspect(__MODULE__)} requires Jason. Please add it as a dev/test dependency."
    end
  end
end
