defmodule Mix.Tasks.Ash.ReplaceDocLinks do
  @moduledoc """
  Replaces any documentation links with text appropriate for hex docs.
  """
  use Mix.Task

  @shortdoc "Replaces any Spark doc links with text appropriate for hex docs."
  def run(_argv) do
    "doc/**/*.html"
    |> Path.wildcard()
    |> Enum.each(fn file ->
      current_project = to_string(Mix.Project.config()[:app])

      new_contents =
        file
        |> File.read!()
        |> Spark.DocIndex.render_replacements(fn
          :mix_dep, %{library: ^current_project}, _context ->
            case Version.parse(Mix.Project.config()[:version]) do
              {:ok, %Version{pre: pre, build: build}} when pre != [] or not is_nil(build) ->
                ~s({:#{current_project}, "~> #{Mix.Project.config()[:version]}"})

              {:ok, %Version{major: major, minor: minor}} ->
                ~s({:#{current_project}, "~> #{major}.#{minor}"})

              _ ->
                ~s({:#{current_project}, "~> x.y.z"})
            end

          :mix_dep, %{library: library}, _context ->
            ~s({:#{library}, "~> x.y.z"})

          :link, %{type: "option", item: item, name_override: name, library: library}, _ ->
            # TODO: validate options
            path =
              item
              |> String.trim_leading("/")
              |> String.split(~r/[\/\.]/)
              |> Enum.drop(1)

            name = name || join_path(path)

            dsl_path = path |> :lists.droplast() |> Enum.map_join("/", &sanitize_name/1)
            anchor = path |> List.last() |> sanitize_name()

            ~s(<a href="https://ash-hq.org/docs/dsl/#{library}/latest/#{dsl_path}##{anchor}">#{name}</a>)

          :link, %{type: "dsl", item: item, name_override: name, library: library}, _ ->
            # TODO: validate dsls
            path =
              item
              |> String.trim_leading("/")
              |> String.split(~r/[\/\.]/)
              |> Enum.drop(1)

            dsl_path = Enum.map_join(path, "/", &sanitize_name/1)

            name = name || join_path(path)

            ~s(<a href="https://ash-hq.org/docs/dsl/#{library}/latest/#{dsl_path}">#{name}</a>)

          :link,
          %{
            type: "extension",
            item: item,
            name_override: name,
            library: ^current_project
          },
          _ ->
            module = Module.concat([item])

            case Code.ensure_compiled(module) do
              {:module, module} ->
                ~s(<a href="#{inspect(module)}.html" class="no-underline">#{name || inspect(module)}</a>)

              _ ->
                ~s(<a href="https://ash-hq.org/docs/dsl/#{current_project}/latest/#{sanitize_name(item)}">#{name || item}</a>)
            end

          :link, %{type: "extension", item: item, name_override: name, library: library}, _ ->
            if String.contains?(item, " ") || !String.contains?(item, ".") do
              ~s(<a href="https://ash-hq.org/docs/dsl/#{current_project}/latest/#{sanitize_name(item)}">#{name || item}</a>)
            else
              ~s(<a href="https://hexdocs.pm/#{library}/#{item}.html" class="no-underline">#{name || item}</a>)
            end

          :link,
          %{type: "guide", item: item, name_override: name, library: ^current_project},
          _ ->
            Enum.find_value(doc_indexes(), fn doc_index ->
              Enum.find(doc_index.guides(), fn guide ->
                sanitize_name(item) == sanitize_name(guide.name) ||
                  sanitize_name_under(item) == sanitize_name_under(guide.name)
              end)
            end)
            |> case do
              nil ->
                raise "No such guide matching name #{item}"

              _ ->
                :ok
            end

            ~s(<a href="#{sanitize_name_under(item)}.html">#{name || item}</a>)

          :link, %{type: "guide", item: item, name_override: name, library: library}, _ ->
            ~s(<a href="https://hexdocs.pm/#{library}/#{sanitize_name_under(item)}.html">#{name || item}</a>)

          :link,
          %{
            type: "module",
            item: item,
            name_override: name,
            library: ^current_project,
            text: text
          },
          _ ->
            module = Module.concat([item])

            case Code.ensure_compiled(module) do
              {:module, module} ->
                in_docs_modules? =
                  Mix.Project.config()[:docs][:groups_for_modules]
                  |> Kernel.||([])
                  |> Enum.any?(fn
                    {_group, %Regex{} = regex} ->
                      Regex.match?(regex, inspect(module))

                    {_group, mods} when is_list(mods) ->
                      module in mods
                  end)

                unless in_docs_modules? do
                  raise """
                  Module #{inspect(module)} referenced in #{text} does not exist in the mix project's `groups_for_modules`.

                  Also be sure to add it to your doc index as well.
                  """
                end

                in_a_spark_index? =
                  Enum.any?(doc_indexes(), fn doc_index ->
                    doc_index.code_modules
                    |> Enum.any?(fn {_key, mods} ->
                      module in mods
                    end)
                  end)

                unless in_a_spark_index? do
                  raise """
                  Module #{inspect(module)} referenced in #{text} does not exist in a `Spark.DocIndex`.
                  """
                end

                ~s(<a href="#{inspect(module)}.html" class="no-underline"><code class="inline">#{name || item}</code></a>)

              {:error, error} ->
                raise """
                Module #{inspect(module)} referenced in #{text} does not exist or could not be compiled: #{inspect(error)}.
                """
            end

          :link, %{type: "module", item: item, name_override: name, library: library}, _ ->
            ~s(<a href="https://hexdocs.pm/#{library}/#{item}.html" class="no-underline">#{name || item}</a>)

          :link, %{type: "function", item: item, name_override: name, library: library}, _ ->
            parts = String.split(item, ".")
            function = List.last(parts)
            module = parts |> :lists.droplast() |> Enum.join(".")

            ~s(<a href="https://hexdocs.pm/#{library}/#{module}.html#{function}" class="no-underline">#{name || item}</a>)

          :link, %{type: "library", name_override: name, library: library}, _ ->
            ~s(<a href="https://hexdocs.pm/#{library}">#{name || library}</a>)

          _, %{text: text}, _ ->
            raise "No link handler for: `#{text}`"
        end)

      File.write!(file, new_contents)
    end)

    "doc/dist/*.js"
    |> Path.wildcard()
    |> Enum.filter(&String.contains?(&1, "sidebar"))
    |> Enum.each(fn file ->
      current_project = to_string(Mix.Project.config()[:app])

      new_contents =
        file
        |> File.read!()
        |> Spark.DocIndex.render_replacements(fn
          :mix_dep, %{library: ^current_project}, _context ->
            case Version.parse(Mix.Project.config()[:version]) do
              {:ok, %Version{pre: pre, build: build}} when pre != [] or not is_nil(build) ->
                ~s({:#{current_project}, "~> #{Mix.Project.config()[:version]}"})

              {:ok, %Version{major: major, minor: minor}} ->
                ~s({:#{current_project}, "~> #{major}.#{minor}"})

              _ ->
                ~s({:#{current_project}, "~> x.y.z"})
            end

          :mix_dep, %{library: library}, _context ->
            library

          :link, %{name_override: name, item: item}, _context ->
            name || item

          _, %{text: text}, _ ->
            text
        end)

      File.write!(file, new_contents)
    end)
  end

  def doc_indexes do
    for module <- modules(),
        {:module, module} = Code.ensure_compiled(module),
        Spark.implements_behaviour?(module, Spark.DocIndex) do
      module
    end
  end

  defp modules do
    Mix.Project.config()[:app]
    |> :application.get_key(:modules)
    |> case do
      {:ok, mods} when is_list(mods) ->
        mods

      _ ->
        []
    end
  end

  defp join_path(path) do
    Enum.join(path, " > ")
  end

  defp sanitize_name(name) do
    String.downcase(String.replace(name, ~r/[^A-Za-z0-9_]/, "-"))
  end

  defp sanitize_name_under(name) do
    String.downcase(String.replace(name, ~r/[^A-Za-z0-9-]/, "_"))
  end
end
