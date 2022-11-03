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
      new_contents =
        file
        |> File.read!()
        |> Spark.DocIndex.render_replacements(fn
          :mix_dep, %{library: library}, _context ->
            ~s({:#{library}, "~> x.y.z"})

          :link, %{type: "option", item: item, name_override: name, library: library}, _ ->
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
            path =
              item
              |> String.trim_leading("/")
              |> String.split(~r/[\/\.]/)
              |> Enum.drop(1)

            dsl_path = Enum.map_join(path, "/", &sanitize_name/1)

            name = name || join_path(path)

            ~s(<a href="https://ash-hq.org/docs/dsl/#{library}/latest/#{dsl_path}">#{name}</a>)

          :link, %{type: "extension", item: item, name_override: name, library: library}, _ ->
            ~s(<a href="https://ash-hq.org/docs/dsl/#{library}/latest/#{sanitize_name(item)}">#{name || item}</a>)

          :link, %{type: "guide", item: item, name_override: name, library: library}, _ ->
            ~s(<a href="https://ash-hq.org/docs/dsl/#{library}/latest/#{sanitize_name(item)}">#{name || item}</a>)

          :link, %{type: "module", item: item, name_override: name, library: library}, _ ->
            ~s(<a href="/docs/module/#{library}/latest/#{sanitize_name(item)}">#{name || item}</a>)

          _, %{text: text}, _ ->
            raise "No link handler for: `#{text}`"
        end)

      File.write!(file, new_contents)
    end)
  end

  defp join_path(path) do
    Enum.join(path, " > ")
  end

  defp sanitize_name(name) do
    String.downcase(String.replace(name, ~r/[^A-Za-z0-9_]/, "-"))
  end
end
