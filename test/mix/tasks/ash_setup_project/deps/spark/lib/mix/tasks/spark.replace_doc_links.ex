# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Spark.ReplaceDocLinks do
  @moduledoc """
  Replaces any documentation links with text appropriate for hex docs.

  This makes projects support
  """
  use Mix.Task

  @auto_prefixes ~w(
    ash ash_postgres ash_graphql ash_phoenix ash_authentication ash_archival
    ash_json_api ash_admin ash_state_machine ash_oban ash_geo ash_appsignal ash_rbac ash_query_builder
    ash_uuid ash_csv pyro smokestack ash_thrift ash_double_entry ash_ulid
  )

  @prefixes @auto_prefixes
            |> Map.new(&{Macro.camelize(&1), &1})
            |> Map.merge(%{
              "AshAuthentication.Phoenix" => "ash_authentication_phoenix"
            })

  @shortdoc "Replaces any spark dsl specific doc links with text appropriate for hex docs."
  def run(_argv) do
    mix_project = Mix.Project.get!()
    module_prefix = mix_project |> Module.split() |> Enum.at(0)

    "doc/**/*.html"
    |> Path.wildcard()
    |> Enum.each(fn file ->
      new_contents =
        file
        |> File.read!()
        |> String.replace(~r/\>d\:[a-zA-Z0-9|_\?\!\.]*\</, fn ">d:" <> contents ->
          contents =
            contents
            |> String.trim_trailing("<")
            |> String.replace("|", ".")

          module_name =
            contents
            |> String.split(".")
            |> Enum.take_while(&capitalized?/1)
            |> Enum.join(".")

          url_prefix =
            if String.starts_with?(module_name, module_prefix <> ".") do
              case Code.ensure_compiled(Module.concat([module_name])) do
                {:module, _} ->
                  {:ok, ""}

                {:error, error} ->
                  raise "Expected #{module_name} to be compiled because the link \"d:#{contents}\" was used, but it was not available: #{inspect(error)}"
              end
            else
              @prefixes
              |> Enum.filter(fn {key, _value} -> String.starts_with?(module_name, key) end)
              |> Enum.sort_by(fn {key, _} -> String.length(key) end)
              |> Enum.reverse()
              |> Enum.at(0)
              |> case do
                nil ->
                  :error

                {_, package_name} ->
                  {:ok, "https://hexdocs.pm/#{package_name}/"}
              end
            end

          case url_prefix do
            {:ok, prefix} ->
              name =
                module_name
                |> String.trim_trailing(".Dsl")
                |> String.split(".")
                |> Enum.map_join("-", &String.downcase/1)

              rest =
                contents |> String.trim_leading(module_name <> ".") |> String.replace(".", "-")

              "><a href=\"#{prefix}dsl-#{name}.html##{rest}\">#{contents}</a><"

            :error ->
              ">#{contents}<"
          end
        end)

      File.write!(file, new_contents)
    end)
  end

  defp capitalized?(string) do
    first =
      string
      |> String.graphemes()
      |> Enum.at(0)

    String.downcase(first) != first
  end
end
