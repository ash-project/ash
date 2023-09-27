defmodule Mix.Tasks.Ash.ReplaceDocLinks do
  @moduledoc """
  Replaces any documentation links with text appropriate for hex docs.
  """
  use Mix.Task

  @shortdoc "Replaces any ash specific doc links with text appropriate for hex docs."
  def run(_argv) do
    mix_project = Mix.Project.get!()
    module_prefix = mix_project |> Module.split() |> Enum.at(0)

    "doc/**/*.html"
    |> Path.wildcard()
    |> Enum.each(fn file ->
      new_contents =
        file
        |> File.read!()
        |> String.replace(~r/\>d\:.*\</, fn ">d:" <> contents ->
          contents =
            contents
            |> String.trim_trailing("<")
            |> String.replace("|", ".")

          module_name =
            contents
            |> String.split(".")
            |> Enum.take_while(&capitalized?/1)
            |> Enum.join(".")

          if String.starts_with?(module_name, module_prefix) do
            case Code.ensure_compiled(Module.concat([module_name])) do
              {:module, _} ->
                :ok

              {:error, error} ->
                raise "Expected #{module_name} to be compiled, but it was not: #{inspect(error)}"
            end
          end

          name =
            module_name
            |> String.trim_trailing(".Dsl")
            |> String.split(".")
            |> Enum.map_join("-", &String.downcase/1)

          rest = contents |> String.trim_leading(module_name <> ".") |> String.replace(".", "-")

          "><a href=\"dsl-#{name}.html##{rest}\">#{contents}</a><"
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
