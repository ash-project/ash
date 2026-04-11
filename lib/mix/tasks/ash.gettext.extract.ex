# SPDX-FileCopyrightText: 2024 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Gettext.Extract do
  @moduledoc """
  Extracts translatable error messages from Ash source files and generates
  a `.pot` (Portable Object Template) file.

  Scans for `error_message("...")` macro calls, recording each call site
  with its file path and line number as standard gettext source references.

  ## Usage

      $ mix ash.gettext.extract

  ## Options

    * `--output` / `-o` — Path to write the `.pot` file.
      Defaults to `priv/gettext/ash.pot`.

    * `--domain` / `-d` — Gettext domain written into the file header.
      Defaults to `"ash"`.

  ## Workflow

  This task is for Ash development. It regenerates `priv/gettext/ash.pot`
  which is shipped with the hex package.

  Users of Ash should run `mix ash.gen.gettext` to copy the `.pot` file
  into their application.
  """

  use Mix.Task

  @shortdoc "Extracts Ash error messages into a .pot file"

  @default_output "priv/gettext/ash.pot"
  @default_domain "ash"

  @impl Mix.Task
  def run(argv) do
    {opts, _} =
      OptionParser.parse!(argv,
        strict: [output: :string, domain: :string],
        aliases: [o: :output, d: :domain]
      )

    output = Keyword.get(opts, :output, @default_output)
    domain = Keyword.get(opts, :domain, @default_domain)

    messages = extract_messages()

    pot_content = build_pot(messages, domain)

    output |> Path.dirname() |> File.mkdir_p!()
    File.write!(output, pot_content)

    Mix.shell().info([:green, "* creating ", :reset, output])
    Mix.shell().info("  #{map_size(messages)} unique message(s) written.")
  end

  @doc false
  def extract_messages do
    "lib/ash/**/*.ex"
    |> Path.wildcard()
    |> Enum.reduce(%{}, fn file, acc ->
      case file |> File.read!() |> Code.string_to_quoted(file: file) do
        {:ok, ast} ->
          ast
          |> extract_from_ast(file)
          |> Enum.reduce(acc, fn {msg, ref}, map ->
            Map.update(map, msg, [ref], &[ref | &1])
          end)

        {:error, reason} ->
          Mix.shell().error(
            "ash.gettext.extract: failed to parse #{file}: #{inspect(reason)}; skipping file"
          )

          acc
      end
    end)
  end

  defp extract_from_ast(ast, file) do
    {_, results} =
      Macro.prewalk(ast, [], fn
        {:error_message, meta, [msg]} = node, acc when is_binary(msg) ->
          line = Keyword.get(meta, :line, 0)
          {node, [{msg, {file, line}} | acc]}

        node, acc ->
          {node, acc}
      end)

    results
  end

  defp build_pot(messages, domain) do
    header = pot_header(domain)

    entries =
      messages
      |> Enum.sort_by(fn {msg, _refs} -> msg end)
      |> Enum.map_join("\n", fn {msg, refs} ->
        pot_entry(msg, Enum.sort_by(refs, fn {file, line} -> {file, line} end))
      end)

    header <> entries
  end

  defp pot_header(domain) do
    """
    ## This file is a PO Template file.
    ##
    ## `msgid`s here are often extracted from source code.
    ## Add new translations manually only if they're not extracted.
    ##
    ## Run `mix ash.gettext.extract` to refresh this file.
    ## Run `mix ash.gen.gettext` in your app to copy this file.
    ## Run `mix gettext.merge priv/gettext --locale LOCALE` to merge into a .po file.

    msgid ""
    msgstr ""
    "Content-Type: text/plain; charset=UTF-8\\n"
    "Content-Transfer-Encoding: 8bit\\n"
    "Domain: #{domain}\\n"

    """
  end

  defp pot_entry(msg, refs) do
    ref_lines = Enum.map_join(refs, "\n", fn {file, line} -> "#: #{file}:#{line}" end)

    """
    #{ref_lines}
    msgid #{inspect(msg)}
    msgstr ""

    """
  end
end
