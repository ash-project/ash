# SPDX-FileCopyrightText: 2024 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Gen.Gettext do
  @moduledoc """
  Copies Ash's `.pot` file into your application's `priv/gettext/` directory.

  This allows you to create locale-specific `.po` files for translating
  Ash error messages using the standard gettext workflow.

  ## Usage

      $ mix ash.gen.gettext
      $ mix ash.gen.gettext --domain errors

  ## Options

    * `--domain` / `-d` — Target gettext domain. Defaults to `"ash"`.

      When set to a domain other than `"ash"` (e.g., `"errors"`), Ash's
      translatable messages are **merged** into the existing
      `priv/gettext/<domain>.pot` file. The Ash entries are placed
      after a marker comment and replaced on each run, making this
      operation idempotent. Multiple libraries (e.g., `ash`,
      `ash_authentication`) can each maintain their own marked
      section in the same file.

      This is useful when your application already translates errors
      under the `"errors"` domain and you want Ash messages available
      there without changing your `translate_error/1` code.

  ## Workflow

  ### Default (`ash` domain)

  1. Run `mix ash.gen.gettext` to copy `ash.pot` into your `priv/gettext/`.
  2. Run `mix gettext.merge priv/gettext --locale ko` to create
     `priv/gettext/ko/LC_MESSAGES/ash.po`.
  3. Fill in the `msgstr` values in the `.po` file.
  4. In your `translate_error/1`, call:

         Gettext.dgettext(MyApp.Gettext, "ash", msg, vars)

  ### Custom domain (e.g., `errors`)

  1. Run `mix ash.gen.gettext --domain errors` to merge Ash messages
     into your existing `priv/gettext/errors.pot`.
  2. Run `mix gettext.merge priv/gettext --locale ko` as usual.
  3. Your existing `translate_error/1` using `dgettext(backend, "errors", ...)`
     works without changes.
  """

  use Mix.Task

  @shortdoc "Copies Ash's .pot file for error message translation"

  @marker "## -- ash-gettext:start --"
  @marker_pattern ~r/^## -- [\w]+-gettext:start --$/m
  @domain_regex ~r/^[a-z][a-z0-9_]*$/

  @impl Mix.Task
  def run(argv) do
    {opts, _} =
      OptionParser.parse!(argv,
        strict: [domain: :string],
        aliases: [d: :domain]
      )

    domain = Keyword.get(opts, :domain, "ash")

    if not Regex.match?(@domain_regex, domain) do
      Mix.raise(
        "Invalid domain #{inspect(domain)}. Must be lowercase alphanumeric with underscores."
      )
    end

    source = Application.app_dir(:ash, "priv/gettext/ash.pot")

    if !File.exists?(source) do
      Mix.raise(
        "Could not find #{source}. Run `mix ash.gettext.extract` in the Ash project first."
      )
    end

    dest_dir = "priv/gettext"
    dest = Path.join(dest_dir, "#{domain}.pot")

    File.mkdir_p!(dest_dir)

    if domain == "ash" do
      File.cp!(source, dest)
      Mix.shell().info([:green, "* creating ", :reset, dest])
    else
      ash_block = build_ash_block(source)

      content =
        case File.read(dest) do
          {:ok, existing} ->
            merge_into_existing(existing, ash_block)

          {:error, :enoent} ->
            pot_header(domain) <> ash_block

          {:error, reason} ->
            Mix.raise("Could not read #{dest}: #{inspect(reason)}")
        end

      File.write!(dest, content)

      Mix.shell().info([:green, "* updating ", :reset, dest])
    end
  end

  defp build_ash_block(source_path) do
    entries =
      source_path
      |> File.read!()
      |> extract_entries()

    @marker <> "\n" <> entries
  end

  defp extract_entries(pot_content) do
    entries =
      pot_content
      |> String.split("\n\n")
      |> Enum.filter(&String.contains?(&1, "msgid \""))
      |> Enum.reject(fn chunk ->
        chunk |> String.trim() |> String.starts_with?("msgid \"\"")
      end)
      |> Enum.map_join("\n\n", &String.trim/1)

    entries <> "\n\n"
  end

  defp merge_into_existing(existing, ash_block) do
    if String.contains?(existing, @marker) do
      [before, rest] = String.split(existing, @marker, parts: 2)

      after_block =
        case Regex.run(@marker_pattern, rest, return: :index) do
          [{pos, _len}] ->
            String.slice(rest, pos, String.length(rest))

          nil ->
            ""
        end

      String.trim_trailing(before) <> "\n\n" <> ash_block <> after_block
    else
      String.trim_trailing(existing) <> "\n\n" <> ash_block
    end
  end

  defp pot_header(domain) do
    """
    ## This file is a PO Template file.
    ##
    ## Run `mix gettext.merge priv/gettext --locale LOCALE` to merge into a .po file.

    msgid ""
    msgstr ""
    "Content-Type: text/plain; charset=UTF-8\\n"
    "Content-Transfer-Encoding: 8bit\\n"
    "Domain: #{domain}\\n"

    """
  end
end
