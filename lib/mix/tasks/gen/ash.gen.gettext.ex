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

  ## Workflow

  1. Run `mix ash.gen.gettext` to copy `ash.pot` into your `priv/gettext/`.
  2. Run `mix gettext.merge priv/gettext --locale ko` to create
     `priv/gettext/ko/LC_MESSAGES/ash.po`.
  3. Fill in the `msgstr` values in the `.po` file.
  4. In your `translate_error/1`, call:

         Gettext.dgettext(MyApp.Gettext, "ash", msg, vars)
  """

  use Mix.Task

  @shortdoc "Copies Ash's .pot file for error message translation"

  @impl Mix.Task
  def run(_argv) do
    source = Application.app_dir(:ash, "priv/gettext/ash.pot")

    if !File.exists?(source) do
      Mix.raise(
        "Could not find #{source}. Run `mix ash.gettext.extract` in the Ash project first."
      )
    end

    dest_dir = "priv/gettext"
    dest = Path.join(dest_dir, "ash.pot")

    File.mkdir_p!(dest_dir)
    File.cp!(source, dest)

    Mix.shell().info([:green, "* creating ", :reset, dest])
  end
end
