# SPDX-FileCopyrightText: 2024 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Gettext do
  @moduledoc """
  Provides the `error_message/1` macro for marking translatable error message strings.

  At runtime, `error_message("must be present")` simply returns the string as-is.
  The macro serves as a marker so that `mix ash.gettext.extract` can scan source files
  and extract all error message strings into a `.pot` file for translation.

  ## Usage

      import Ash.Gettext

      message: error_message("must equal %{value}")

  This is automatically imported by `use Ash.Resource.Validation`.
  """

  @doc """
  Marks a string as a translatable error message.

  Returns the string unchanged at runtime. Used by `mix ash.gettext.extract`
  to extract messages for gettext `.pot` file generation.
  """
  defmacro error_message(msg) do
    quote do: unquote(msg)
  end
end
