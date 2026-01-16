# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Stacktrace do
  @moduledoc "Utilities for working with stacktraces"
  defstruct [:stacktrace]

  @type t :: %__MODULE__{stacktrace: list()}

  defimpl Inspect do
    def inspect(_, _) do
      "#Stacktrace<>"
    end
  end
end
