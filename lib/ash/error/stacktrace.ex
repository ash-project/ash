# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Stacktrace do
  @moduledoc "A placeholder for a stacktrace so that we can avoid printing it everywhere"
  defstruct [:stacktrace]

  @type t :: %__MODULE__{stacktrace: list()}

  defimpl Inspect do
    def inspect(_, _) do
      "#Stacktrace<>"
    end
  end
end
