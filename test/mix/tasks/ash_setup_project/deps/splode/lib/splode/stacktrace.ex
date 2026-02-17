# SPDX-FileCopyrightText: 2024 splode contributors <https://github.com/ash-project/splode/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Splode.Stacktrace do
  @moduledoc "A placeholder for a stacktrace so that we can avoid printing it everywhere"
  defstruct [:stacktrace]

  @type t :: %__MODULE__{stacktrace: list}

  defimpl Inspect do
    def inspect(_, _) do
      "#Splode.Stacktrace<>"
    end
  end
end
