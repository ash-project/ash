# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Union do
  @moduledoc """
  A wrapper for values that are sourced from `Ash.Type.Union`.
  """

  @type t :: %__MODULE__{}
  defstruct [:value, :type]

  defimpl Jason.Encoder do
    def encode(%{value: value}, opts) do
      Jason.Encode.value(value, opts)
    end
  end
end
