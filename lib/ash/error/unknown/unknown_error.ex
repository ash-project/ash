# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Unknown.UnknownError do
  @moduledoc "Used when an unknown error occurs"

  use Splode.Error, fields: [:error, :field, :value], class: :unknown

  @type t :: %__MODULE__{
          error: binary() | nil,
          field: term() | nil,
          value: term() | nil
        }

  def message(%{error: error}) do
    if is_binary(error) do
      to_string(error)
    else
      inspect(error)
    end
  end
end
