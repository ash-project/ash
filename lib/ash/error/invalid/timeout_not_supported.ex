# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.TimeoutNotSupported do
  @moduledoc "Used when timeouts are not supported by the data layer, but one is set"

  use Splode.Error, fields: [:resource], class: :invalid

  def message(%{resource: resource}) do
    "The data layer for #{inspect(resource)} does not support timeouts"
  end
end
