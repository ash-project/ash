# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.AtomicsNotSupported do
  @moduledoc "Used when atomics for the given action type are not not supported by the data layer, but one is used."

  use Splode.Error, fields: [:resource, :action_type], class: :invalid

  def message(%{resource: resource, action_type: action_type}) do
    "The data layer for #{inspect(resource)} does not support atomics on #{action_type} actions"
  end
end
