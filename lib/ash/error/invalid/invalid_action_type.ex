# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.InvalidActionType do
  @moduledoc "Used when a callback returns an invalid type"

  use Splode.Error, fields: [:message, :type, :expectation, :resource, :action], class: :framework

  def message(%{type: type, expectation: expectation, resource: resource, action: action}) do
    if resource && action do
      "Expected action of type for #{inspect(resource)}.#{inspect(action)}: #{inspect(expectation)} got #{inspect(type)}"
    else
      "Expected action of type: #{inspect(expectation)} got #{inspect(type)}"
    end
  end
end
