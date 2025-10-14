# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.NonCountableAction do
  @moduledoc "Used when page[:count] option is passed but the action's pagination is not countable."
  use Splode.Error, fields: [:resource, :action], class: :invalid

  def message(%{resource: resource, action: action}) do
    "Action #{inspect(resource)}.#{action} cannot be counted while paginating but a count was requested."
  end
end
