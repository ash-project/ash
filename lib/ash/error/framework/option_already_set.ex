# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Framework.OptionAlreadySet do
  @moduledoc "Used when an option is given for a changeset already validated for an action with a different value"

  use Splode.Error, fields: [:resource, :action, :option, :given, :set], class: :framework

  def message(error) do
    "#{inspect(error.resource)}.#{error.action} was given #{error.option} #{inspect(error.given)}, " <>
      "but the changeset was validated for the action with #{inspect(error.set)}"
  end
end
