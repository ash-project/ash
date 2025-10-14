# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.NoPrimaryAction do
  @moduledoc "Used when an action name is provided that doesn't exist"

  use Splode.Error, fields: [:resource, :type], class: :invalid

  def message(%{resource: resource, type: type}) do
    "No primary action of type #{inspect(type)} for resource #{inspect(resource)}, and no action specified"
  end
end
