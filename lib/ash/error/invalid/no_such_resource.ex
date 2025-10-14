# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.NoSuchResource do
  @moduledoc "Used when a resource or alias is provided that doesn't exist"

  use Splode.Error, fields: [:resource, :message], class: :invalid

  def message(%{message: message}) when message not in ["", nil], do: message

  def message(%{resource: resource}) do
    "No such resource #{inspect(resource)}"
  end
end
