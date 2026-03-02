# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.InvalidStoredData do
  @moduledoc "Used when stored data cannot be cast to its expected type when loading from the data layer."

  use Splode.Error, fields: [:resource, :field], class: :invalid

  def message(%{resource: resource, field: field}) when not is_nil(resource) do
    "Could not load #{inspect(resource)}: invalid stored data for #{field}"
  end

  def message(%{field: field}) do
    "Invalid stored data for #{field}"
  end
end
