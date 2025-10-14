# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.SimpleDataLayer.NoDataProvided do
  @moduledoc "Used when no data was provided to the simple data layer"

  use Splode.Error, fields: [:resource, :message], class: :framework

  def message(%{message: message}) when message not in ["", nil], do: message

  def message(%{resource: resource}) do
    "No data provided in resource #{inspect(resource)}"
  end
end
