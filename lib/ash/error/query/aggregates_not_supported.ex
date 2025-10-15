# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.AggregatesNotSupported do
  @moduledoc "Used when the data_layer does not support aggregates, or filtering/sorting them"

  use Splode.Error, fields: [:resource, :feature, type: :aggregate], class: :invalid

  def message(%{resource: resource, feature: feature, type: type}) do
    type =
      case type do
        :aggregate ->
          "aggregates"

        :query_aggregate ->
          "query aggregates"
      end

    "Data layer for #{inspect(resource)} does not support #{feature} #{type}"
  end
end
