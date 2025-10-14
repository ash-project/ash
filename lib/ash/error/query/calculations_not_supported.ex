# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.CalculationsNotSupported do
  @moduledoc "Used when the data_layer does not support calculations, or filtering/sorting them"

  use Splode.Error, fields: [:resource, :feature], class: :invalid

  def message(%{resource: resource, feature: feature}) do
    "Data layer for #{inspect(resource)} does not support #{feature} calculations"
  end
end
