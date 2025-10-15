# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.InvalidPrimaryKey do
  @moduledoc "Used when an invalid primary key is given to `Ash.get/2`"
  use Splode.Error, fields: [:resource, :value], class: :invalid

  def message(%{resource: _resource, value: value}) do
    "invalid primary key #{inspect(value)} provided"
  end
end
