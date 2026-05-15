# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.Option do
  @moduledoc false
  use Ash.Resource, data_layer: :embedded

  alias Ash.Test.EmbeddedArray.LineItem

  attributes do
    attribute :name, :string, public?: true
    attribute :total_amt, :decimal, public?: true
    attribute :line_items, {:array, LineItem}, public?: true, default: []
  end
end
