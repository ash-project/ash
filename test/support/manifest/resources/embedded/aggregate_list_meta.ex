# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.AggregateListMeta do
  @moduledoc """
  Test embedded resource reachable only through a `list` aggregate.
  """
  use Ash.Resource,
    data_layer: :embedded,
    domain: nil

  attributes do
    attribute :score, :integer, public?: true
  end
end
