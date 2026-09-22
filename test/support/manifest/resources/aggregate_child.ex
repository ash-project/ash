# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.AggregateChild do
  @moduledoc """
  Test resource holding the embedded attributes that `AggregateSource`
  aggregates, so `AggregateHolder` can aggregate over those aggregates.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
    attribute :nested_first_meta, Ash.Test.Manifest.AggregateNestedFirstMeta
    attribute :nested_list_meta, Ash.Test.Manifest.AggregateNestedListMeta
  end

  relationships do
    belongs_to :source, Ash.Test.Manifest.AggregateSource
  end

  actions do
    defaults [:read]
  end
end
