# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.AggregateSource do
  @moduledoc """
  Test resource holding the embedded attributes and aggregates that
  `AggregateHolder` aggregates.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
    attribute :first_meta, Ash.Test.Manifest.AggregateFirstMeta
    attribute :list_meta, Ash.Test.Manifest.AggregateListMeta
    attribute :unrelated_first_meta, Ash.Test.Manifest.AggregateUnrelatedFirstMeta
    attribute :unrelated_list_meta, Ash.Test.Manifest.AggregateUnrelatedListMeta
    attribute :shadowed_meta, Ash.Test.Manifest.AggregateShadowedMeta
  end

  relationships do
    belongs_to :holder, Ash.Test.Manifest.AggregateHolder
    has_many :children, Ash.Test.Manifest.AggregateChild, destination_attribute: :source_id
  end

  aggregates do
    first :child_first_meta, :children, :nested_first_meta
    first :first_child_nested_list_meta, :children, :nested_list_meta
  end

  actions do
    defaults [:read]
  end
end
