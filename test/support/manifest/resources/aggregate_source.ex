# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.AggregateSource do
  @moduledoc """
  Test resource holding the embedded attributes that `AggregateHolder` aggregates.
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
  end

  relationships do
    belongs_to :holder, Ash.Test.Manifest.AggregateHolder
  end

  actions do
    defaults [:read]
  end
end
