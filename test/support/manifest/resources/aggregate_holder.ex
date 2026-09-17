# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.AggregateHolder do
  @moduledoc """
  Test resource whose embedded types are reachable only through aggregates.

  The `:sources` relationship is private, so reachability does not reach
  `AggregateSource` or its embedded attributes through it. The `first` and
  `list` aggregates are the only public path to `AggregateFirstMeta` and
  `AggregateListMeta`.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
  end

  relationships do
    has_many :sources, Ash.Test.Manifest.AggregateSource, destination_attribute: :holder_id
  end

  aggregates do
    first :first_meta, :sources, :first_meta, public?: true
    list :list_metas, :sources, :list_meta, public?: true
  end

  actions do
    defaults [:read]
  end
end
