# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.AggregateHolder do
  @moduledoc """
  Test resource whose embedded types are reachable only through aggregates.

  The `:sources` relationship is private, so reachability does not reach
  `AggregateSource`, `AggregateChild` or their embedded attributes through it.
  Each public aggregate is the only path to one embedded type:

    * `first` and `list` over a related attribute
    * `first` and `list` with `related?: false`, over an attribute of `AggregateSource`
    * `first` and `list` over an aggregate on `AggregateSource`
    * `first` with `related?: false` over `:shadowed_meta`, a field name this
      resource also uses for a private `:string` attribute
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
    attribute :shadowed_meta, :string
  end

  relationships do
    has_many :sources, Ash.Test.Manifest.AggregateSource, destination_attribute: :holder_id
  end

  aggregates do
    first :first_meta, :sources, :first_meta, public?: true
    list :list_metas, :sources, :list_meta, public?: true

    first :unrelated_first_meta, Ash.Test.Manifest.AggregateSource, :unrelated_first_meta,
      public?: true

    list :unrelated_list_metas, Ash.Test.Manifest.AggregateSource, :unrelated_list_meta,
      public?: true

    first :nested_first_meta, :sources, :child_first_meta, public?: true
    list :nested_list_metas, :sources, :first_child_nested_list_meta, public?: true

    first :unrelated_shadowed_meta, Ash.Test.Manifest.AggregateSource, :shadowed_meta,
      public?: true
  end

  actions do
    defaults [:read]
  end
end
