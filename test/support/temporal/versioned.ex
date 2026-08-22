# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.Versioned do
  @moduledoc """
  A temporal resource whose period is declared by hand.
  """
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.Test.Temporal.StubDataLayer

  temporal do
    strategy :context
    attribute :valid_at
  end

  actions do
    defaults [:read, :destroy]

    create :create do
      accept [:id, :name]
    end

    update :update do
      accept [:name]
    end
  end

  attributes do
    attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    attribute :name, :string, public?: true

    attribute :valid_at, Ash.Type.Range,
      allow_nil?: false,
      generated?: true,
      constraints: [
        inner_type: :datetime,
        lower: [inclusive?: true],
        upper: [inclusive?: false]
      ]
  end

  relationships do
    has_many :notes, Ash.Test.Temporal.VersionedNote do
      no_attributes? true
      temporal_keys {:valid_at, :valid_at}
    end
  end
end
