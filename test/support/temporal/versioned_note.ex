# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.VersionedNote do
  @moduledoc """
  A temporal resource related to another one, so both sides of a relationship
  carry a period.
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
      accept [:id, :body, :versioned_id]
    end
  end

  attributes do
    attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    attribute :body, :string, public?: true

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
    belongs_to :versioned, Ash.Test.Temporal.Versioned do
      attribute_type :integer
      temporal_keys {:valid_at, :valid_at}
    end
  end
end
