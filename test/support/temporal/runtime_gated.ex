# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.RuntimeGated do
  @moduledoc """
  A temporal resource whose data layer decides at runtime whether it can serve an `as_of`
  at all. See `Ash.Test.Temporal.RuntimeGatedDataLayer`.
  """
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.Test.Temporal.RuntimeGatedDataLayer

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
end
