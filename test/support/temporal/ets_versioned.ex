# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.EtsVersioned do
  @moduledoc """
  A temporal resource on the ETS data layer.

  The period is never action input, so a test that needs a particular one seeds it
  with `Ash.Seed.seed!/2`. It is declared with second precision, so writes are cast to
  whole seconds.
  """
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  temporal do
    strategy :context
    attribute :valid_at
  end

  actions do
    defaults [:read, :destroy]

    create :create do
      primary? true
      accept [:id, :name]
    end

    update :update do
      primary? true
      accept [:name]
    end

    create :upsert do
      accept [:id, :name]
      upsert? true
    end

    destroy :cancel do
      soft? true
      change set_attribute(:name, "cancelled")
    end
  end

  attributes do
    attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    attribute :name, :string, public?: true

    attribute :valid_at, Ash.Type.Range,
      allow_nil?: false,
      constraints: [
        inner_type: :datetime,
        inner_constraints: [precision: :second],
        lower: [inclusive?: true],
        upper: [inclusive?: false]
      ]
  end
end
