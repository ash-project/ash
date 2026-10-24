# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.EtsVersioned.Whisper do
  @moduledoc """
  An expression-only calculation. It has no `calculate/3`, so only the data layer can run it.
  """
  use Ash.Resource.Calculation

  @impl true
  def expression(_opts, _context), do: expr(name <> "...")
end

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

    update :update_in_memory do
      accept [:name]
      require_atomic? false
      atomic_upgrade? false
    end

    update :update_and_load do
      accept [:name]
      change load(:shout)
    end

    update :update_before_2021 do
      accept [:name]
      change filter(expr(now() < ^~U[2021-01-01 00:00:00Z]))
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

  calculations do
    calculate :shout, :string, expr(name <> "!"), public?: true
    calculate :whisper, :string, Ash.Test.Temporal.EtsVersioned.Whisper, public?: true
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
