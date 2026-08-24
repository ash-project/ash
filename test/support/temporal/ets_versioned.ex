# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.EtsVersioned do
  @moduledoc """
  A temporal resource on the ETS data layer.

  The period is never action input, so a test that needs a particular one seeds it
  with `Ash.Seed.seed!/2`.
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
      accept [:id, :name]
    end

    update :update do
      accept [:name]
    end
  end

  attributes do
    attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    attribute :name, :string, public?: true
  end
end
