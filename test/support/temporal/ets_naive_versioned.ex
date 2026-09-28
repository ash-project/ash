# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.EtsNaiveVersioned do
  @moduledoc """
  A temporal resource on the ETS data layer whose periods are built from naive datetimes.
  """
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  temporal do
    strategy :context
    attribute :valid_during
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
  end

  attributes do
    attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    attribute :name, :string, public?: true

    attribute :valid_during, Ash.Type.Range,
      allow_nil?: false,
      constraints: [
        inner_type: :naive_datetime,
        lower: [inclusive?: true],
        upper: [inclusive?: false]
      ]
  end
end
