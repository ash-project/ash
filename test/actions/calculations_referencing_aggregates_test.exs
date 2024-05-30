defmodule Ash.Test.Actions.CalculationsReferenceAggregatesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule One do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Actions.CalculationsReferenceAggregatesTest.Domain

    ets do
      private? true
    end

    actions do
      defaults [:read]
    end

    relationships do
      belongs_to :two, Ash.Test.Actions.CalculationsReferenceAggregatesTest.Two
    end

    attributes do
      uuid_primary_key :id
      attribute :quantity, :integer, allow_nil?: false, public?: true, constraints: [min: 0]
    end
  end

  defmodule Two do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Actions.CalculationsReferenceAggregatesTest.Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:read]
    end

    relationships do
      has_many :one, One
    end

    aggregates do
      sum :total_quantity, :one, :quantity, default: 0
    end
  end

  defmodule Three do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Actions.CalculationsReferenceAggregatesTest.Domain

    ets do
      private? true
    end

    actions do
      defaults [:read]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :two, Two
      belongs_to :four, Ash.Test.Actions.CalculationsReferenceAggregatesTest.Four
    end
  end

  defmodule Four do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Actions.CalculationsReferenceAggregatesTest.Domain

    ets do
      private? true
    end

    actions do
      defaults [:read, :create]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      has_many :three, Three
    end

    calculations do
      # This throws
      calculate :total_quantity,
                :integer,
                expr(sum(three.two, field: :total_quantity) || 0)
    end
  end

  defmodule Domain do
    use Ash.Domain

    resources do
      resource One
      resource Two
      resource Three
      resource Four
    end
  end

  test "loading calculations that reference aggregates" do
    Four
    |> Ash.create!(%{})
    |> Ash.load!(:total_quantity)
    |> Map.get(:total_quantity)
    |> Kernel.==(0)
    |> assert()
  end
end
