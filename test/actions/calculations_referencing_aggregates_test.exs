# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.CalculationsReferenceAggregatesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

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

  defmodule Item do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Actions.CalculationsReferenceAggregatesTest.Domain

    ets do
      private? true
    end

    actions do
      default_accept [:parent_id, :value, :category]
      defaults [:read, :create]
    end

    attributes do
      uuid_primary_key :id
      attribute :parent_id, :uuid, public?: true
      attribute :value, :integer, public?: true
      attribute :category, :atom, constraints: [one_of: [:a, :b]], public?: true
    end

    relationships do
      belongs_to :parent, Ash.Test.Actions.CalculationsReferenceAggregatesTest.Parent,
        destination_attribute: :id,
        source_attribute: :parent_id
    end
  end

  defmodule Parent do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Actions.CalculationsReferenceAggregatesTest.Domain

    ets do
      private? true
    end

    actions do
      default_accept [:name]
      defaults [:read, :create]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    aggregates do
      sum :sum_of_category_a_values, :items, :value do
        filter expr(category == :a)
      end

      count :count_of_category_b, :items do
        filter expr(category == :b)
      end
    end

    calculations do
      calculate :combined_metric,
                :integer,
                expr(sum_of_category_a_values + count_of_category_b)
    end

    relationships do
      has_many :items, Ash.Test.Actions.CalculationsReferenceAggregatesTest.Item,
        destination_attribute: :parent_id
    end
  end

  defmodule Domain do
    use Ash.Domain

    authorization do
      require_actor? true
    end

    resources do
      resource One
      resource Two
      resource Three
      resource Four
      resource Item
      resource Parent
    end
  end

  test "loading calculations that reference aggregates" do
    Four
    |> Ash.create!(%{}, actor: nil)
    |> Ash.load!(:total_quantity, actor: nil)
    |> Map.get(:total_quantity)
    |> Kernel.==(0)
    |> assert()
  end

  test "loading calculations with multiple filtered aggregates" do
    parent = Parent |> Ash.create!(%{name: "test"}, actor: nil)

    _item_a1 =
      Item |> Ash.create!(%{parent_id: parent.id, value: 10, category: :a}, actor: nil)

    _item_a2 =
      Item |> Ash.create!(%{parent_id: parent.id, value: 20, category: :a}, actor: nil)

    _item_b1 = Item |> Ash.create!(%{parent_id: parent.id, value: 5, category: :b}, actor: nil)

    _item_b2 =
      Item |> Ash.create!(%{parent_id: parent.id, value: 15, category: :b}, actor: nil)

    result =
      Parent
      |> Ash.Query.for_read(:read, %{}, actor: nil)
      |> Ash.Query.filter(id == ^parent.id)
      |> Ash.Query.load([:combined_metric, :sum_of_category_a_values, :count_of_category_b])
      |> Ash.read!(actor: nil)
      |> Enum.at(0)

    assert result.sum_of_category_a_values == 30
    assert result.count_of_category_b == 2
    assert result.combined_metric == 32

    result_with_shared_loading =
      Parent
      |> Ash.Query.for_read(:read, %{}, actor: nil)
      |> Ash.Query.filter(id == ^parent.id)
      |> Ash.Query.load([:count_of_category_b, :combined_metric])
      |> Ash.read!(actor: nil)
      |> Enum.at(0)

    assert result_with_shared_loading.combined_metric == 32
    assert result_with_shared_loading.count_of_category_b == 2
  end
end
