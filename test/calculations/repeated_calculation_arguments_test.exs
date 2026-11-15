# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Calculations.RepeatedCalculationArgumentsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  import Ash.Expr

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Item do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Calculations.RepeatedCalculationArgumentsTest.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :base, :integer, public?: true
    end

    calculations do
      calculate :plus, :integer, expr(base + ^arg(:amount)) do
        public? true
        argument :amount, :integer, allow_nil?: false
      end

      calculate :combined, :integer, expr(plus(amount: 1) * 1000 + plus(amount: 2)), public?: true

      calculate :nested_combined, :integer, expr(combined * 10 + plus(amount: 3)), public?: true
    end
  end

  setup do
    %{item: Ash.create!(Item, %{base: 5})}
  end

  test "each reference to a calculation uses its own arguments when loaded", %{item: item} do
    assert %{combined: 6007, nested_combined: 60_078} =
             Ash.load!(item, [:combined, :nested_combined])
  end

  test "each reference to a calculation uses its own arguments when read", %{item: item} do
    assert [%{combined: 6007}] =
             Item
             |> Ash.Query.filter(id == ^item.id)
             |> Ash.Query.load(:combined)
             |> Ash.read!()
  end

  test "each reference to a calculation uses its own arguments in a filter", %{item: item} do
    assert [_] = Item |> Ash.Query.filter(combined == 6007) |> Ash.read!()
    assert [] = Item |> Ash.Query.filter(combined in [6006, 7007]) |> Ash.read!()
    assert [_] = Item |> Ash.Query.filter(plus(amount: 1) < plus(amount: 2)) |> Ash.read!()
    _ = item
  end

  test "each reference to a calculation uses its own arguments with Ash.calculate", %{
    item: item
  } do
    assert {:ok, 6007} = Ash.calculate(item, :combined)
  end

  test "a calculation loaded with other arguments is not used for a reference", %{item: item} do
    assert %{combined: 6007, plus: 14} =
             Ash.load!(item, [:combined, plus: %{amount: 9}])

    assert %{combined: 6007} =
             item
             |> Ash.load!(plus: %{amount: 9})
             |> Ash.load!(:combined)
  end

  test "a runtime filter on already loaded records uses each reference's arguments", %{
    item: item
  } do
    item = Ash.load!(item, plus: %{amount: 9})

    assert {:ok, [_]} =
             Ash.Filter.Runtime.filter_matches(
               Domain,
               [item],
               Ash.Filter.parse!(Item, expr(combined == 6007))
             )
  end
end
