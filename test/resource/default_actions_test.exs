# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.DefaultActionsTest do
  @moduledoc false
  use ExUnit.Case, async: true
  use Mimic

  test "when the data layer does not support transactions, it doesn't enable them on the generated default actions" do
    defmodule TransactionFree do
      @moduledoc false
      use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
      end

      actions do
        defaults [:read, :destroy, create: :*, update: :*]
      end
    end

    transacting_actions =
      TransactionFree
      |> Ash.Resource.Info.actions()
      |> Enum.filter(&(&1.transaction? == true))
      |> Enum.map(& &1.name)

    assert transacting_actions == []
  end

  test "when the data layer does support transactions, it enables them on the generated default actions" do
    Ash.DataLayer
    |> Mimic.stub(:can?, fn
      :transact, _ -> true
      cap, resource -> call_original(Ash.DataLayer, :can?, [cap, resource])
    end)

    defmodule TransactionFiend do
      @moduledoc false
      use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
      end

      actions do
        defaults [:read, :destroy, create: :*, update: :*]
      end
    end

    transacting_actions =
      TransactionFiend
      |> Ash.Resource.Info.actions()
      |> Enum.filter(&(&1.transaction? == true))
      |> Enum.map(& &1.name)
      |> Enum.sort()

    assert transacting_actions == [:create, :destroy, :update]
  end
end
