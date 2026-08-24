# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.ActionTransactionsTest do
  @moduledoc false
  use ExUnit.Case, async: true
  use Mimic

  defp transacting_actions(resource) do
    resource
    |> Ash.Resource.Info.actions()
    |> Enum.filter(& &1.transaction?)
    |> Enum.map(& &1.name)
    |> Enum.sort()
  end

  test "when the data layer does not support transactions, it doesn't enable them on explicitly defined actions" do
    defmodule ExplicitTransactionFree do
      @moduledoc false
      use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
      end

      actions do
        create :create_it
        update :update_it
        destroy :destroy_it
      end
    end

    assert transacting_actions(ExplicitTransactionFree) == []
  end

  test "when the data layer does support transactions, it enables them on explicitly defined actions" do
    Ash.DataLayer
    |> Mimic.stub(:can?, fn
      :transact, _ -> true
      cap, resource -> call_original(Ash.DataLayer, :can?, [cap, resource])
    end)

    defmodule ExplicitTransactionFiend do
      @moduledoc false
      use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
      end

      actions do
        create :create_it
        update :update_it
        destroy :destroy_it
      end
    end

    assert transacting_actions(ExplicitTransactionFiend) == [:create_it, :destroy_it, :update_it]
  end

  test "read actions are left alone, since `transaction? true` on one is never a default" do
    defmodule TransactingRead do
      @moduledoc false
      use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
      end

      actions do
        read :read_it do
          transaction? true
        end
      end
    end

    assert transacting_actions(TransactingRead) == [:read_it]
  end
end
