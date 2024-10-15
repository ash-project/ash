defmodule Ash.Test.Resource.Verifiers.VerifyTransactionsSupportedTest do
  @moduledoc false
  use ExUnit.Case, async: false
  use Mimic

  alias Spark.Error.DslError

  setup :set_mimic_global

  describe "non transacting data layers" do
    test "actions with transaction? enabled don't compile" do
      Ash.DataLayer.Simple
      |> stub(:can?, fn
        _, :inhibit_transaction_validation -> false
        resource, capability -> call_original(Ash.DataLayer.Simple, :can?, [resource, capability])
      end)

      assert_raise(DslError, ~r/transaction\? false/, fn ->
        defmodule FailBecauseCantTransact do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Simple

          attributes do
            uuid_primary_key :id
          end

          actions do
            read :read do
              primary? true
              transaction? true
            end
          end
        end
      end)
    end
  end

  describe "tranacting data layers" do
    test "actions with transaction? enabled compile file" do
      Ash.DataLayer.Simple
      |> stub(:can?, fn
        _, :inhibit_transaction_validation -> false
        _, :transact -> true
        resource, capability -> call_original(Ash.DataLayer.Simple, :can?, [resource, capability])
      end)

      defmodule SucceedBecauseCanTransact do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Simple

        attributes do
          uuid_primary_key :id
        end

        actions do
          read :read do
            primary? true
            transaction? true
          end
        end
      end
    end
  end
end
