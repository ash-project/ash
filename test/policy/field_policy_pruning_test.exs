# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.FieldPolicyPruningTest do
  @doc false
  use ExUnit.Case

  defmodule TestResource do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :* do
        authorize_if always()
      end

      field_policy :calc do
        forbid_if always()
      end

      field_policy :calc2 do
        forbid_if always()
      end
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    calculations do
      calculate :calc2, :map do
        public? true

        calculation fn records, _ ->
          Enum.map(records, & &1.id)
        end
      end

      calculate :calc, :map do
        load :calc2

        calculation fn records, _ ->
          raise "shouldn't get here!"
        end

        public? true
      end
    end
  end

  test "field policies prune unnecessary calculations" do
    Ash.create!(TestResource, %{}, authorize?: false)

    TestResource
    |> Ash.Query.load([:calc, :calc2])
    |> Ash.read!()
  end
end
