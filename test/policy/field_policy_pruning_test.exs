defmodule Ash.Test.Policy.FieldPolicyTest do
  @doc false
  use ExUnit.Case

  defmodule App.Core.TestResource do
    use Ash.Resource,
      domain: App.Core,
      authorizers: [Ash.Policy.Authorizer]

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :* do
        authorize_if always()
      end

      field_policy :graphs do
        forbid_if always()
      end
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      read :read do
        primary? true
      end
    end

    calculations do
      calculate :graphs, :map do
        calculation fn records, _ ->
          raise "shouldn't get here!"
        end

        public? true
      end
    end
  end

  test "field policies prune unnecessary calculations" do
  end
end
