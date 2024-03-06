defmodule Ash.Test.Policy.FieldPolicy.ExpressionConditionTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule ResourceWithMultiplePoliciesForOneField do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end

      attribute :other_name, :string do
        public?(true)
      end

      attribute :other_other_name, :string do
        public?(true)
      end
    end

    field_policies do
      field_policy :name, [actor_attribute_equals(:admin, true)] do
        authorize_if always()
      end

      field_policy :name, expr(name == ^actor(:name)) do
        authorize_if always()
      end

      field_policy :other_name, [actor_attribute_equals(:admin, true)] do
        forbid_if always()
      end

      field_policy :other_name, expr(name == ^actor(:name)) do
        forbid_if always()
      end

      field_policy :other_other_name, [actor_attribute_equals(:admin, true)] do
        authorize_if always()
      end

      field_policy :other_other_name, expr(name == ^actor(:name)) do
        forbid_if always()
      end
    end

    policies do
      policy always() do
        forbid_if never()
        authorize_if always()
      end
    end

    code_interface do
      define :create
      define :read
    end

    actions do
      default_accept :*
      defaults [:create, :read]
    end
  end

  test "multiple field policies for the same field with different conditions work" do
    ResourceWithMultiplePoliciesForOneField.create!(%{
      name: "foo",
      other_name: "foo",
      other_other_name: "foo"
    })

    ResourceWithMultiplePoliciesForOneField.create!(%{
      name: "baz",
      other_name: "baz",
      other_other_name: "bar"
    })

    assert [
             %{
               name: "baz",
               other_name: %Ash.ForbiddenField{},
               other_other_name: %Ash.ForbiddenField{}
             },
             %{name: "foo", other_name: %Ash.ForbiddenField{}, other_other_name: "foo"}
           ] =
             ResourceWithMultiplePoliciesForOneField.read!(
               actor: %{name: "baz", admin: true},
               query: ResourceWithMultiplePoliciesForOneField |> Ash.Query.sort([:name])
             )
  end
end
