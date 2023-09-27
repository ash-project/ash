defmodule Ash.Test.Policy.FieldPolicy.ExpressionConditionTest do
  use ExUnit.Case, async: true

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      allow_unregistered? true
    end
  end

  defmodule ResourceWithExprCondition do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id

      attribute :name, :string
    end

    field_policies do
      field_policy :name, [expr(name == ^actor(:name))] do
        authorize_if always()
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    code_interface do
      define_for Api

      define :create
      define :read
    end

    actions do
      defaults [:create, :read]
    end
  end

  test "expr condition forbids field if it does not match" do
    ResourceWithExprCondition.create!(%{name: "foo"})
    ResourceWithExprCondition.create!(%{name: "bar"})

    assert [
             %{name: "bar"},
             %{name: %Ash.ForbiddenField{field: :name, type: :attribute}}
           ] =
             ResourceWithExprCondition.read!(
               actor: %{name: "bar"},
               query: ResourceWithExprCondition |> Ash.Query.sort([:name])
             )
  end

  defmodule ResourceWithMultiplePoliciesForOneField do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id

      attribute :name, :string
    end

    field_policies do
      field_policy :name, [actor_attribute_equals(:admin, true)] do
        authorize_if always()
      end

      field_policy :name, expr(name == ^actor(:name)) do
        authorize_if always()
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    code_interface do
      define_for Api

      define :create
      define :read
    end

    actions do
      defaults [:create, :read]
    end
  end

  test "multiple field policies for the same field with different conditions work" do
    ResourceWithMultiplePoliciesForOneField.create!(%{name: "foo"})
    ResourceWithMultiplePoliciesForOneField.create!(%{name: "baz"})

    assert [
             %{name: "baz"},
             %{name: "foo"}
           ] =
             ResourceWithMultiplePoliciesForOneField.read!(
               actor: %{name: "baz", admin: true},
               query: ResourceWithMultiplePoliciesForOneField |> Ash.Query.sort([:name])
             )
  end

  defmodule ResourceWithMultiplePoliciesForOneFieldWithExtraCheckOptions do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id

      attribute :name, :string
    end

    field_policies do
      field_policy :name, [actor_attribute_equals(:admin, true)] do
        authorize_if always()
      end

      # I saw in the generated spark_dsl_config, that the extra check options are
      # set for expr inside the field policy, but not for conditions I think.
      # The behaviour is different if I set them here.
      field_policy :name,
                   {Ash.Policy.Check.Expression,
                    [
                      expr: expr(name == ^actor(:name)),
                      ash_field_policy?: true,
                      access_type: :filter
                    ]} do
        authorize_if always()
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    code_interface do
      define_for Api

      define :create
      define :read
    end

    actions do
      defaults [:create, :read]
    end
  end

  test "multiple field policies for the same field with different conditions work (extra check options)" do
    ResourceWithMultiplePoliciesForOneFieldWithExtraCheckOptions.create!(%{name: "foo"})
    ResourceWithMultiplePoliciesForOneFieldWithExtraCheckOptions.create!(%{name: "baz"})

    assert [
             %{name: "baz"},
             %{name: "foo"}
           ] =
             ResourceWithMultiplePoliciesForOneFieldWithExtraCheckOptions.read!(
               actor: %{name: "baz", admin: true},
               query:
                 ResourceWithMultiplePoliciesForOneFieldWithExtraCheckOptions
                 |> Ash.Query.sort([:name])
             )
  end
end
