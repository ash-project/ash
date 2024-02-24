defmodule Ash.Test.Policy.FilterConditionTest do
  use ExUnit.Case, async: true

  defmodule Resource do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.FilterConditionTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      defaults([:create, :read, :update, :destroy])
    end

    attributes do
      uuid_primary_key :id
      attribute :visible, :boolean, allow_nil?: false
    end

    policies do
      default_access_type :filter

      policy [action(:read), expr(visible == true)] do
        authorize_if always()
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    authorization do
      authorize :by_default
    end

    resources do
      resource Resource
    end
  end

  test "condition in filter policy is evaluated" do
    Resource
    |> Ash.Changeset.for_create(:create, %{visible: true}, authorize?: false)
    |> Domain.create!()

    Resource
    |> Ash.Changeset.for_create(:create, %{visible: false}, authorize?: false)
    |> Domain.create!()

    [visible_resource] =
      Resource
      |> Ash.Query.for_read(:read, %{}, actor: %{id: "foo"})
      |> Domain.read!()

    assert visible_resource.visible == true
  end
end
