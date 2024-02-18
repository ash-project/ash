defmodule Ash.Test.Policy.StrictConditionTest do
  use ExUnit.Case, async: true

  defmodule Resource do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, authorizers: [Ash.Policy.Authorizer]

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
      default_access_type :strict

      policy [action(:read), expr(visible == true)] do
        authorize_if always()
      end
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

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
    |> Api.create!()

    Resource
    |> Ash.Changeset.for_create(:create, %{visible: false}, authorize?: false)
    |> Api.create!()

    assert_raise Ash.Error.Forbidden, fn ->
      Resource
      |> Ash.Query.for_read(:read, %{}, actor: %{id: "foo"})
      |> Api.read!()
    end
  end
end
