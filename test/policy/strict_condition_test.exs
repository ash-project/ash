defmodule Ash.Test.Policy.StrictConditionTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Resource do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:create, :read, :update, :destroy])
    end

    attributes do
      uuid_primary_key :id
      attribute :visible, :boolean, allow_nil?: false, public?: true
    end

    policies do
      default_access_type :strict

      policy [action(:read), expr(visible == true)] do
        authorize_if always()
      end
    end
  end

  test "condition in filter policy is evaluated" do
    Resource
    |> Ash.Changeset.for_create(:create, %{visible: true}, authorize?: false)
    |> Ash.create!()

    Resource
    |> Ash.Changeset.for_create(:create, %{visible: false}, authorize?: false)
    |> Ash.create!()

    assert_raise Ash.Error.Forbidden, fn ->
      Resource
      |> Ash.Query.for_read(:read, %{}, actor: %{id: "foo"})
      |> Ash.read!()
    end
  end
end
