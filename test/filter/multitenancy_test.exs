defmodule Ash.Test.MultitenancyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule MultiTenant do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    multitenancy do
      strategy :attribute
      attribute :owner
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :owner, :integer, primary_key?: true, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  test "reading an object doesn't require multitenancy attribute in the primary key" do
    MultiTenant
    |> Ash.Changeset.for_create(:create, %{id: 1000, owner: 1})
    |> Ash.create!(tenant: 1)

    MultiTenant
    |> Ash.get!(1000, tenant: 1)
  end

  defmodule NonMultiTenant do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :owner, :integer, primary_key?: true, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  test "reading an object without multitenancy requires attribute in the primary key" do
    NonMultiTenant
    |> Ash.Changeset.for_create(:create, %{id: 1000, owner: 1})
    |> Ash.create!()

    ExUnit.Assertions.assert_raise(Ash.Error.Invalid, fn ->
      NonMultiTenant
      |> Ash.get!(1000)
    end)

    NonMultiTenant
    |> Ash.get!(%{id: 1000, owner: 1})
  end
end
