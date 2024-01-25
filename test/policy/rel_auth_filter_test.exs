defmodule Policy.RelAuthFilterTest do
  use ExUnit.Case

  defmodule Parent do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :owner_id, :string
      attribute :guest_id, :string
    end

    relationships do
      has_one :owner_only_resource, Policy.RelAuthFilterTest.OwnerOnlyResource do
        source_attribute :id
        destination_attribute :parent_id
      end
    end

    policies do
      policy always() do
        authorize_if expr(owner_id == ^actor(:id) or guest_id == ^actor(:id))
      end
    end
  end

  defmodule OwnerOnlyResource do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :parent_id, :uuid

      attribute :state, :string
    end

    relationships do
      belongs_to :owner_resource, Policy.RelAuthFilterTest.Parent do
        source_attribute :parent_id
        destination_attribute :id
      end
    end

    policies do
      policy always() do
        authorize_if expr(
                       owner_resource.owner_id == ^actor(:id) and state in ["active", "inactive"]
                     )
      end
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      resource Parent
      resource OwnerOnlyResource
    end
  end

  test "owner can see owner_only_resource" do
    parent =
      Parent
      |> Ash.Changeset.new(%{owner_id: "owner", guest_id: "guest"})
      |> Ash.Changeset.for_create(:create)
      |> Api.create!()

    OwnerOnlyResource
    |> Ash.Changeset.new(%{parent_id: parent.id, state: "active"})
    |> Ash.Changeset.for_create(:create)
    |> Api.create!()

    assert {:ok, parent} =
             Parent
             |> Ash.Query.for_read(:read)
             |> Ash.Query.load(:owner_only_resource)
             |> Ash.Query.limit(1)
             |> Api.read_one(actor: %{id: "owner"})
             |> IO.inspect()

    refute is_nil(parent.owner_only_resource)
  end

  test "guest is forbidden from querying if loading forbidden rel" do
    parent =
      Parent
      |> Ash.Changeset.new(%{owner_id: "owner", guest_id: "guest"})
      |> Ash.Changeset.for_create(:create)
      |> Api.create!()

    OwnerOnlyResource
    |> Ash.Changeset.new(%{parent_id: parent.id, state: "active"})
    |> Ash.Changeset.for_create(:create)
    |> Api.create!()

    assert {:error, %Ash.Error.Forbidden{}} =
             Parent
             |> Ash.Query.for_read(:read)
             |> Ash.Query.load(:owner_only_resource)
             |> Ash.Query.limit(1)
             |> Api.read_one(actor: %{id: "guest"})
             |> IO.inspect()
  end
end
