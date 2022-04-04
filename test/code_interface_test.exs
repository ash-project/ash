defmodule Ash.Test.CodeInterfaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    code_interface do
      define_for Ash.Test.CodeInterfaceTest.Api
      define :get_user, action: :read, get?: true, args: [:id]
      define :read_users, action: :read
      define :get_by_id, action: :read, get_by: [:id]
    end

    actions do
      read :read do
        primary? true
      end

      create :create

      read :by_id do
        argument :id, :uuid, allow_nil?: false

        filter expr(id == ^arg(:id))
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :first_name, :string
      attribute :last_name, :string
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(User)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "get! raises on not found" do
    assert_raise Ash.Error.Query.NotFound, fn ->
      User.get_user!(Ash.UUID.generate())
    end
  end

  test "get_by! adds the proper arguments and filters" do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{first_name: "ted", last_name: "Danson"})
      |> Api.create!()

    assert User.get_by_id!(user.id).id == user.id
  end
end
