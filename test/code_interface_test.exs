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
      define :get_user, action: :read, get?: true, args: [:id]
      define :read_users, action: :read
    end

    actions do
      read :read do
        primary? true
      end

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

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(User)
    end
  end

  test "get! raises on not found" do
    assert_raise Ash.Error.Query.NotFound, fn ->
      Api.get_user!(Ash.UUID.generate())
    end
  end
end
