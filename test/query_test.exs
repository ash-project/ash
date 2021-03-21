defmodule Ash.Test.QueryTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :read do
        primary? true
      end

      read :by_id do
        argument :id, :uuid, allow_nil?: false

        filter expr(id == ^arg(:id))
      end

      create :create
      update :update
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      resource User
    end
  end

  describe "read argument validation" do
    test "it returns an appropriate error when an argument is invalid" do
      query = Ash.Query.for_read(User, :by_id, %{id: "foobar"})
      assert [%Ash.Error.Query.InvalidArgument{field: :id}] = query.errors
    end
  end
end
