defmodule Ash.Test.QueryTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.AnyApi, as: Api

  defmodule User do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

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
      attribute :email, :string, sensitive?: true
    end

    relationships do
      belongs_to :best_friend, __MODULE__
    end
  end

  describe "read argument validation" do
    test "it returns an appropriate error when an argument is invalid" do
      query = Ash.Query.for_read(User, :by_id, %{id: "foobar"})
      assert [%Ash.Error.Query.InvalidArgument{field: :id}] = query.errors
    end
  end

  describe "sensitive attributes" do
    test "it hides the value in filters" do
      equals = Ash.Query.filter(User, email == "fred") |> inspect()
      refute equals =~ "fred"
      assert equals =~ "**redacted**"
      contains = Ash.Query.filter(User, contains(email, "fred")) |> inspect()
      refute contains =~ "fred"
      assert contains =~ "**redacted**"

      concat = Ash.Query.filter(User, email <> "-fred" == "a@b.com-fred") |> inspect()

      refute concat =~ "fred"
      assert concat =~ "**redacted**"
    end
  end

  describe "loading?" do
    test "it detects a simple load" do
      assert User
             |> Ash.Query.load(:best_friend)
             |> Ash.Query.loading?(:best_friend)
    end
  end
end
