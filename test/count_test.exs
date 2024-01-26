defmodule Ash.Test.CountTest do
  use ExUnit.Case

  defmodule Countable do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      defaults [:create, :update, :destroy]

      read :read do
        primary? true

        pagination do
          offset? true
          default_limit 5
        end
      end
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :owner_id, :string
    end

    policies do
      policy always() do
        authorize_if expr(owner_id == ^actor(:id))
      end
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      resource Countable
    end
  end

  test "counts all entries without authorization" do
    Enum.each(1..10, fn _ ->
      Countable
      |> Ash.Changeset.new(%{owner_id: "foo"})
      |> Ash.Changeset.for_create(:create)
      |> Api.create!()
    end)

    assert {:ok, %Ash.Page.Offset{count: count}} =
             Countable
             |> Ash.Query.for_read(:read)
             |> Api.read(page: [count: true])

    assert count == 10
  end

  test "counts only visible entries with authorization" do
    Enum.each(1..10, fn _ ->
      Countable
      |> Ash.Changeset.new(%{owner_id: "foo"})
      |> Ash.Changeset.for_create(:create)
      |> Api.create!()
    end)

    Enum.each(1..10, fn _ ->
      Countable
      |> Ash.Changeset.new(%{owner_id: "bar"})
      |> Ash.Changeset.for_create(:create)
      |> Api.create!()
    end)

    assert {:ok, %Ash.Page.Offset{count: count}} =
             Countable
             |> Ash.Query.for_read(:read)
             |> Api.read(actor: %{id: "foo"}, page: [count: true])

    assert count == 10
  end
end
