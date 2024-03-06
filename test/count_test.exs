defmodule Ash.Test.CountTest do
  use ExUnit.Case

  alias Ash.Test.Domain, as: Domain

  defmodule Countable do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept :*
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

      attribute :owner_id, :string do
        public?(true)
      end
    end

    policies do
      policy always() do
        authorize_if expr(owner_id == ^actor(:id))
      end
    end
  end

  test "counts all entries without authorization" do
    Enum.each(1..10, fn _ ->
      Countable
      |> Ash.Changeset.for_create(:create, %{owner_id: "foo"})
      |> Domain.create!(authorize?: false)
    end)

    assert {:ok, %Ash.Page.Offset{count: count}} =
             Countable
             |> Ash.Query.for_read(:read)
             |> Domain.read(page: [count: true], authorize?: false)

    assert count == 10
  end

  test "counts only visible entries with authorization" do
    Enum.each(1..10, fn _ ->
      Countable
      |> Ash.Changeset.for_create(:create, %{owner_id: "foo"}, authorize?: false)
      |> Domain.create!()
    end)

    Enum.each(1..10, fn _ ->
      Countable
      |> Ash.Changeset.for_create(:create, %{owner_id: "bar"}, authorize?: false)
      |> Domain.create!()
    end)

    assert {:ok, %Ash.Page.Offset{count: count}} =
             Countable
             |> Ash.Query.for_read(:read)
             |> Domain.read(actor: %{id: "foo"}, page: [count: true])

    assert count == 10
  end
end
