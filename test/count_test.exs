# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true

        pagination do
          offset? true
          default_limit 5
        end
      end

      read :non_countable do
        pagination do
          offset? true
          default_limit 5
          countable false
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
      |> Ash.create!(authorize?: false)
    end)

    assert {:ok, %Ash.Page.Offset{count: count}} =
             Countable
             |> Ash.Query.for_read(:read)
             |> Ash.read(page: [count: true], authorize?: false)

    assert count == 10
  end

  test "counts only visible entries with authorization" do
    Enum.each(1..10, fn _ ->
      Countable
      |> Ash.Changeset.for_create(:create, %{owner_id: "foo"}, authorize?: false)
      |> Ash.create!()
    end)

    Enum.each(1..10, fn _ ->
      Countable
      |> Ash.Changeset.for_create(:create, %{owner_id: "bar"}, authorize?: false)
      |> Ash.create!()
    end)

    assert {:ok, %Ash.Page.Offset{count: count}} =
             Countable
             |> Ash.Query.for_read(:read)
             |> Ash.read(actor: %{id: "foo"}, page: [count: true])

    assert count == 10
  end

  test "trying to count a non-countable action is invalid" do
    assert_raise Ash.Error.Invalid,
                 ~r/cannot be counted while paginating/,
                 fn ->
                   Countable
                   |> Ash.Query.for_read(:non_countable)
                   |> Ash.read!(actor: %{id: "foo"}, page: [count: true])
                 end
  end
end
