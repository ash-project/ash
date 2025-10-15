# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SimpleDataLayerTest do
  use ExUnit.Case

  alias Ash.Test.Domain, as: Domain

  defmodule Person do
    use Ash.Resource, domain: Domain

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*

      read :read do
        primary? true

        prepare fn query, _ ->
          Ash.Query.before_action(query, fn query ->
            query
            |> Ash.DataLayer.Simple.set_data([
              struct(__MODULE__, %{id: Ash.UUID.generate(), name: "Fred"})
            ])
          end)
        end
      end

      read :paginated do
        pagination offset?: true

        prepare fn query, _ ->
          Ash.Query.before_action(query, fn query ->
            query
            |> Ash.DataLayer.Simple.set_data(
              [
                struct(__MODULE__, %{id: Ash.UUID.generate(), name: "Fred"}),
                struct(__MODULE__, %{id: Ash.UUID.generate(), name: "Fred"}),
                struct(__MODULE__, %{id: Ash.UUID.generate(), name: "Fred"})
              ]
              |> offset(query.offset)
              |> limit(query.limit)
            )
          end)
        end
      end
    end

    defp limit(list, nil), do: list
    defp limit(list, value), do: Enum.take(list, value)

    defp offset(list, nil), do: list
    defp offset(list, value), do: Enum.drop(list, value)
  end

  defmodule Bob do
    use Ash.Resource, domain: Domain

    actions do
      defaults([:read])

      create :create_with_name do
        accept([:name])
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string)
    end

    calculations do
      calculate :check, :string do
        load([:name])

        calculation(fn records, _ ->
          Enum.map(records, fn record -> record.name end)
        end)
      end
    end
  end

  test "set_data can be used in a before_action callback" do
    assert [%{name: "Fred"}] = Ash.read!(Person)
  end

  test "pagination works" do
    assert %Ash.Page.Offset{results: [%{name: "Fred"}]} =
             Ash.read!(Person, action: :paginated, page: [limit: 1])
  end

  test "can reselect attributes as necessary" do
    # this will fail - calculation will say that cannot convert NotLoaded to string
    Bob
    |> Ash.Changeset.for_create(:create_with_name, %{name: "Boby"})
    |> Ash.Changeset.select(:id)
    |> Ash.Changeset.load(:check)
    |> Ash.create!()
  end
end
