defmodule Ash.Test.SimpleDataLayerTest do
  use ExUnit.Case

  alias Ash.Test.AnyApi, as: Api

  defmodule Person do
    use Ash.Resource, api: Api

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    actions do
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

  test "set_data can be used in a before_action callback" do
    assert [%{name: "Fred"}] = Api.read!(Person)
  end

  test "pagination works" do
    assert %Ash.Page.Offset{results: [%{name: "Fred"}]} =
             Api.read!(Person, action: :paginated, page: [limit: 1])
  end
end
