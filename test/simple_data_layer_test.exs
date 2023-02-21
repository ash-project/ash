defmodule Ash.Test.SimpleDataLayerTest do
  use ExUnit.Case

  defmodule Person do
    use Ash.Resource

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
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      allow_unregistered? true
    end
  end

  test "set_data can be used in a before_action callback" do
    assert [%{name: "Fred"}] = Api.read!(Person)
  end
end
