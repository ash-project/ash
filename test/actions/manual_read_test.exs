defmodule Ash.Test.Actions.ManualReadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule ManualRead do
    use Ash.Resource.ManualRead

    def read(_query, _data_layer_query, _opts, _context) do
      {:ok, []}
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      api: Ash.Test.Actions.ManualReadTest.Api,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :update, :destroy]

      read :read do
        primary? true
        manual ManualRead
      end

      read :all
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Author
    end
  end

  test "reading works" do
    Author
    |> Ash.Changeset.for_create(:create, %{name: "name"})
    |> Api.create!()

    assert [] =
             Author
             |> Ash.Query.for_read(:read)
             |> Api.read!()

    assert [_] =
             Author
             |> Ash.Query.for_read(:all)
             |> Api.read!()
  end
end
