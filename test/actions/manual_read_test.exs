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
      domain: Ash.Test.Actions.ManualReadTest.Domain,
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

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Author
    end
  end

  test "reading works" do
    Author
    |> Ash.Changeset.for_create(:create, %{name: "name"})
    |> Domain.create!()

    assert [] =
             Author
             |> Ash.Query.for_read(:read)
             |> Domain.read!()

    assert [_] =
             Author
             |> Ash.Query.for_read(:all)
             |> Domain.read!()
  end
end
