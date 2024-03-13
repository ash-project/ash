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
      default_accept :*
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        manual ManualRead
      end

      read :all
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
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
    |> Ash.create!()

    assert [] =
             Author
             |> Ash.Query.for_read(:read)
             |> Ash.read!()

    assert [_] =
             Author
             |> Ash.Query.for_read(:all)
             |> Ash.read!()
  end
end
