defmodule Ash.Test.Actions.ManualReadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule ManualRead do
    use Ash.Resource.ManualRead

    def load_relationships(_, data, _, _, _) do
      send(self(), :loading_relationships)
      {:ok, data}
    end

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

  defmodule QueryableManualRead do
    use Ash.Resource.ManualRead
    alias Ash.Test.Actions.ManualReadTest.Post

    def read(query, _data_layer_query, _opts, _context) do
      results = [
        Ash.create!(Post, %{id: "0a71151f-173c-40e9-b3b1-e3f29c49483f", name: "post1"}),
        Ash.create!(Post, %{id: "4137893e-b28f-445e-9b63-e394953942e2", name: "post2"})
      ]

      Ash.Query.apply_to(query, results)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Actions.ManualReadTest.Domain

    actions do
      default_accept :*
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        manual QueryableManualRead
      end
    end

    attributes do
      uuid_primary_key :id, writable?: true

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
      resource Post
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

    assert_received :loading_relationships
  end

  test "Ash.Query.apply_to/2 can be used" do
    assert [%{name: "post1"}, %{name: "post2"}] =
             Post
             |> Ash.Query.for_read(:read)
             |> Ash.read!()

    assert [%{name: "post1"}] =
             Post
             |> Ash.Query.limit(1)
             |> Ash.read!()

    assert [%{name: "post2"}] =
             Post
             |> Ash.Query.filter(name == "post2")
             |> Ash.read!()
  end
end
