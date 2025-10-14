# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
      {:ok, [], %{full_count: 0}}
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
        pagination offset?: true, required?: false
      end

      read :all
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      has_one :self, __MODULE__, source_attribute: :id, destination_attribute: :id
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

      case Ash.Query.apply_to(query, results) do
        {:ok, results} ->
          if query.page[:count] do
            {:ok, results, %{full_count: 2}}
          else
            {:ok, results}
          end

        other ->
          other
      end
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
        pagination offset?: true, required?: false
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
             |> Ash.Query.load(:self)
             |> Ash.read!()

    assert_received :loading_relationships

    assert [_] =
             Author
             |> Ash.Query.for_read(:all)
             |> Ash.read!()
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

  test "the action can return the full conut for pagination" do
    assert %{results: [_], count: 2} =
             Post
             |> Ash.Query.for_read(:read)
             |> Ash.Query.page(limit: 1, count: true)
             |> Ash.read!()

    assert %{results: [_], count: nil} =
             Post
             |> Ash.Query.for_read(:read)
             |> Ash.Query.page(limit: 1)
             |> Ash.read!()
  end
end
