defmodule Ash.Test.ReactorReadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, api: Ash.Test.AnyApi

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false
    end

    actions do
      defaults [:create, :read]
    end

    code_interface do
      define :create
    end
  end

  defmodule SimpleReadReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    ash do
      default_api Ash.Test.AnyApi
    end

    read :read_posts, Ash.Test.ReactorReadTest.Post
  end

  test "when a posts exist it returns them" do
    expected =
      ~w[Marty Doc Einstein]
      |> Enum.map(&Post.create!(%{title: &1}))

    {:ok, actual} = Reactor.run(SimpleReadReactor, %{}, %{}, async?: false)

    assert actual |> Enum.map(& &1.id) |> Enum.sort() ==
             expected |> Enum.map(& &1.id) |> Enum.sort()
  end

  test "when posts don't exist it returns an empty list" do
    assert {:ok, []} = Reactor.run(SimpleReadReactor, %{}, %{}, async?: false)
  end
end
