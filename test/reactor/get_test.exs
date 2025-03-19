defmodule Ash.Test.ReactorGetTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias __MODULE__, as: Self
  alias Ash.Test.Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    code_interface do
      define :create
    end
  end

  defmodule SimpleGetReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    input(:post_id)

    get :post, Self.Post, :read do
      by([:id])
      inputs(%{id: input(:post_id)})
    end

    return :post
  end

  test "it automatically filters the records by the supplied keys" do
    posts =
      ~w[Marty Doc Einstein]
      |> Enum.map(&Post.create!(%{title: &1}))

    post_id =
      posts
      |> Enum.random()
      |> Map.fetch!(:id)

    assert {:ok, %{id: ^post_id}} =
             Reactor.run(SimpleGetReactor, %{post_id: post_id}, %{}, async?: false)
  end
end
