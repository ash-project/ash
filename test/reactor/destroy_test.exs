defmodule Ash.Test.ReactorDestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false
      attribute :sub_title, :string
      attribute :published, :boolean, default: false
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    code_interface do
      define_for Ash.Test.ReactorDestroyTest.Api
      define :create
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Post
    end
  end

  test "it can destroy a post" do
    defmodule SimpleDestroyPostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_api Api
      end

      input :post

      destroy :delete_post, Post, :destroy do
        initial(input(:post))
      end
    end

    {:ok, original_post} =
      Post.create(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, :ok} =
             Reactor.run(SimpleDestroyPostReactor, %{post: original_post}, %{}, async?: false)
  end

  test "it can destroy and return a post" do
    defmodule ReturningDestroyPostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_api Api
      end

      input :post

      destroy :delete_post, Post, :destroy do
        initial(input(:post))
        return_destroyed?(true)
      end
    end

    {:ok, original_post} =
      Post.create(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, post} =
             Reactor.run(ReturningDestroyPostReactor, %{post: original_post}, %{}, async?: false)

    assert original_post.__struct__ == post.__struct__
    assert original_post.id == post.id
    assert post.__meta__.state == :deleted
  end
end
