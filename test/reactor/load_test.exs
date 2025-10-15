# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ReactorLoadTest do
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

    relationships do
      has_many :comments, Self.Comment, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    code_interface do
      define :create
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :comment, :string, allow_nil?: false, public?: true
    end

    relationships do
      belongs_to :post, Self.Post, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    code_interface do
      define :create
    end
  end

  defmodule SimpleLoadReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    ash do
      default_domain(Domain)
    end

    input :post

    load(:post_with_comments, input(:post), value(comments: :comment))
  end

  test "it performs loading" do
    post = Post.create!(%{title: "Marty"})
    comments = ["This is heavy", "You made a time machine... out of a Delorean?"]

    for comment <- comments do
      Comment.create!(%{post_id: post.id, comment: comment})
    end

    assert {:ok, post} = Reactor.run(SimpleLoadReactor, %{post: post}, %{}, async?: false)
    assert Enum.sort(Enum.map(post.comments, & &1.comment)) == comments
  end
end
