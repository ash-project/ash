alias Ash.Test.LoadIgnoranceTest, as: ThisTest
alias Ash.Changeset

defmodule ThisTest.Post do
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.DataLayer.Ets

  actions do
    defaults [:read, create: :*]

    read :read_with_comment_count do
      prepare build(load: [:comment_count])
    end

    update :append_comment do
      require_atomic? false
      argument :comment, :map, allow_nil?: false
      change manage_relationship(:comment, :comments, type: :create)
    end
  end

  preparations do
    prepare build(load: [:has_comment])
  end

  attributes do
    uuid_v7_primary_key :id
    attribute :content, :string, allow_nil?: false, public?: true
  end

  relationships do
    has_many :comments, ThisTest.Comment
  end

  aggregates do
    exists :has_comment, :comments
    count :comment_count, :comments
  end
end

defmodule ThisTest.Comment do
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.DataLayer.Ets

  actions do
    defaults [:read, create: :*]
  end

  attributes do
    uuid_v7_primary_key :id
    attribute :content, :string, allow_nil?: false, public?: true
  end

  relationships do
    belongs_to :post, ThisTest.Post
  end
end

defmodule ThisTest do
  use ExUnit.Case, async: true

  test "The Ash.load function ignores global and action-specific loads" do
    params = %{content: "post"}
    post = Changeset.for_create(ThisTest.Post, :create, params) |> Ash.create!()

    params = %{comment: %{content: "comment"}}

    for _ <- 1..3 do
      Changeset.for_update(post, :append_comment, params) |> Ash.update!()
    end

    # Not loaded
    assert %Ash.NotLoaded{} = post.comments
    post = Ash.load!(post, [comments: []], action: :read_with_comment_count)
    # Loaded
    assert [_, _, _] = post.comments

    # Global load ignored
    assert %Ash.NotLoaded{} = post.comment_count
    # Action load ignored
    assert %Ash.NotLoaded{} = post.has_comment
  end
end
