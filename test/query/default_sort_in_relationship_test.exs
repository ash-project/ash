# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Query.DefaultSortInRelationshipTest do
  use ExUnit.Case, async: true

  defmodule Post do
    use Ash.Resource,
      domain: Ash.Test.Query.DefaultSortInRelationshipTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      primary_read_warning?: false

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :published_at, :utc_datetime
      attribute :author_id, :uuid
    end

    actions do
      defaults create: [:title, :published_at, :author_id]

      read :read do
        primary? true
        # sorted by published_at desc
        prepare build(default_sort: [published_at: :desc])
      end
    end
  end

  defmodule Comment do
    use Ash.Resource,
      domain: Ash.Test.Query.DefaultSortInRelationshipTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      primary_read_warning?: false

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :body, :string
      attribute :created_at, :utc_datetime
    end

    relationships do
      belongs_to :post, Post
    end

    actions do
      defaults create: [:body, :created_at, :post_id]

      read :read do
        primary? true
        # sorted by created_at desc
        prepare build(default_sort: [created_at: :desc])
      end
    end
  end

  defmodule Author do
    use Ash.Resource,
      domain: Ash.Test.Query.DefaultSortInRelationshipTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      primary_read_warning?: false

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :posts, Post do
        default_sort(published_at: :desc)
      end
    end

    actions do
      defaults create: [:name]

      read :read do
        primary? true
        # sorted by name asc
        prepare build(default_sort: [name: :asc])
      end
    end
  end

  defmodule Domain do
    use Ash.Domain

    resources do
      resource Post
      resource Comment
      resource Author
    end
  end

  alias Ash.Test.Query.DefaultSortInRelationshipTest.{Author, Comment, Post}

  describe "default_sort in relationship" do
    test "applies default sort when loading relationship" do
      # Create an author
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      # Create posts with different published dates
      {:ok, post1} =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "002-post-older-published",
          published_at: ~U[2023-01-01 00:00:00Z],
          author_id: author.id
        })
        |> Ash.create()

      {:ok, post2} =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "001-post-newer-published",
          published_at: ~U[2023-02-01 00:00:00Z],
          author_id: author.id
        })
        |> Ash.create()

      # Load the author with posts
      {:ok, author_with_posts} =
        Author
        |> Ash.get!(author.id)
        |> Ash.load(:posts)

      # The posts should be sorted by published_at desc
      assert length(author_with_posts.posts) == 2
      [first_post, second_post] = author_with_posts.posts

      # Newer post should be first due to default_sort
      assert first_post.id == post2.id
      assert second_post.id == post1.id
    end

    test "explicit sort overrides default_sort" do
      # Create an author
      {:ok, author} =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create()

      # Create posts with different published dates
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "Older Post",
          published_at: ~U[2023-01-01 00:00:00Z],
          author_id: author.id
        })
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "Newer Post",
          published_at: ~U[2023-02-01 00:00:00Z],
          author_id: author.id
        })
        |> Ash.create!()

      # Load the author with posts, explicitly sorting by title
      author_with_posts =
        Author
        |> Ash.get!(author.id)
        |> Ash.load!(posts: Ash.Query.sort(Post, :title))

      # The posts should be sorted by title asc
      assert length(author_with_posts.posts) == 2
      [first_post, second_post] = author_with_posts.posts

      # "Newer Post" should be first alphabetically
      assert first_post.id == post2.id
      assert second_post.id == post1.id
    end
  end
end
