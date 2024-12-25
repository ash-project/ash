defmodule Ash.Test.Resource.Validation.ChangingTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Author do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end
  end

  defmodule Comment do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :text, :string, public?: true
    end

    relationships do
      belongs_to :post, Post, public?: true
    end
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, create: :*, update: :*]

      update :ensure_order_changing do
        validate changing(:order)
      end

      update :ensure_author_changing do
        validate changing(:author)
      end

      update :ensure_comments_changing do
        validate changing(:comments)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :order, :integer, public?: true
    end

    relationships do
      belongs_to :author, Author, public?: true
      has_many :comments, Comment, public?: true
    end
  end

  describe "Ash.Resource.Validation.Changing atomic validation" do
    test "fails if attribute is not changing" do
      post = Ash.create!(Post, %{title: "foo", order: 1})

      assert_raise Ash.Error.Invalid, ~r/must be changing/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_order_changing, %{})
      end
    end

    test "fails if attribute is being set to the same value" do
      post = Ash.create!(Post, %{title: "foo", order: 1})

      assert_raise Ash.Error.Invalid, ~r/must be changing/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_order_changing, %{order: 1})
      end
    end

    test "succeeds if attribute changing to another value" do
      post = Ash.create!(Post, %{title: "foo", order: 1})

      assert %Ash.BulkResult{status: :success} =
               Post
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.bulk_update!(:ensure_order_changing, %{order: 2})
    end

    test "fails if relationship is not changing" do
      author = Ash.create!(Author, %{name: "fred"})
      post = Ash.create!(Post, %{title: "foo", author_id: author.id})

      assert_raise Ash.Error.Invalid, ~r/must be changing/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_author_changing, %{})
      end
    end

    test "fails if relationship is being set to the same value" do
      author = Ash.create!(Author, %{name: "fred"})
      post = Ash.create!(Post, %{title: "foo", author_id: author.id})

      assert_raise Ash.Error.Invalid, ~r/must be changing/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_author_changing, %{author_id: author.id})
      end
    end

    test "succeeds if relationship is being set to another value" do
      author1 = Ash.create!(Author, %{name: "fred"})
      author2 = Ash.create!(Author, %{name: "george"})
      post = Ash.create!(Post, %{title: "foo", author_id: author1.id})

      assert %Ash.BulkResult{status: :success} =
               Post
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.bulk_update!(:ensure_author_changing, %{author_id: author2.id})
    end

    test "returns :not_atomic on has_many relationships" do
      post = Ash.create!(Post, %{title: "foo"})

      assert_raise Ash.Error.Invalid, ~r/can't atomically check/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_comments_changing, %{}, return_errors?: true)
      end
    end
  end
end
