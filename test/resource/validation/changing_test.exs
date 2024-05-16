defmodule Ash.Test.Resource.Validation.ChangingTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Author do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end
  end

  defmodule Comment do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :text, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :post, Post
    end
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :ensure_order_changing do
        accept([:order])

        validate(changing(:order))
      end

      update :ensure_author_changing do
        accept([:author_id])

        validate(changing(:author))
      end

      update :ensure_comments_changing do
        validate(changing(:comments))
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :order, :integer do
        public?(true)
      end
    end

    relationships do
      belongs_to :author, Author, public?: true
      has_many :comments, Comment, public?: true
    end
  end

  describe "Ash.Resource.Validation.Changing" do
    test "fails atomic validation if attribute is not changing" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", order: 1})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/must be changing/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_order_changing, %{})
      end
    end

    test "fails atomic validation if attribute is being set to the same value" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", order: 1})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/must be changing/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_order_changing, %{order: 1})
      end
    end

    test "succeeds atomic validation if attribute changing to another value" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", order: 1})
        |> Ash.create!()

      assert %Ash.BulkResult{status: :success} =
               Post
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.bulk_update!(:ensure_order_changing, %{order: 2})
    end

    test "fails atomic validation if relationship is not changing" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", author_id: author.id})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/must be changing/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_author_changing, %{})
      end
    end

    test "fails atomic validation if relationship is being set to the same value" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", author_id: author.id})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/must be changing/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_author_changing, %{author_id: author.id})
      end
    end

    test "succeeds atomic validation if relationship is being set to another value" do
      author1 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "fred"})
        |> Ash.create!()

      author2 =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "george"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", author_id: author1.id})
        |> Ash.create!()

      assert %Ash.BulkResult{status: :success} =
               Post
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.bulk_update!(:ensure_author_changing, %{author_id: author2.id})
    end

    test "returns not_atomic on has_many relationships" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/can't atomically check/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_comments_changing, %{}, return_errors?: true)
      end
    end
  end
end
