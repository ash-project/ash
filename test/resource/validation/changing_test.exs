# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
        require_atomic? false
        validate changing(:order)
      end

      update :ensure_author_changing do
        require_atomic? false
        validate changing(:author), message: "change is inevitable!"
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

  def assert_invalid_updates(
        %resource_module{} = resource,
        action,
        input,
        message \\ "must be changing"
      ) do
    assert_raise Ash.Error.Invalid, ~r/#{message}/, fn ->
      resource
      |> Ash.Changeset.for_update(action, input)
      |> Ash.update!(atomic_upgrade?: false)
    end

    assert_raise Ash.Error.Invalid, ~r/#{message}/, fn ->
      resource_module
      |> Ash.Query.filter(id == ^resource.id)
      |> Ash.bulk_update!(action, input)
    end
  end

  def assert_valid_updates(%resource_module{} = resource, action, input) do
    assert %Ash.BulkResult{status: :success} =
             resource_module
             |> Ash.Query.filter(id == ^resource.id)
             |> Ash.bulk_update!(action, input)

    # Ensures the non-atomic version works even if the data layer has already
    # been updated.
    assert resource
           |> Ash.Changeset.for_update(action, input)
           |> Ash.update!(atomic_upgrade?: false)
  end

  describe "Ash.Resource.Validation.Changing" do
    test "fails if attribute is not changing" do
      Post
      |> Ash.create!(%{title: "foo", order: 1})
      |> assert_invalid_updates(:ensure_order_changing, %{})
    end

    test "fails if attribute is being set to the same value" do
      Post
      |> Ash.create!(%{title: "foo", order: 1})
      |> assert_invalid_updates(:ensure_order_changing, %{order: 1})
    end

    test "succeeds if attribute changing to another value" do
      Post
      |> Ash.create!(%{title: "foo", order: 1})
      |> assert_valid_updates(:ensure_order_changing, %{order: 2})
    end

    test "fails if relationship is not changing" do
      author = Ash.create!(Author, %{name: "fred"})

      Post
      |> Ash.create!(%{title: "foo", author_id: author.id})
      |> assert_invalid_updates(:ensure_author_changing, %{}, "change is inevitable!")
    end

    test "fails if relationship is being set to the same value" do
      author = Ash.create!(Author, %{name: "fred"})

      Post
      |> Ash.create!(%{title: "foo", author_id: author.id})
      |> assert_invalid_updates(
        :ensure_author_changing,
        %{author_id: author.id},
        "change is inevitable!"
      )
    end

    test "succeeds if relationship is being set to another value" do
      author1 = Ash.create!(Author, %{name: "fred"})
      author2 = Ash.create!(Author, %{name: "george"})

      Post
      |> Ash.create!(%{title: "foo", author_id: author1.id})
      |> assert_valid_updates(:ensure_author_changing, %{author_id: author2.id})
    end

    test "returns :not_atomic on has_many relationships" do
      post = Ash.create!(Post, %{title: "foo"})

      assert_raise Ash.Error.Invalid, ~r/can't atomically check/, fn ->
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:ensure_comments_changing, %{}, return_errors?: true)
      end
    end

    test "succeeds when changing from nil to a value" do
      # Create post with order = nil
      post = Ash.create!(Post, %{title: "foo"})
      assert is_nil(post.order)

      # Should succeed when changing from nil to a value
      post
      |> assert_valid_updates(:ensure_order_changing, %{order: 42})
    end

    test "absent validation with changing condition fails when setting nil field" do
      # This reproduces the exact Eden issue: absent(:outcome) with where: [changing(:outcome)]
      # should prevent setting a value on a field that was nil

      defmodule TestPost do
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        actions do
          default_accept :*
          defaults [:read, create: :*, update: :*]

          update :close_task do
            # This should fail when trying to set outcome on a task that had no outcome
            validate absent(:outcome) do
              where [changing(:outcome)]
              message "Task already closed"
            end
          end
        end

        attributes do
          uuid_primary_key :id
          attribute :title, :string, public?: true
          attribute :outcome, :string, public?: true
        end
      end

      # Create a post with outcome = nil
      post = Ash.create!(TestPost, %{title: "test"})
      assert is_nil(post.outcome)

      # Try to update outcome from nil to "closed" - this should fail
      # because absent(:outcome) with where: [changing(:outcome)] should prevent
      # setting a value when the field was previously absent (nil)
      assert_raise Ash.Error.Invalid, ~r/Task already closed/, fn ->
        TestPost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:close_task, %{outcome: "closed"})
      end
    end
  end
end
