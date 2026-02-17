# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.AtomicSetTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  require Ash.Expr

  alias Ash.Test.Domain, as: Domain

  # Custom change that returns {:atomic_set, ...}
  defmodule SetCounterChange do
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context) do
      Ash.Changeset.force_change_attribute(changeset, :counter, 77)
    end

    @impl true
    def atomic(_changeset, _opts, _context) do
      {:atomic_set, %{counter: Ash.Expr.expr(77)}}
    end
  end

  # Custom change that returns a list with both {:atomic, ...} and {:atomic_set, ...}
  defmodule MixedAtomicsChange do
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context) do
      changeset
      |> Ash.Changeset.force_change_attribute(:counter, 99)
      |> Ash.Changeset.force_change_attribute(:title, "Modified Title")
    end

    @impl true
    def atomic(_changeset, _opts, _context) do
      [
        {:atomic_set, %{counter: Ash.Expr.expr(99)}},
        {:atomic, %{title: Ash.Expr.expr("Modified Title")}}
      ]
    end
  end

  defmodule RequiredFieldPost do
    @moduledoc "Resource for testing allow_nil? protection with atomic_set"
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy]

      create :create do
        accept [:title, :required_field]
      end

      create :create_with_nilable_atomic do
        accept [:title]
        argument :value, :string, public?: true

        change atomic_set(:required_field, expr(^arg(:value)))
      end

      create :create_with_safe_atomic do
        accept [:title]

        change atomic_set(:required_field, expr("safe_value"))
      end

      create :create_with_conditional_nil do
        accept [:title]
        argument :condition, :boolean, public?: true

        # This expression can return nil at runtime
        change atomic_set(
                 :required_field,
                 expr(
                   if ^arg(:condition) do
                     "value"
                   else
                     nil
                   end
                 )
               )
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :required_field, :string do
        public?(true)
        allow_nil?(false)
      end
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :body, :string, public?: true
      attribute :post_id, :uuid, public?: true
    end

    relationships do
      belongs_to :post, Post
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, update: :*]

      create :create do
        accept [:title, :counter]
      end

      create :create_with_atomic_set do
        accept [:title]
        change atomic_set(:counter, expr(10))
      end

      create :create_with_now do
        accept [:title]
        change atomic_set(:created_at, expr(now()))
      end

      create :create_with_custom_change do
        accept [:title]
        change SetCounterChange
      end

      create :create_with_mixed_atomics do
        accept [:title]
        change MixedAtomicsChange
      end

      update :update_with_mixed_atomics do
        accept [:title]
        change MixedAtomicsChange
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :counter, :integer do
        public?(true)
        default 0
      end

      attribute :created_at, :utc_datetime_usec do
        public?(true)
      end
    end

    aggregates do
      count :comment_count, :comments
    end

    relationships do
      has_many :comments, Comment
    end

    code_interface do
      define :create_with_atomic_set, args: [:title]
      define :create_with_now, args: [:title]
      define :create_with_custom_change, args: [:title]
      define :create_with_mixed_atomics, args: [:title]
      define :update_with_mixed_atomics
    end
  end

  describe "atomic_set/3 in actions" do
    test "atomic_set sets value during create" do
      {:ok, post} = Post.create_with_atomic_set("Test Post")

      assert post.title == "Test Post"
      assert post.counter == 10
    end

    test "atomic_set with now() sets timestamp" do
      before = DateTime.utc_now()
      {:ok, post} = Post.create_with_now("Test Post")
      after_time = DateTime.utc_now()

      assert post.title == "Test Post"
      assert post.created_at != nil
      assert DateTime.compare(post.created_at, before) in [:gt, :eq]
      assert DateTime.compare(post.created_at, after_time) in [:lt, :eq]
    end
  end

  describe "Ash.Changeset.atomic_set/3" do
    test "can be called directly on changeset" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Test"})
        |> Ash.Changeset.atomic_set(:counter, Ash.Expr.expr(42))

      {:ok, post} = Ash.create(changeset)

      assert post.counter == 42
    end

    test "can set multiple values" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Test"})
        |> Ash.Changeset.atomic_set(%{
          counter: Ash.Expr.expr(100)
        })

      {:ok, post} = Ash.create(changeset)

      assert post.counter == 100
    end
  end

  describe "atomic_update validation on create" do
    test "atomic_update on create returns error" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Test"})
        |> Ash.Changeset.atomic_update(:counter, Ash.Expr.expr(counter + 1))

      assert {:error, %Ash.Error.Changes.InvalidChanges{} = error} = Ash.create(changeset)
      assert error.message =~ "atomic_update cannot be used on create actions"
    end
  end

  describe "{:atomic_set, ...} return value from atomic/3 callback" do
    test "custom change can return {:atomic_set, atomics}" do
      {:ok, post} = Post.create_with_custom_change("Test")

      assert post.title == "Test"
      assert post.counter == 77
    end

    test "custom change can return list with both {:atomic, ...} and {:atomic_set, ...}" do
      {:ok, post} = Post.create_with_mixed_atomics("Test")

      assert post.counter == 99
      assert post.title == "Modified Title"
    end

    test "mixed atomics on update uses both atomic and atomic_set" do
      {:ok, post} = Post.create_with_atomic_set("Original Title")
      {:ok, updated} = Post.update_with_mixed_atomics(post)

      assert updated.title == "Modified Title"
      assert updated.counter == 99
    end
  end

  describe "atomic_ref in create actions" do
    test "atomic_ref resolves to nil when field has no value" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Test"})
        |> Ash.Changeset.atomic_set(
          :counter,
          Ash.Expr.expr((^Ash.Expr.atomic_ref(:counter) || 0) + 5)
        )

      {:ok, post} = Ash.create(changeset)

      # atomic_ref(:counter) resolves to nil, so (nil || 0) + 5 = 5
      assert post.counter == 5
    end

    test "atomic_ref resolves to current attribute value" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Test", counter: 10})
        |> Ash.Changeset.atomic_set(:counter, Ash.Expr.expr(^Ash.Expr.atomic_ref(:counter) + 5))

      {:ok, post} = Ash.create(changeset)

      # atomic_ref(:counter) resolves to 10 (the attribute value), so 10 + 5 = 15
      assert post.counter == 15
    end
  end

  describe "bulk create with atomic_set" do
    test "bulk_create applies atomic_set to all records" do
      inputs = [
        %{title: "Post 1"},
        %{title: "Post 2"},
        %{title: "Post 3"}
      ]

      assert %Ash.BulkResult{status: :success, records: records} =
               Ash.bulk_create(inputs, Post, :create_with_atomic_set, return_records?: true)

      assert length(records) == 3

      for record <- records do
        assert record.counter == 10
      end
    end

    test "bulk_create with custom change returning {:atomic_set, ...}" do
      inputs = [
        %{title: "Post 1"},
        %{title: "Post 2"}
      ]

      assert %Ash.BulkResult{status: :success, records: records} =
               Ash.bulk_create(inputs, Post, :create_with_custom_change, return_records?: true)

      assert length(records) == 2

      for record <- records do
        assert record.counter == 77
      end
    end
  end

  describe "allow_nil? protection for create_atomics" do
    # These tests verify that atomic_set properly enforces allow_nil?: false constraints
    # on create actions, similar to how handle_allow_nil_atomics works for updates.
    # The protection should wrap expressions that can_return_nil? with error checks.

    test "atomic_set with eagerly-evaluated nil expression raises error for required attribute" do
      # When arg(:value) is nil, the expression resolves to nil during hydration
      assert {:error, error} =
               RequiredFieldPost
               |> Ash.Changeset.for_create(:create_with_nilable_atomic, %{
                 title: "Test",
                 value: nil
               })
               |> Ash.create()

      assert Exception.message(error) =~ "required"
    end

    test "atomic_set with expression that cannot return nil succeeds for required attribute" do
      # Literal expressions like expr("safe_value") cannot return nil
      {:ok, post} =
        RequiredFieldPost
        |> Ash.Changeset.for_create(:create_with_safe_atomic, %{title: "Test"})
        |> Ash.create()

      assert post.required_field == "safe_value"
    end

    test "atomic_set with eagerly-evaluated non-nil expression succeeds for required attribute" do
      # When arg(:value) is provided, the expression resolves to that value during hydration
      {:ok, post} =
        RequiredFieldPost
        |> Ash.Changeset.for_create(:create_with_nilable_atomic, %{
          title: "Test",
          value: "provided_value"
        })
        |> Ash.create()

      assert post.required_field == "provided_value"
    end

    test "bulk_create with atomic_set nil expression raises error for required attribute" do
      inputs = [
        %{title: "Post 1", value: nil},
        %{title: "Post 2", value: "valid"}
      ]

      assert %Ash.BulkResult{status: :error, errors: errors} =
               Ash.bulk_create(inputs, RequiredFieldPost, :create_with_nilable_atomic,
                 return_errors?: true,
                 return_records?: true
               )

      assert length(errors) > 0
    end

    test "bulk_create with atomic_set non-nil expressions succeeds" do
      inputs = [
        %{title: "Post 1", value: "value1"},
        %{title: "Post 2", value: "value2"}
      ]

      assert %Ash.BulkResult{status: :success, records: records} =
               Ash.bulk_create(inputs, RequiredFieldPost, :create_with_nilable_atomic,
                 return_records?: true
               )

      assert length(records) == 2

      for record <- records do
        assert record.required_field in ["value1", "value2"]
      end
    end

    test "atomic_set with runtime expression that evaluates to nil raises error" do
      assert {:error, error} =
               RequiredFieldPost
               |> Ash.Changeset.for_create(:create_with_conditional_nil, %{
                 title: "Test",
                 condition: false
               })
               |> Ash.create()

      assert Exception.message(error) =~ "required"
    end

    test "atomic_set with runtime expression that evaluates to non-nil succeeds" do
      {:ok, post} =
        RequiredFieldPost
        |> Ash.Changeset.for_create(:create_with_conditional_nil, %{
          title: "Test",
          condition: true
        })
        |> Ash.create()

      assert post.required_field == "value"
    end

    test "bulk_create with runtime conditional expression that evaluates to nil fails" do
      inputs = [
        %{title: "Post 1", condition: false}
      ]

      result =
        Ash.bulk_create(inputs, RequiredFieldPost, :create_with_conditional_nil,
          return_errors?: true,
          return_records?: true
        )

      assert result.status == :error
      assert length(result.errors) > 0
    end

    test "bulk_create with runtime conditional expression that evaluates to non-nil succeeds" do
      inputs = [
        %{title: "Post 1", condition: true},
        %{title: "Post 2", condition: true}
      ]

      result =
        Ash.bulk_create(inputs, RequiredFieldPost, :create_with_conditional_nil,
          return_errors?: true,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 2

      for record <- result.records do
        assert record.required_field == "value"
      end
    end
  end

  describe "atomic_set validation for create actions" do
    test "atomic_set rejects refs with relationship_path" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Test"})
        |> Ash.Changeset.atomic_set(:counter, Ash.Expr.expr(comments.body))

      assert changeset.valid? == false
      assert [error] = changeset.errors
      assert error.error =~ "atomic_set cannot reference related fields"
    end

    test "atomic_set rejects exists with relationships" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Test"})
        |> Ash.Changeset.atomic_set(
          :counter,
          Ash.Expr.expr(
            if exists(comments, body == "test") do
              1
            else
              0
            end
          )
        )

      assert changeset.valid? == false
      assert [error] = changeset.errors
      assert error.error =~ "atomic_set cannot use exists with relationships"
    end

    test "atomic_set rejects related aggregates" do
      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Test"})
        |> Ash.Changeset.atomic_set(:counter, Ash.Expr.expr(comment_count))

      assert changeset.valid? == false
      assert [error] = changeset.errors
      assert error.error =~ "atomic_set cannot reference related aggregates"
    end
  end
end
