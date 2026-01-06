# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkDestroyAfterTransactionTest do
  @moduledoc """
  Tests for after_transaction hooks in bulk destroy operations.

  Note: Uses `async: false` because of Mnesia.

  Tests are organized by strategy:
  - :atomic strategy
  - :atomic_batches strategy
  - :stream strategy
  - :stream strategy with Mnesia
  - soft destroy
  - manual actions
  - forbidden errors
  - validation errors
  """
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  # Shared change modules (alphabetically ordered)
  alias Ash.Test.BulkAfterTransaction.AfterActionFailsWithAfterTransaction
  alias Ash.Test.BulkAfterTransaction.AfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.AfterTransactionConvertsErrorToSuccess
  alias Ash.Test.BulkAfterTransaction.AfterTransactionFailsForSomeRecords
  alias Ash.Test.BulkAfterTransaction.AfterTransactionHandlingErrors
  alias Ash.Test.BulkAfterTransaction.AfterTransactionModifiesError
  alias Ash.Test.BulkAfterTransaction.AfterTransactionRaisesException
  alias Ash.Test.BulkAfterTransaction.AfterTransactionReturnsError
  alias Ash.Test.BulkAfterTransaction.AfterTransactionWithStopOnError
  alias Ash.Test.BulkAfterTransaction.ConditionalAfterActionErrorWithAfterTransaction
  alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionConvertsErrorChange
  alias Ash.Test.BulkAfterTransaction.MnesiaAfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.MultipleAfterTransactionHooks
  alias Ash.Test.BulkAfterTransaction.Notifier

  defmodule ConditionalValidation do
    @moduledoc """
    Validation that fails only for records with "fail" in title.
    Used to test mixed success/failure scenarios with validations.
    """
    use Ash.Resource.Validation

    @impl true
    def validate(changeset, _opts, _context) do
      title = (changeset.data && Map.get(changeset.data, :title)) || ""

      if String.contains?(title, "fail") do
        {:error, field: :title, message: "conditional validation failed"}
      else
        :ok
      end
    end

    @impl true
    def atomic(_changeset, _opts, _context) do
      {:atomic, [:title], expr(contains(title, "fail")),
       expr(
         error(Ash.Error.Changes.InvalidAttribute, %{
           field: :title,
           message: "conditional validation failed"
         })
       )}
    end
  end

  defmodule SoftDestroyWithAfterTransaction do
    @moduledoc """
    Change module that adds after_transaction hooks for soft destroy actions.
    Verifies the record was updated (not deleted) by checking attributes.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, {:ok, result} ->
        send(self(), {:after_transaction_soft_destroy_called, result.id, result.title2})
        {:ok, result}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      has_many :posts, Ash.Test.Actions.BulkDestroyAfterTransactionTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      notifiers: [Notifier],
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        pagination keyset?: true, required?: false
      end

      destroy :destroy_with_after_transaction do
        require_atomic? false

        change after_transaction(fn
                 _changeset, {:ok, result}, _context ->
                   {:ok, %{result | title: result.title <> "_stuff"}}

                 _changeset, {:error, error}, _context ->
                   send(self(), {:error, error})
               end)
      end

      destroy :destroy_with_atomic_after_transaction do
        change AfterTransactionChange
      end

      destroy :destroy_with_atomic_after_transaction_handling_errors do
        change AfterTransactionHandlingErrors
      end

      destroy :destroy_with_atomic_after_transaction_always_fails do
        change AfterTransactionHandlingErrors
        validate ConditionalValidation
      end

      destroy :destroy_with_atomic_after_transaction_returns_error do
        change AfterTransactionReturnsError
      end

      destroy :destroy_with_after_transaction_converts_error_to_success do
        change AfterTransactionConvertsErrorToSuccess
        validate ConditionalValidation
      end

      destroy :destroy_with_after_transaction_modifies_error do
        change AfterTransactionModifiesError
        validate ConditionalValidation
      end

      destroy :destroy_with_after_transaction_partial_failure do
        change AfterTransactionFailsForSomeRecords
      end

      destroy :destroy_with_after_action_failure_converted_to_success do
        change AfterActionFailsWithAfterTransaction
      end

      destroy :destroy_with_after_transaction_raises_exception do
        change AfterTransactionRaisesException
      end

      destroy :destroy_with_stop_on_error_hook do
        change AfterTransactionWithStopOnError
      end

      destroy :destroy_with_multiple_hooks do
        change MultipleAfterTransactionHooks
      end

      destroy :soft_with_after_transaction do
        soft? true
        change set_attribute(:title2, "soft_deleted")
        change SoftDestroyWithAfterTransaction
      end

      destroy :destroy_with_conditional_validation do
        validate ConditionalValidation
        change AfterTransactionHandlingErrors
      end

      destroy :destroy_with_conditional_after_action_error do
        change ConditionalAfterActionErrorWithAfterTransaction
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true
    end

    relationships do
      belongs_to :author, Author, public?: true
    end
  end

  defmodule MnesiaPost do
    @doc false

    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia, notifiers: [Notifier]

    mnesia do
      table :mnesia_post_after_transaction_destroys
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create, :update]

      destroy :destroy_with_after_transaction do
        change MnesiaAfterTransactionChange
      end

      destroy :destroy_with_after_action_error_and_after_transaction do
        # after_action returns error, triggering rollback
        change after_action(fn _changeset, _result, _context ->
                 send(self(), {:after_action_error_hook_called})
                 {:error, "after_action hook error"}
               end)

        # after_transaction should still be called after the rollback
        change after_transaction(fn _changeset, result, _context ->
                 send(self(), {:after_transaction_called, result})
                 result
               end)
      end

      destroy :destroy_with_conditional_after_action_error do
        change ConditionalAfterActionErrorWithAfterTransaction
      end

      destroy :destroy_with_after_transaction_partial_failure do
        change AfterTransactionFailsForSomeRecords
      end
    end
  end

  defmodule ConditionalManualDestroy do
    @moduledoc """
    Manual destroy module that fails only for records with "fail" in title.
    Used to test mixed success/failure scenarios with manual actions.
    """
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _opts, _context) do
      title = changeset.data.title || ""

      if String.contains?(title, "fail") do
        {:error, "conditional manual destroy error for: #{title}"}
      else
        case Ash.DataLayer.Ets.destroy(changeset.resource, changeset) do
          :ok -> {:ok, changeset.data}
          {:error, error} -> {:error, error}
        end
      end
    end
  end

  defmodule ManualDestroyPost do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier]

    alias Ash.Test.Actions.BulkDestroyAfterTransactionTest.ConditionalManualDestroy
    alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionChange
    alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionConvertsErrorChange

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :update, create: :*]

      destroy :destroy_manual_with_after_transaction do
        manual ConditionalManualDestroy
        change ManualAfterTransactionChange
      end

      destroy :destroy_manual_with_after_transaction_converts_error do
        manual ConditionalManualDestroy
        change ManualAfterTransactionConvertsErrorChange
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true
    end
  end

  defmodule PolicyPost do
    @moduledoc """
    Resource with policies for testing forbidden error scenarios.
    Reading is allowed for everyone, but destroying is forbidden unless actor has allow: true.
    This allows stream strategy to query records but fail on destroy.
    """
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    policies do
      # Allow reading for everyone (so stream can query records)
      policy action_type(:read) do
        authorize_if always()
      end

      # Allow creating without actor for test setup
      policy action_type(:create) do
        authorize_if always()
      end

      # Forbid destroy unless actor has allow: true
      policy action_type(:destroy) do
        forbid_unless actor_attribute_equals(:allow, true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, create: :*, update: :*]

      destroy :destroy do
        primary? true
      end

      destroy :destroy_with_after_transaction do
        change AfterTransactionHandlingErrors
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  describe ":atomic strategy" do
    test "after_transaction hooks work with :atomic strategy" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy!(:destroy_with_atomic_after_transaction, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for post_id <- post_ids do
        assert_received {:after_transaction_called, ^post_id}
      end

      assert [] = Ash.read!(Post)
    end

    test "empty result set doesn't cause errors with after_transaction hooks" do
      result =
        Post
        |> Ash.Query.filter(id == "nonexistent")
        |> Ash.bulk_destroy(:destroy_with_atomic_after_transaction, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert result.status == :success
      assert result.records == []
      refute_received {:after_transaction_called, _}
    end

    test "after_transaction hooks run on failure with :atomic strategy" do
      for i <- 1..3 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "z_fail_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy(:destroy_with_atomic_after_transaction_always_fails, %{},
          strategy: :atomic,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1
      assert_receive {:after_transaction_error, _error}
      refute_receive {:after_transaction_error, _}
      assert length(Ash.read!(Post)) == 3
    end

    test "after_transaction hook error is captured in result" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy(:destroy_with_atomic_after_transaction_returns_error, %{},
          strategy: :atomic,
          return_errors?: true,
          return_records?: true
        )

      for _ <- 1..5, do: assert_receive({:after_transaction_returning_error, _post_id}, 1000)

      assert result.status == :error
      assert result.error_count == 5
      assert length(result.errors) == 5
      assert result.records == []

      # Records ARE deleted - after_transaction runs after DB commit
      assert [] = Ash.read!(Post)
    end

    test "multiple after_transaction hooks execute in order" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy!(:destroy_with_multiple_hooks, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for post_id <- post_ids do
        assert_receive {:hook_1_executed, ^post_id, order1}, 1000
        assert_receive {:hook_2_executed, ^post_id, order2}, 1000
        assert order1 < order2, "Expected hook_1 to execute before hook_2"
      end
    end

    test "after_transaction hooks work with return_notifications?: true" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy!(:destroy_with_atomic_after_transaction, %{},
          strategy: :atomic,
          return_records?: true,
          return_notifications?: true
        )

      assert result.status == :success
      assert length(result.records) == 5
      assert length(result.notifications) == 5

      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "after_transaction hooks work with notify?: true" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      # Flush create notifications
      for _ <- 1..5, do: assert_receive({:notification, _})

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy!(:destroy_with_atomic_after_transaction, %{},
          strategy: :atomic,
          return_records?: true,
          notify?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_received({:notification, _})

      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "atomic_batches strategy return_notifications?: true returns notifications" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "notify_test_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy!(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: :atomic_batches,
          return_records?: true,
          return_notifications?: true,
          batch_size: 2
        )

      assert result.status == :success
      assert length(result.records) == 5
      assert length(result.notifications) == 5

      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "after_transaction hook partial failure sets status to :partial_success with :atomic strategy" do
      success_titles = ["success_1", "success_2", "success_3"]
      fail_titles = ["will_fail_1", "will_fail_2"]

      success_posts =
        for title <- success_titles do
          Post
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      fail_posts =
        for title <- fail_titles do
          Post
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      all_post_ids = Enum.map(success_posts ++ fail_posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^all_post_ids)
        |> Ash.bulk_destroy(:destroy_with_after_transaction_partial_failure, %{},
          strategy: :atomic,
          return_errors?: true,
          return_records?: true
        )

      for %{id: id} <- success_posts do
        assert_receive {:after_transaction_partial_success, ^id}, 1000
      end

      for %{id: id} <- fail_posts do
        assert_receive {:after_transaction_partial_failure, ^id}, 1000
      end

      assert result.status == :partial_success
      assert result.error_count == 2
      assert length(result.errors) == 2
      assert length(result.records) == 3
    end

    test "hooks execute with :atomic strategy even without return_records?" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: :atomic
        )

      assert result.status == :success

      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end

      assert [] = Ash.read!(Post)
    end

    # Note: "atomic operation fails entirely - hook converts error to success" test was removed
    # because in atomic mode, changeset.data is OriginalDataNotAvailable, so the hook
    # cannot return meaningful data. The same behavior is tested with :stream strategy
    # in "hook can convert validation error to success" test.

    test "load option with :atomic strategy and after_transaction hooks" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "load_atomic_#{i}", author_id: author.id})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.filter(contains(title, "load_atomic_"))
        |> Ash.bulk_destroy!(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: :atomic,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      for record <- result.records do
        assert %Author{name: "Test Author"} = record.author
      end

      for _ <- 1..5 do
        assert_receive {:after_transaction_called, _id}, 1000
      end
    end

    test "atomic destroy with multiple records" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "atomic_multi_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy!(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: :atomic,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end

      remaining = Post |> Ash.Query.filter(id in ^post_ids) |> Ash.read!()
      assert remaining == []
    end

    test "atomic destroy with hook returning error" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "atomic_error_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.filter(contains(title, "atomic_error_"))
        |> Ash.bulk_destroy(
          :destroy_with_atomic_after_transaction_returns_error,
          %{},
          strategy: :atomic,
          return_errors?: true,
          return_records?: true
        )

      for _ <- 1..5 do
        assert_receive {:after_transaction_returning_error, _id}, 1000
      end

      assert result.status == :error
      assert length(result.errors) == 5

      # Data is deleted even though hooks returned errors (after_transaction doesn't rollback)
      remaining = Post |> Ash.Query.filter(contains(title, "atomic_error_")) |> Ash.read!()
      assert remaining == []
    end
  end

  describe ":atomic_batches strategy" do
    test "after_transaction hooks work with list of records and atomic_batches strategy" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        posts
        |> Ash.bulk_destroy!(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: [:atomic_batches],
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success

      for post_id <- post_ids do
        assert_received {:after_transaction_called, ^post_id}
      end

      assert [] = Ash.read!(Post)
    end

    test "after_transaction hooks run on partial failure with :atomic_batches strategy" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_fail_5"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      # Flush create notifications
      for _ <- 1..5, do: assert_receive({:notification, _}, 1000)

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(:destroy_with_conditional_after_action_error, %{},
          strategy: [:atomic_batches],
          batch_size: 2,
          return_errors?: true
        )

      # Batch 1 (a_ok_1, a_ok_2): success
      assert_receive {:conditional_after_action_success, _}, 1000
      assert_receive {:conditional_after_action_success, _}, 1000
      assert_receive {:conditional_after_transaction, {:ok, _}}, 1000
      assert_receive {:conditional_after_transaction, {:ok, _}}, 1000

      # Batch 2 (a_ok_3, z_fail_4): partial - both deleted atomically, then hooks run
      assert_receive {:conditional_after_action_success, _}, 1000
      assert_receive {:conditional_after_action_error, _}, 1000
      assert_receive {:conditional_after_transaction, {:ok, _}}, 1000
      assert_receive {:conditional_after_transaction, {:error, _}}, 1000

      # Batch 3 (z_fail_5): never runs due to stop_on_error?: true
      refute_receive {:conditional_after_transaction, _}, 100

      assert result.status == :partial_success
      assert result.error_count == 1

      # 4 deleted (batches 1 and 2), 1 remaining (batch 3 never ran)
      remaining = Ash.read!(Post)
      assert length(remaining) == 1
      assert hd(remaining).title == "z_fail_5"
    end

    test "after_transaction hook error is captured with :atomic_batches strategy" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_fail_5"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(:destroy_with_after_transaction_partial_failure, %{},
          strategy: [:atomic_batches],
          batch_size: 2,
          return_errors?: true
        )

      # Batch 1 (a_ok_1, a_ok_2): hooks succeed
      assert_receive {:after_transaction_partial_success, _}, 1000
      assert_receive {:after_transaction_partial_success, _}, 1000

      # Batch 2 (a_ok_3, z_fail_4): a_ok_3 succeeds, z_fail_4 hook returns error
      assert_receive {:after_transaction_partial_success, _}, 1000
      assert_receive {:after_transaction_partial_failure, _}, 1000

      # Batch 3 (z_fail_5): never runs due to stop_on_error?: true
      refute_receive {:after_transaction_partial_failure, _}, 100

      assert result.status == :partial_success
      assert result.error_count == 1
    end

    test "after_transaction hooks work with notify?: true and :atomic_batches strategy" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      for _ <- 1..5, do: assert_receive({:notification, _}, 1000)

      post_ids = Enum.map(posts, & &1.id)

      result =
        posts
        |> Ash.bulk_destroy!(:destroy_with_atomic_after_transaction, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_records?: true,
          notify?: true
        )

      assert result.status == :success

      for _ <- 1..5, do: assert_receive({:notification, %{action: %{type: :destroy}}}, 1000)

      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "load option with :atomic_batches strategy and after_transaction hooks" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "load_batch_#{i}", author_id: author.id})
          |> Ash.create!()
        end

      for _ <- 1..5, do: assert_receive({:notification, _}, 1000)

      post_ids = Enum.map(posts, & &1.id)

      result =
        posts
        |> Ash.bulk_destroy!(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      for record <- result.records do
        assert %Author{name: "Test Author"} = record.author
      end

      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "atomic_batches destroy with after_transaction hooks" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "atomic_batch_#{i}"})
          |> Ash.create!()
        end

      for _ <- 1..5, do: assert_receive({:notification, _}, 1000)

      post_ids = Enum.map(posts, & &1.id)

      result =
        posts
        |> Ash.bulk_destroy!(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end
  end

  describe ":stream strategy" do
    test "after_transaction hooks work with :stream strategy" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
        |> Ash.create!()
      end

      for _ <- 1..5, do: assert_receive({:notification, _}, 1000)

      result =
        Post
        |> Ash.bulk_destroy!(:destroy_with_atomic_after_transaction, %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5 do
        assert_receive {:after_transaction_called, _}, 1000
      end

      assert [] = Ash.read!(Post)
    end

    test "after_transaction hooks work with :stream strategy and return_stream?" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
        |> Ash.create!()
      end

      result_stream =
        Post
        |> Ash.bulk_destroy(:destroy_with_atomic_after_transaction, %{},
          strategy: :stream,
          batch_size: 2,
          return_stream?: true,
          return_records?: true
        )

      results = Enum.to_list(result_stream)
      assert length(results) == 5

      for _ <- 1..5 do
        assert_receive {:after_transaction_called, _}, 1000
      end

      assert [] = Ash.read!(Post)
    end

    test "after_transaction hook error is captured with :stream strategy" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_fail_5"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(:destroy_with_after_transaction_partial_failure, %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true
        )

      # Batch 1 (a_ok_1, a_ok_2): hooks succeed
      assert_receive {:after_transaction_partial_success, _}, 1000
      assert_receive {:after_transaction_partial_success, _}, 1000

      # Batch 2 (a_ok_3, z_fail_4): a_ok_3 succeeds, z_fail_4 hook returns error
      assert_receive {:after_transaction_partial_success, _}, 1000
      assert_receive {:after_transaction_partial_failure, _}, 1000

      # Batch 3 (z_fail_5): never runs due to stop_on_error?: true
      refute_receive {:after_transaction_partial_failure, _}, 100

      assert result.status == :partial_success
      assert result.error_count == 1
    end

    test "after_transaction hooks run on failure with return_stream?" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "z_fail_#{i}"})
        |> Ash.create!()
      end

      result_stream =
        Post
        |> Ash.bulk_destroy(:destroy_with_atomic_after_transaction_always_fails, %{},
          strategy: :stream,
          batch_size: 2,
          return_stream?: true,
          return_errors?: true
        )

      results = Enum.to_list(result_stream)
      assert length(results) == 5

      for result <- results do
        assert {:error, _} = result
      end

      for _ <- 1..5, do: assert_receive({:after_transaction_error, _})

      assert length(Ash.read!(Post)) == 5
    end

    test "after_transaction hooks work with :stream strategy on success" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy!(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5 do
        assert_receive {:after_transaction_called, _}, 1000
      end
    end

    test "after_transaction hooks work with return_notifications?: true and :stream strategy" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy!(:destroy_with_atomic_after_transaction, %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true,
          return_notifications?: true
        )

      assert result.status == :success
      assert length(result.records) == 5
      assert length(result.notifications) == 5
      assert Enum.all?(result.notifications, &(&1.action.type == :destroy))

      for _ <- 1..5 do
        assert_receive {:after_transaction_called, _}, 1000
      end
    end

    test "after_transaction hooks work with notify?: true and :stream strategy" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
        |> Ash.create!()
      end

      for _ <- 1..5, do: assert_receive({:notification, _}, 1000)

      result =
        Post
        |> Ash.bulk_destroy!(:destroy_with_atomic_after_transaction, %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true,
          notify?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:notification, %{action: %{type: :destroy}}}, 1000)

      for _ <- 1..5 do
        assert_receive {:after_transaction_called, _}, 1000
      end
    end

    test "hook can convert validation error to success" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "z_fail_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_converts_error_to_success,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true,
          return_errors?: true
        )

      for _ <- 1..5, do: assert_receive({:after_transaction_converted_error})

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5

      assert length(Ash.read!(Post)) == 5
    end

    test "load option is applied to records from after_transaction converting error to success" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "z_fail_#{i}", author_id: author.id})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_converts_error_to_success,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          load: [:author]
        )

      for _ <- 1..5, do: assert_receive({:after_transaction_converted_error})

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5

      for record <- result.records do
        assert record.author_id == author.id
        assert %Author{name: "Test Author"} = record.author
      end

      assert length(Ash.read!(Post)) == 5
    end

    test "sorted? option works with after_transaction converting error to success" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "z_fail_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_converts_error_to_success,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          sorted?: true
        )

      for _ <- 1..5, do: assert_receive({:after_transaction_converted_error})

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5

      titles = Enum.map(result.records, & &1.title) |> Enum.uniq()
      assert titles == ["recovered_from_error"]

      assert length(Ash.read!(Post)) == 5
    end

    test "hook can modify validation error" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "z_fail_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_modifies_error,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true
        )

      assert_receive {:after_transaction_modified_error}, 1000

      assert result.status == :error
      assert result.error_count == 1

      error = hd(result.errors)
      assert Exception.message(error) =~ "custom error from hook"

      assert length(Ash.read!(Post)) == 5
    end

    test "error to success conversion with transaction: :all and sorted?: true" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "z_fail_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_converts_error_to_success,
          %{},
          strategy: :stream,
          batch_size: 2,
          transaction: :all,
          return_records?: true,
          return_errors?: true,
          sorted?: true
        )

      for _ <- 1..5 do
        assert_receive {:after_transaction_converted_error}, 1000
      end

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5

      titles = Enum.map(result.records, & &1.title) |> Enum.uniq()
      assert titles == ["recovered_from_error"]

      assert length(Ash.read!(Post)) == 5
    end

    test "error to success conversion with transaction: :all without sorted?" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "z_fail_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_converts_error_to_success,
          %{},
          strategy: :stream,
          batch_size: 2,
          transaction: :all,
          return_records?: true,
          return_errors?: true
        )

      for _ <- 1..5 do
        assert_receive {:after_transaction_converted_error}, 1000
      end

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5

      assert length(Ash.read!(Post)) == 5
    end

    test "after_action failure converted to success with transaction: :all" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "fail_4", "fail_5"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(
          :destroy_with_after_action_failure_converted_to_success,
          %{},
          strategy: :stream,
          batch_size: 2,
          transaction: :all,
          return_records?: true,
          return_errors?: true
        )

      # after_action fails for "fail_" titles, after_transaction converts to success
      for _ <- 1..2 do
        assert_receive {:after_action_failed_converted_to_success}, 1000
      end

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5
    end

    test "after_action failure converted to success with transaction: :batch" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "fail_4", "fail_5"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(
          :destroy_with_after_action_failure_converted_to_success,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true,
          return_errors?: true
        )

      # after_action fails for "fail_" titles, after_transaction converts to success
      for _ <- 1..2 do
        assert_receive {:after_action_failed_converted_to_success}, 1000
      end

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5
    end

    test "exception in hook is caught and converted to error" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "fail_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_raises_exception,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true,
          return_records?: true,
          authorize?: false
        )

      # First hook raises, stop_on_error stops processing
      assert_receive {:before_exception_raise, _}

      assert result.status == :error
      assert length(result.errors) == 1
      [error] = result.errors
      assert Exception.message(error) =~ "Hook intentionally raised exception"
    end

    test "hook raising exception with stop_on_error?: false processes all records" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "fail_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_raises_exception,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true,
          return_records?: true,
          stop_on_error?: false,
          authorize?: false
        )

      for _ <- 1..5 do
        assert_receive {:before_exception_raise, _}, 1000
      end

      assert result.status == :error
      assert length(result.errors) == 5
    end

    test "hook returning error stops at first failure" do
      # First batch (a_*) succeeds, second batch (z_*) fails and stops
      for title <- ["a_ok_1", "a_ok_2", "z_stop_here_1", "z_stop_here_2", "z_stop_here_3"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(
          :destroy_with_stop_on_error_hook,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true,
          authorize?: false
        )

      # First batch succeeded (both destroyed)
      assert_receive {:hook_success_for_stop_on_error, _}
      assert_receive {:hook_success_for_stop_on_error, _}
      # Second batch: first record destroyed then hook error stopped processing
      assert_receive {:hook_error_for_stop_on_error, _}

      assert result.status == :partial_success
      assert result.error_count == 1

      # z_stop_here_2 and z_stop_here_3 remain - never processed due to stop
      remaining = Post |> Ash.Query.sort(:title) |> Ash.read!()
      assert [%{title: "z_stop_here_2"}, %{title: "z_stop_here_3"}] = remaining
    end

    test "stop_on_error?: false continues after hook error" do
      for title <- ["a_ok_1", "a_ok_2", "z_stop_here_1", "z_stop_here_2", "z_stop_here_3"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(
          :destroy_with_stop_on_error_hook,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true,
          return_records?: true,
          stop_on_error?: false,
          authorize?: false
        )

      for _ <- 1..2, do: assert_receive({:hook_success_for_stop_on_error, _})
      for _ <- 1..3, do: assert_receive({:hook_error_for_stop_on_error, _})

      assert result.status == :partial_success
      assert length(result.records) == 2
      assert length(result.errors) == 3

      # All records destroyed (hooks run after destroy completes)
      assert [] = Ash.read!(Post)
    end

    test "after_transaction hooks modify result with :stream strategy" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy!(
          :destroy_with_after_transaction,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_records?: true,
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      for record <- result.records do
        assert String.ends_with?(record.title, "_stuff")
      end

      assert [] = Ash.read!(Post)
    end

    test "load option with transaction: :all and after_transaction hooks" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post_#{i}", author_id: author.id})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy!(
          :destroy_with_atomic_after_transaction,
          %{},
          strategy: :stream,
          batch_size: 2,
          transaction: :all,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      for record <- result.records do
        assert %Author{name: "Test Author"} = record.author
      end

      for _ <- 1..5 do
        assert_receive {:after_transaction_called, _}, 1000
      end

      assert [] = Ash.read!(Post)
    end
  end

  describe ":stream strategy with Mnesia" do
    setup do
      capture_log(fn ->
        Ash.DataLayer.Mnesia.start(Domain, [MnesiaPost])
      end)

      # Clean up any existing records to ensure test isolation
      MnesiaPost
      |> Ash.read!()
      |> Enum.each(fn record -> Ash.destroy!(record) end)

      :ok
    end

    test "after_action error with rollback_on_error? triggers rollback" do
      # First batch (a_ok_*) succeeds, second batch (z_fail_*) fails
      for title <- ["a_ok_1", "a_ok_2", "z_fail_3", "z_fail_4", "z_fail_5"] do
        MnesiaPost
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      for _ <- 1..5, do: assert_receive({:notification, _})

      {result, log} =
        with_log(fn ->
          MnesiaPost
          |> Ash.Query.sort(:title)
          |> Ash.bulk_destroy(
            :destroy_with_conditional_after_action_error,
            %{},
            strategy: :stream,
            batch_size: 2,
            transaction: :all,
            rollback_on_error?: true,
            return_errors?: true
          )
        end)

      # First batch succeeded
      assert_receive {:conditional_after_action_success, _}
      assert_receive {:conditional_after_action_success, _}
      # Second batch failed
      assert_receive {:conditional_after_action_error, _}

      assert result.status == :error
      assert length(result.errors) > 0

      # With transaction: :all, after_transaction runs INSIDE transaction
      # So it's called for first batch before second batch triggers rollback
      assert_receive {:conditional_after_transaction, {:ok, _}}
      assert_receive {:conditional_after_transaction, {:ok, _}}
      # No further after_transaction calls after rollback
      refute_receive {:conditional_after_transaction, _}

      # Warning emitted about after_transaction inside transaction
      assert log =~ "after_transaction"

      # All records still exist (entire transaction rolled back, including first batch)
      assert length(Ash.read!(MnesiaPost)) == 5
    end

    test "after_action error with transaction: :batch rolls back failing batch only" do
      # First batch (a_ok_*) succeeds and commits, second batch (z_fail_*) fails and rolls back
      for title <- ["a_ok_1", "a_ok_2", "z_fail_3", "z_fail_4", "z_fail_5"] do
        MnesiaPost
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      for _ <- 1..5, do: assert_receive({:notification, _})

      {result, log} =
        with_log(fn ->
          MnesiaPost
          |> Ash.Query.sort(:title)
          |> Ash.bulk_destroy(
            :destroy_with_conditional_after_action_error,
            %{},
            strategy: :stream,
            batch_size: 2,
            transaction: :batch,
            rollback_on_error?: true,
            return_errors?: true
          )
        end)

      # First batch succeeded and committed
      assert_receive {:conditional_after_action_success, _}
      assert_receive {:conditional_after_action_success, _}
      # Second batch failed
      assert_receive {:conditional_after_action_error, _}

      assert result.status == :partial_success
      assert result.error_count == 1

      # With transaction: :batch, after_transaction runs OUTSIDE transaction
      # So it's called for first batch (which committed)
      assert_receive {:conditional_after_transaction, {:ok, _}}
      assert_receive {:conditional_after_transaction, {:ok, _}}
      # No further after_transaction calls (second batch rolled back, third batch never ran)
      refute_receive {:conditional_after_transaction, _}

      # No warning - after_transaction runs outside transaction with :batch
      refute log =~ "after_transaction"

      # First batch deleted (committed), second batch rolled back, third batch never ran
      remaining = MnesiaPost |> Ash.Query.sort(:title) |> Ash.read!()
      assert [%{title: "z_fail_3"}, %{title: "z_fail_4"}, %{title: "z_fail_5"}] = remaining
    end

    test "after_transaction hooks run outside batch transaction - no warning" do
      for i <- 1..5 do
        MnesiaPost
        |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
        |> Ash.create!()
      end

      for _ <- 1..5, do: assert_receive({:notification, _})

      log =
        capture_log(fn ->
          result =
            MnesiaPost
            |> Ash.bulk_destroy(
              :destroy_with_after_transaction,
              %{},
              batch_size: 2,
              return_records?: true,
              authorize?: false
            )

          assert result.status == :success
          assert length(result.records) == 5
        end)

      for _ <- 1..5, do: assert_receive({:mnesia_after_transaction_called, _})

      refute log =~ "after_transaction"

      assert Ash.read!(MnesiaPost) == []
    end

    test "after_transaction error with transaction: :batch - records still deleted (runs outside tx)" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_ok_5"] do
        MnesiaPost
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        MnesiaPost
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(
          :destroy_with_after_transaction_partial_failure,
          %{},
          strategy: :stream,
          transaction: :batch,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      for _ <- 1..3, do: assert_receive({:after_transaction_partial_success, _})
      assert_receive {:after_transaction_partial_failure, _}
      refute_receive {:after_transaction_partial_success, _}

      assert result.status == :partial_success
      assert length(result.errors) == 1

      # Batches 1-2 committed (4 deleted), batch 3 skipped - hook runs OUTSIDE tx
      remaining = MnesiaPost |> Ash.read!() |> Enum.map(& &1.title)
      assert remaining == ["z_ok_5"]
    end

    test "after_transaction error with transaction: :all - entire operation rolls back" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_ok_5"] do
        MnesiaPost
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      {result, log} =
        with_log(fn ->
          MnesiaPost
          |> Ash.Query.sort(:title)
          |> Ash.bulk_destroy(
            :destroy_with_after_transaction_partial_failure,
            %{},
            strategy: :stream,
            transaction: :all,
            batch_size: 2,
            rollback_on_error?: true,
            return_errors?: true
          )
        end)

      # Hook runs INSIDE tx with :all (warning emitted)
      assert log =~ "after_transaction" and log =~ "ongoing transaction"

      for _ <- 1..3, do: assert_receive({:after_transaction_partial_success, _})
      assert_receive {:after_transaction_partial_failure, _}

      assert result.status == :error
      assert length(result.errors) == 1

      # All records still exist - entire transaction rolled back
      remaining =
        MnesiaPost
        |> Ash.Query.sort(:title)
        |> Ash.read!()
        |> Enum.map(& &1.title)

      assert remaining == ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_ok_5"]
    end
  end

  describe "soft destroy" do
    test "after_transaction hooks execute on soft destroy" do
      for i <- 1..5 do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "soft_test_#{i}"})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.bulk_destroy!(:soft_with_after_transaction, %{},
          batch_size: 2,
          strategy: [:stream],
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5,
          do: assert_receive({:after_transaction_soft_destroy_called, _, "soft_deleted"})

      remaining = Ash.read!(Post)
      assert length(remaining) == 5
      assert Enum.all?(remaining, &(&1.title2 == "soft_deleted"))
    end
  end

  describe "manual actions" do
    test "after_transaction hooks work with manual destroy action" do
      for i <- 1..5 do
        ManualDestroyPost
        |> Ash.Changeset.for_create(:create, %{title: "manual_bulk_destroy_#{i}"})
        |> Ash.create!()
      end

      result =
        ManualDestroyPost
        |> Ash.bulk_destroy!(
          :destroy_manual_with_after_transaction,
          %{},
          batch_size: 2,
          strategy: :stream,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:manual_after_transaction_success, _})

      assert Ash.read!(ManualDestroyPost) == []
    end

    test "mixed success/failure with error conversion in manual destroy" do
      for title <- ["a_ok_1", "a_ok_2", "z_fail_3", "z_fail_4", "z_fail_5"] do
        ManualDestroyPost
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        ManualDestroyPost
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(
          :destroy_manual_with_after_transaction_converts_error,
          %{},
          batch_size: 2,
          strategy: :stream,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      # 2 successes (a_ok_*) + 3 converted errors (z_fail_*)
      for _ <- 1..3, do: assert_receive({:manual_after_transaction_converted_error})

      # a_ok_* records deleted, z_fail_* records still exist (manual destroy failed)
      remaining = ManualDestroyPost |> Ash.Query.sort(:title) |> Ash.read!()
      assert [%{title: "z_fail_3"}, %{title: "z_fail_4"}, %{title: "z_fail_5"}] = remaining
    end

    test "manual destroy after_transaction hooks work with transaction: :all" do
      for i <- 1..5 do
        ManualDestroyPost
        |> Ash.Changeset.for_create(:create, %{title: "tx_all_manual_destroy_#{i}"})
        |> Ash.create!()
      end

      result =
        ManualDestroyPost
        |> Ash.bulk_destroy!(
          :destroy_manual_with_after_transaction,
          %{},
          batch_size: 2,
          strategy: :stream,
          transaction: :all,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:manual_after_transaction_success, _})

      assert Ash.read!(ManualDestroyPost) == []
    end
  end

  describe "forbidden errors" do
    test "after_transaction hook is called on forbidden error with :atomic strategy" do
      for i <- 1..3 do
        PolicyPost
        |> Ash.Changeset.for_create(:create, %{title: "forbidden_atomic_#{i}"})
        |> Ash.create!(authorize?: false)
      end

      result =
        PolicyPost
        |> Ash.bulk_destroy(:destroy_with_after_transaction, %{},
          strategy: :atomic,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error

      assert_receive {:after_transaction_error, error}
      assert %Ash.Error.Forbidden{} = error
      refute_receive {:after_transaction_error, _}

      assert length(Ash.read!(PolicyPost, authorize?: false)) == 3
    end

    test "after_transaction hook is called on forbidden error with :atomic_batches strategy" do
      for i <- 1..3 do
        PolicyPost
        |> Ash.Changeset.for_create(:create, %{title: "forbidden_batches_#{i}"})
        |> Ash.create!(authorize?: false)
      end

      result =
        PolicyPost
        |> Ash.bulk_destroy(:destroy_with_after_transaction, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error

      assert_receive {:after_transaction_error, error}
      assert %Ash.Error.Forbidden{} = error
      refute_receive {:after_transaction_error, _}

      assert length(Ash.read!(PolicyPost, authorize?: false)) == 3
    end

    test "after_transaction hook is called on forbidden error with :stream strategy" do
      for i <- 1..3 do
        PolicyPost
        |> Ash.Changeset.for_create(:create, %{title: "forbidden_stream_#{i}"})
        |> Ash.create!(authorize?: false)
      end

      result =
        PolicyPost
        |> Ash.bulk_destroy(:destroy_with_after_transaction, %{},
          strategy: :stream,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error

      assert_receive {:after_transaction_error, error}
      assert %Ash.Error.Forbidden{} = error
      refute_receive {:after_transaction_error, _}

      assert length(Ash.read!(PolicyPost, authorize?: false)) == 3
    end
  end

  describe "validation errors" do
    test "validation error with :atomic strategy" do
      for title <- ["a_ok_1", "a_ok_2", "z_fail_3"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(:destroy_with_conditional_validation, %{},
          strategy: :atomic,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error

      assert_receive {:after_transaction_error, _error}
      refute_receive {:after_transaction_error, _}

      assert length(Ash.read!(Post)) == 3
    end

    test "validation error with :atomic_batches strategy" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_fail_5"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(:destroy_with_conditional_validation, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_errors?: true
        )

      # First batch succeeds, second batch fails (stop_on_error?: true by default)
      assert result.status == :error

      # First batch: 2 successes, second batch: error (batch fails atomically)
      assert_receive {:after_transaction_success, _}
      assert_receive {:after_transaction_success, _}
      assert_receive {:after_transaction_error, _}
      refute_receive {:after_transaction_error, _}

      # a_ok_1, a_ok_2 deleted; a_ok_3 rolled back with z_fail_4; z_fail_5 never ran
      remaining = Post |> Ash.Query.sort(:title) |> Ash.read!()
      assert [%{title: "a_ok_3"}, %{title: "z_fail_4"}, %{title: "z_fail_5"}] = remaining
    end

    test "validation error with :stream strategy" do
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_fail_5"] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: title})
        |> Ash.create!()
      end

      result =
        Post
        |> Ash.Query.sort(:title)
        |> Ash.bulk_destroy(:destroy_with_conditional_validation, %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true
        )

      # Stream processes records individually (no batch-level transaction)
      assert result.status == :partial_success

      # 3 successes (a_ok_*), 1 error (z_fail_4), then stop_on_error kicks in
      assert_receive {:after_transaction_success, _}
      assert_receive {:after_transaction_success, _}
      assert_receive {:after_transaction_success, _}
      assert_receive {:after_transaction_error, _}
      refute_receive {:after_transaction_error, _}

      # a_ok_* all deleted; z_fail_4 failed; z_fail_5 never ran
      remaining = Post |> Ash.Query.sort(:title) |> Ash.read!()
      assert [%{title: "z_fail_4"}, %{title: "z_fail_5"}] = remaining
    end
  end
end
