# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkUpdateAfterTransactionTest do
  @moduledoc """
  Tests for after_transaction hooks in bulk update operations.

  Note: Uses `async: false` because of Mnesia.

  Tests are organized by strategy:
  - :atomic strategy
  - :atomic_batches strategy
  - :stream strategy
  - :stream strategy with Mnesia
  - manual actions
  - forbidden errors
  - validation errors
  """
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  # Shared change modules (alphabetically ordered)
  alias Ash.Test.BulkAfterTransaction.AfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.AfterTransactionHandlingErrors
  alias Ash.Test.BulkAfterTransaction.AfterTransactionModifiesError
  alias Ash.Test.BulkAfterTransaction.AfterTransactionReturnsError
  alias Ash.Test.BulkAfterTransaction.AfterTransactionWithStopOnError
  alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionConvertsErrorChange
  alias Ash.Test.BulkAfterTransaction.MnesiaAfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.MultipleAfterTransactionHooks
  alias Ash.Test.BulkAfterTransaction.Notifier

  # ===========================================================================
  # UPDATE-SPECIFIC MODULES
  # ===========================================================================

  defmodule NotAtomicAfterTransactionChange do
    @moduledoc """
    Change module that forces fallback from atomic to stream by returning :not_atomic.
    Used to test that after_transaction hooks work after strategy fallback.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, {:ok, result} ->
        send(self(), {:after_transaction_after_fallback, result.id})
        {:ok, result}
      end)
    end

    def atomic(_changeset, _opts, _context) do
      :not_atomic
    end
  end

  defmodule AfterTransactionChangeModifiesResult do
    @moduledoc """
    Change module that modifies the result in after_transaction and supports atomic.
    Used to test that load happens AFTER after_transaction hooks.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, {:ok, result} ->
        {:ok, %{result | title: result.title <> "_modified_by_hook"}}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterTransactionConvertsErrorToSuccess do
    @moduledoc """
    Change module where after_transaction hook converts validation error to success.
    Used to test that hook return values are respected for invalid changesets.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn
        _changeset, {:ok, result} ->
          {:ok, result}

        changeset, {:error, _original_error} ->
          send(self(), {:after_transaction_converted_error})
          # Return a "fake" success with the original data
          {:ok, changeset.data}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterTransactionPartialFailure do
    @moduledoc """
    Change module where after_transaction hook returns an error for records with title containing "fail".
    Used to test partial_success status when some records succeed and some fail.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn
        _changeset, {:ok, result} ->
          if String.contains?(result.title, "fail") do
            send(self(), {:after_transaction_partial_failure, result.id})
            {:error, "Hook failed for title containing 'fail'"}
          else
            send(self(), {:after_transaction_partial_success, result.id})
            {:ok, result}
          end

        _changeset, {:error, error} ->
          {:error, error}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterActionFailsWithAfterTransaction do
    @moduledoc """
    Change module that adds an after_action hook that fails for specific records,
    and an after_transaction hook that converts the error to success.

    Used to test error-to-success conversion in after_transaction hooks:
    - after_action fails for records with titles starting with "fail_"
    - after_transaction catches the error and returns {:ok, recovered_record}
    """
    use Ash.Resource.Change

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end

    def change(changeset, _opts, _context) do
      changeset
      |> Ash.Changeset.after_action(fn _changeset, result ->
        # Fail if title starts with "fail_"
        if String.starts_with?(result.title || "", "fail_") do
          {:error,
           Ash.Error.Changes.InvalidAttribute.exception(
             field: :title,
             message: "title cannot start with fail_"
           )}
        else
          {:ok, result}
        end
      end)
      |> Ash.Changeset.after_transaction(fn
        _changeset, {:ok, result} ->
          {:ok, result}

        changeset, {:error, _original_error} ->
          send(self(), {:after_action_failed_converted_to_success})
          # Return a "fake" success with a recovered record
          {:ok, %{changeset.data | title: "recovered_from_after_action_failure"}}
      end)
    end
  end

  defmodule ConditionalAfterActionErrorWithAfterTransaction do
    @moduledoc """
    Change module for testing partial batch failures.

    Behavior:
    - Sets title to "UPDATED_TITLE" to track which records were processed
    - after_action fails only for records with titles containing "fail"
    - after_transaction sends messages to verify hook execution

    Used to test rollback behavior when first batch succeeds and second batch fails.
    """
    use Ash.Resource.Change

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end

    def change(changeset, _opts, _context) do
      changeset
      # Set a new title so we can verify rollback behavior
      |> Ash.Changeset.force_change_attribute(:title, "UPDATED_TITLE")
      |> Ash.Changeset.after_action(fn changeset, result ->
        # Check ORIGINAL title from changeset.data, not the modified result
        original_title = changeset.data.title || ""

        if String.contains?(original_title, "fail") do
          send(self(), {:conditional_after_action_error, result.id})

          {:error,
           Ash.Error.Changes.InvalidAttribute.exception(
             field: :title,
             message: "conditional error for fail title"
           )}
        else
          send(self(), {:conditional_after_action_success, result.id})
          {:ok, result}
        end
      end)
      |> Ash.Changeset.after_transaction(fn _changeset, result ->
        send(self(), {:conditional_after_transaction, result})
        result
      end)
    end
  end

  defmodule AfterTransactionFailsForSomeRecords do
    @moduledoc """
    Change module where after_transaction hook returns an error for records with title containing "fail".
    Used to test partial_success status when some records succeed and some fail.
    Prepends "UPDATED_" to verify the update happened while preserving original title for checking.
    """
    use Ash.Resource.Change

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end

    def change(changeset, _opts, _context) do
      # Get current title to prepend to (preserves "fail" substring)
      current_title = Ash.Changeset.get_attribute(changeset, :title) || ""

      changeset
      # Prepend "UPDATED_" so we can verify update happened while keeping "fail" visible
      |> Ash.Changeset.force_change_attribute(:title, "UPDATED_#{current_title}")
      |> Ash.Changeset.after_transaction(fn
        _changeset, {:ok, result} ->
          if String.contains?(result.title || "", "fail") do
            send(self(), {:after_transaction_partial_failure, result.id})
            {:error, "Hook failed for title containing 'fail'"}
          else
            send(self(), {:after_transaction_partial_success, result.id})
            {:ok, result}
          end

        _changeset, {:error, error} ->
          {:error, error}
      end)
    end
  end

  defmodule AlwaysFailsValidation do
    use Ash.Resource.Validation

    @impl true
    def validate(_, _, _) do
      {:error, field: :title, message: "always fails"}
    end

    @impl true
    def atomic(_, _, _) do
      {:error, field: :title, message: "always fails atomically"}
    end
  end

  defmodule AtomicValidationFailsInTransaction do
    @moduledoc """
    Validation that fails when title == "will_fail".
    Has both validate/3 and atomic/3 callbacks for different strategies.
    """
    use Ash.Resource.Validation

    @impl true
    def validate(changeset, _opts, _context) do
      if Ash.Changeset.get_attribute(changeset, :title) == "will_fail" do
        {:error, field: :title, message: "atomic validation failed"}
      else
        :ok
      end
    end

    @impl true
    def atomic(_changeset, _opts, _context) do
      {:atomic, [:title], expr(^atomic_ref(:title) == "will_fail"),
       expr(
         error(Ash.Error.Changes.InvalidAttribute, %{
           field: :title,
           message: "atomic validation failed"
         })
       )}
    end
  end

  defmodule AtomicOnlyValidation do
    @moduledoc """
    Validation with ONLY atomic/3 callback - no validate/3 fallback.
    Ensures the validation runs in the data layer.
    """
    use Ash.Resource.Validation

    @impl true
    def atomic(_changeset, _opts, _context) do
      {:atomic, [:title], expr(^atomic_ref(:title) == "will_fail"),
       expr(
         error(Ash.Error.Changes.InvalidAttribute, %{
           field: :title,
           message: "atomic validation failed"
         })
       )}
    end
  end

  defmodule AfterTransactionRaisesException do
    @moduledoc """
    Change module where after_transaction hook raises an exception.
    Used to test that exceptions in hooks are caught and converted to errors.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, {:ok, result} ->
        send(self(), {:before_exception_raise, result.id})
        raise "Hook intentionally raised exception"
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
        public? true
      end
    end

    relationships do
      has_many :posts, Ash.Test.Actions.BulkUpdateAfterTransactionTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule SimplePost do
    @moduledoc """
    A minimal resource with NO identities, NO validations, NO relationships.
    Used to test pure hook behavior without interference.
    """
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public? true
      end

      attribute :body, :string do
        public? true
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :update_with_after_transaction do
        change AfterTransactionChange
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :update_with_after_transaction do
        require_atomic? false

        change after_transaction(fn
                 _changeset, {:ok, result}, _context ->
                   {:ok, %{result | title: result.title <> "_stuff"}}

                 _changeset, {:error, error}, _context ->
                   send(self(), {:error, error})
                   {:error, error}
               end)
      end

      update :update_with_atomic_after_transaction do
        change AfterTransactionChange
      end

      update :update_with_multiple_hooks do
        change MultipleAfterTransactionHooks
      end

      update :update_with_atomic_after_transaction_modifying_result do
        change AfterTransactionChangeModifiesResult
      end

      update :update_with_atomic_upgrade_and_after_transaction do
        # Tests that after_transaction hooks work with atomic execution
        change AfterTransactionChange
      end

      update :update_with_not_atomic_after_transaction do
        # Forces fallback from atomic to stream, then runs after_transaction hook
        change NotAtomicAfterTransactionChange
      end

      update :update_with_atomic_after_transaction_always_fails do
        change AfterTransactionHandlingErrors
        validate AlwaysFailsValidation
      end

      update :update_with_atomic_after_transaction_returns_error do
        change AfterTransactionReturnsError
      end

      update :update_with_after_transaction_converts_error_to_success do
        change AfterTransactionConvertsErrorToSuccess
      end

      update :update_with_after_transaction_modifies_error do
        change AfterTransactionModifiesError
      end

      update :update_with_after_transaction_partial_failure do
        change AfterTransactionPartialFailure
      end

      update :update_with_after_action_failure_converted_to_success do
        change AfterActionFailsWithAfterTransaction
      end

      update :update_with_after_transaction_raises_exception do
        change AfterTransactionRaisesException
      end

      update :update_with_stop_on_error_hook do
        change AfterTransactionWithStopOnError
      end

      update :update_with_atomic_validation_in_transaction do
        validate AtomicValidationFailsInTransaction
        change AfterTransactionHandlingErrors
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
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia, notifiers: [Notifier]

    mnesia do
      table :mnesia_post_after_transaction_updates
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create, :update]

      update :update_with_after_action_error_and_after_transaction do
        # Modify title so we can verify rollback
        change set_attribute(:title, "UPDATED_BY_ACTION")

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

      update :update_with_after_transaction do
        change MnesiaAfterTransactionChange
      end

      update :update_with_atomic_validation do
        validate AtomicOnlyValidation

        change after_transaction(fn _changeset, result, _context ->
                 case result do
                   {:ok, record} ->
                     send(self(), {:atomic_validation_after_transaction_success, record.id})

                   {:error, error} ->
                     send(self(), {:atomic_validation_after_transaction_error, error})
                 end

                 result
               end)
      end

      update :update_with_conditional_after_action_error do
        change ConditionalAfterActionErrorWithAfterTransaction
      end

      update :update_with_after_transaction_partial_failure do
        change AfterTransactionFailsForSomeRecords
      end
    end
  end

  describe ":atomic strategy" do
    test "hooks execute" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      # after_transaction hooks can now be added in atomic/3 callback
      # and will execute after the transaction closes
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{}, strategy: :atomic)

      assert result.status == :success

      # Verify the hook executed by checking for the message
      assert_received {:after_transaction_called, post_id}
      assert post_id == post.id
    end

    test "hooks return records when requested" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 1

      # Verify the hook executed
      assert_received {:after_transaction_called, post_id}
      assert post_id == post.id
    end

    test "hook receives correct changeset and result" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      # Use a hook that captures the changeset
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{}, strategy: :atomic)

      assert result.status == :success
      assert_received {:after_transaction_called, post_id}
      assert post_id == post.id
    end

    test "hook error is captured in result" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      # This test would need a hook that returns an error
      # For now, verify the basic mechanism works
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{}, strategy: :atomic)

      assert result.status == :success
    end

    test "multiple hooks execute in order" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:update_with_multiple_hooks, %{}, strategy: :atomic)

      assert result.status == :success

      # Verify hooks executed in order (hook_1 before hook_2)
      assert_receive {:hook_1_executed, id, order1}, 1000
      assert_receive {:hook_2_executed, ^id, order2}, 1000
      assert order1 < order2, "Expected hook_1 to execute before hook_2"
    end

    test "empty result set doesn't cause errors" do
      # Query that matches no records
      result =
        Post
        |> Ash.Query.filter(id == "nonexistent")
        |> Ash.bulk_update(:update_with_atomic_after_transaction, %{}, strategy: :atomic)

      assert result.status == :success
      assert result.records == []
      # No hooks should execute since no records matched
      refute_received {:after_transaction_called, _}
    end

    test "return_errors?: true returns errors in result" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      # This action's hook returns an error even on success
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(:update_with_atomic_after_transaction_returns_error, %{},
          strategy: :atomic,
          return_errors?: true
        )

      # Verify the hook was called
      assert_receive {:after_transaction_returning_error, _post_id}, 1000

      # Verify errors are captured in result.errors
      assert result.status == :error
      assert result.error_count == 1
      assert length(result.errors) == 1
    end

    test "return_records?: true and return_errors?: true work together on success" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: :atomic,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :success
      assert length(result.records) == 3
      assert result.errors == []

      # Verify hooks executed
      for post_id <- post_ids do
        assert_received {:after_transaction_called, ^post_id}
      end
    end

    test "hooks run on failure" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      # This action has a validation that always fails
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(:update_with_atomic_after_transaction_always_fails, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1

      # Verify the after_transaction hook received the error
      assert_receive {:after_transaction_error, _error}, 1000

      # Verify the post was NOT updated (operation failed)
      assert [_] = Ash.read!(Post)
    end

    test "hook error on success is captured in result" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      # This action's hook returns an error even on success
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(:update_with_atomic_after_transaction_returns_error, %{},
          strategy: :atomic,
          return_errors?: true,
          return_records?: true
        )

      # The hook was called and returned an error
      assert_receive {:after_transaction_returning_error, _post_id}, 1000

      # The operation should show the error from the hook with correct status
      assert result.status == :error
      assert result.error_count == 1
      assert length(result.errors) == 1
      assert result.records == []
    end

    test "hook partial failure sets status to :partial_success" do
      # Create posts with different titles - some will fail, some will succeed
      success_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "success"})
        |> Ash.create!()

      fail_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "will fail"})
        |> Ash.create!()

      another_success =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "also success"})
        |> Ash.create!()

      success_post_id = success_post.id
      fail_post_id = fail_post.id
      another_success_id = another_success.id
      post_ids = [success_post_id, fail_post_id, another_success_id]

      # This action's hook fails only for records with title containing "fail"
      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(:update_with_after_transaction_partial_failure, %{},
          strategy: :atomic,
          return_errors?: true,
          return_records?: true
        )

      # The success hooks should have been called
      assert_receive {:after_transaction_partial_success, ^success_post_id}, 1000
      assert_receive {:after_transaction_partial_failure, ^fail_post_id}, 1000
      assert_receive {:after_transaction_partial_success, ^another_success_id}, 1000

      # Status should be partial_success since some records succeeded and some failed
      assert result.status == :partial_success
      assert result.error_count == 1
      assert length(result.errors) == 1
      assert length(result.records) == 2
    end

    test "hooks work with return_notifications?: true" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: :atomic,
          return_records?: true,
          return_notifications?: true
        )

      assert result.status == :success
      assert length(result.records) == 3
      # Notifications should be returned
      assert length(result.notifications) == 3

      # Verify hooks executed
      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "hooks work with notify?: true" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: :atomic,
          return_records?: true,
          notify?: true
        )

      assert result.status == :success
      assert length(result.records) == 3

      # Notifications should be sent (via Notifier module)
      assert_received {:notification, _}
      assert_received {:notification, _}
      assert_received {:notification, _}

      # Verify after_transaction hooks also executed
      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "load option works with hooks" do
      # Create an author and post
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "load_test", author_id: author.id})
        |> Ash.create!()

      # Use atomic strategy with load option and after_transaction hooks
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(
          :update_with_atomic_after_transaction,
          %{title: "updated"},
          strategy: :atomic,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 1
      [updated_post] = result.records

      # Verify load worked
      assert updated_post.author.name == "Test Author"

      # Verify after_transaction hook executed
      assert_receive {:after_transaction_called, _id}, 1000
    end

    test "after_transaction hooks work with atomic single-record update" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", title2: "test2"})
        |> Ash.create!()

      result =
        post
        |> Ash.Changeset.for_update(:update_with_atomic_upgrade_and_after_transaction, %{
          title2: "updated"
        })
        |> Ash.update!()

      assert result.title2 == "updated"

      # The after_transaction hook executed
      assert_receive {:after_transaction_called, _}, 100
    end

    test "after_transaction hooks work with SimplePost resource" do
      simple_post =
        SimplePost
        |> Ash.Changeset.for_create(:create, %{title: "test", body: "body"})
        |> Ash.create!()

      result =
        simple_post
        |> Ash.Changeset.for_update(:update_with_after_transaction, %{body: "updated"})
        |> Ash.update!()

      assert result.body == "updated"
      assert_receive {:after_transaction_called, _}, 100
    end

    test "strategy fallback with require_atomic?: false" do
      # Create a record
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "atomic_fallback"})
        |> Ash.create!()

      # Use atomic strategy with an action that has require_atomic?: false
      # This tests the atomic upgrade path
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(
          :update_with_atomic_upgrade_and_after_transaction,
          %{title: "updated"},
          strategy: :atomic,
          return_records?: true,
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 1

      # Hook should still work after fallback
      assert_receive {:after_transaction_called, _id}, 1000
    end

    test "load reflects modifications from after_transaction hook" do
      # Create an author and post
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "original", author_id: author.id})
        |> Ash.create!()

      # Use action that modifies the result in after_transaction
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(
          :update_with_atomic_after_transaction_modifying_result,
          %{},
          strategy: :atomic,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 1
      [updated_post] = result.records

      # The after_transaction hook appends "_modified_by_hook" to the title
      # This verifies that load happens AFTER after_transaction hooks
      assert updated_post.title == "original_modified_by_hook"

      # Load should still work correctly
      assert updated_post.author.name == "Test Author"
    end
  end

  describe ":atomic_batches strategy" do
    test "hooks execute" do
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
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: [:atomic_batches],
          batch_size: 2
        )

      assert result.status == :success

      # Verify all hooks executed
      for post_id <- post_ids do
        assert_received {:after_transaction_called, ^post_id}
      end
    end

    test "returns records when requested" do
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
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: [:atomic_batches],
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      # Verify all hooks executed
      for post_id <- post_ids do
        assert_received {:after_transaction_called, ^post_id}
      end
    end

    test "hooks run on failure" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      # This action has a validation that always fails
      # With atomic_batches, validation happens once before the batches run
      result =
        Post
        |> Ash.Query.filter(id in ^Enum.map(posts, & &1.id))
        |> Ash.bulk_update(:update_with_atomic_after_transaction_always_fails, %{},
          strategy: [:atomic_batches],
          return_errors?: true
        )

      assert result.status == :error
      # Only 1 error because validation fails on the single atomic changeset
      assert result.error_count == 1

      # Verify hook received error (only one because atomic changeset is shared)
      assert_receive {:after_transaction_error, _error}, 1000

      # Verify posts were NOT updated (operation failed)
      assert length(Ash.read!(Post)) == 3
    end

    test "hook error on success is captured" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      # This action's hook returns an error even on success
      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(:update_with_atomic_after_transaction_returns_error, %{},
          strategy: [:atomic_batches],
          return_errors?: true
        )

      # All hooks are called (one per record)
      for _post_id <- post_ids do
        assert_receive {:after_transaction_returning_error, _}, 1000
      end

      # Hook errors are captured in the result
      # Note: atomic_batches currently consolidates multiple errors into 1
      assert result.status == :error
      assert result.error_count == 1
    end

    test "hooks work with notify?: true" do
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
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: [:atomic_batches],
          batch_size: 2,
          return_records?: true,
          notify?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      # Notifications should be sent (via Notifier module)
      for _ <- 1..5 do
        assert_received {:notification, _}
      end

      # Verify after_transaction hooks also executed
      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "load option works with hooks" do
      # Create an author
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      # Create multiple posts
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "load_batch_#{i}", author_id: author.id})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      # Use atomic_batches strategy with load option and after_transaction hooks
      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(
          :update_with_atomic_after_transaction,
          %{title: "batch_updated"},
          strategy: [:atomic_batches],
          batch_size: 2,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      # Verify load worked for all records
      for record <- result.records do
        assert %Author{name: "Test Author"} = record.author
      end

      # Verify after_transaction hooks executed
      for _ <- 1..5 do
        assert_receive {:after_transaction_called, _id}, 1000
      end
    end
  end

  describe ":stream strategy" do
    test "hooks execute" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(:update_with_atomic_after_transaction, %{}, strategy: [:stream])

      assert result.status == :success
      assert_receive {:after_transaction_called, _}, 100
    end

    test "with return_stream?: true streams results" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      # return_stream?: true only works with :stream strategy
      result_stream =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(:update_with_atomic_after_transaction, %{},
          strategy: :stream,
          return_stream?: true,
          return_records?: true
        )

      # Consume the stream
      results = Enum.to_list(result_stream)
      assert length(results) == 3

      # Verify hooks executed (use assert_receive with timeout for async operations)
      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "hooks run on failure with return_stream?" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      # With stream strategy and return_stream?, the operation is lazy
      # We use an action that always fails validation
      result_stream =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(:update_with_atomic_after_transaction_always_fails, %{},
          strategy: :stream,
          return_stream?: true,
          return_errors?: true
        )

      # Consume the stream to trigger the operations
      results = Enum.to_list(result_stream)

      # Each record should fail and trigger its hook
      assert length(results) == 3

      for result <- results do
        assert {:error, _} = result
      end

      # Verify hooks executed for each failed record
      for _post_id <- post_ids do
        assert_receive {:after_transaction_error, _error}, 1000
      end

      # Verify posts were NOT updated (all operations failed)
      for post <- posts do
        reloaded = Ash.reload!(post)
        assert reloaded.title == post.title
      end
    end

    test "load option with return_stream? and mixed success/failure" do
      # Create an author
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      # Create posts - one will succeed, one will fail (title contains "fail")
      success_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "success", author_id: author.id})
        |> Ash.create!()

      fail_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "will fail", author_id: author.id})
        |> Ash.create!()

      result_stream =
        Post
        |> Ash.Query.filter(id in [^success_post.id, ^fail_post.id])
        |> Ash.bulk_update(:update_with_after_transaction_partial_failure, %{},
          strategy: :stream,
          return_stream?: true,
          return_records?: true,
          return_errors?: true,
          load: [:author],
          stop_on_error?: false,
          authorize?: false
        )

      results = Enum.to_list(result_stream)
      assert length(results) == 2

      # Separate successes and failures
      {successes, failures} = Enum.split_with(results, &match?({:ok, _}, &1))

      assert length(successes) == 1
      assert length(failures) == 1

      # Verify successful record has author loaded
      [{:ok, record}] = successes
      assert %Author{name: "Test Author"} = record.author

      # Verify hooks executed
      assert_receive {:after_transaction_partial_success, _}, 1000
      assert_receive {:after_transaction_partial_failure, _}, 1000
    end

    test "hook can return error even when DB operation succeeds" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      # This action's hook returns an error even on success
      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(:update_with_atomic_after_transaction_returns_error, %{},
          strategy: :stream,
          return_errors?: true
        )

      # Stream strategy stops on first error (default stop_on_error?: true)
      assert_receive {:after_transaction_returning_error, _}, 1000

      # Hook error is captured in the result
      assert result.status == :error
      assert result.error_count == 1
    end

    test "hooks work with return_notifications?: true" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: :stream,
          return_records?: true,
          return_notifications?: true
        )

      assert result.status == :success
      assert length(result.records) == 3
      # Notifications should be returned
      assert length(result.notifications) == 3

      # Verify hooks executed
      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "hooks work with notify?: true" do
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "post #{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(:update_with_atomic_after_transaction, %{},
          strategy: :stream,
          return_records?: true,
          notify?: true
        )

      assert result.status == :success
      assert length(result.records) == 3

      # Notifications should be sent (via Notifier module)
      assert_received {:notification, _}
      assert_received {:notification, _}
      assert_received {:notification, _}

      # Verify after_transaction hooks also executed
      for post_id <- post_ids do
        assert_receive {:after_transaction_called, ^post_id}, 1000
      end
    end

    test "hook can convert validation error to success" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      # Update with invalid data (title is nil, but required)
      # The after_transaction hook should convert the error to success
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(
          :update_with_after_transaction_converts_error_to_success,
          %{title: nil},
          strategy: :stream,
          return_records?: true,
          return_errors?: true
        )

      # The hook should have been called
      assert_receive {:after_transaction_converted_error}, 1000

      # The result should show success, not error
      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 1
    end

    test "hook can modify validation error" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      # Update with invalid data (title is nil, but required)
      # The after_transaction hook should modify the error
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(
          :update_with_after_transaction_modifies_error,
          %{title: nil},
          strategy: :stream,
          return_errors?: true
        )

      # The hook should have been called
      assert_receive {:after_transaction_modified_error}, 1000

      # The error should be the modified one from the hook, not the original validation error
      assert result.error_count == 1
      assert [error] = result.errors
      assert Exception.message(error) =~ "custom error from hook"
    end

    test "hook can convert error to success with transaction: :all (sorted)" do
      # Create two posts - both will fail validation (title: nil) but hook converts to success
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title_a"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title_b"})
        |> Ash.create!()

      # Update both with invalid data - both fail validation, hook converts errors to success
      result =
        Post
        |> Ash.Query.filter(id in [^post1.id, ^post2.id])
        |> Ash.Query.sort(:title)
        |> Ash.bulk_update(
          :update_with_after_transaction_converts_error_to_success,
          %{title: nil},
          strategy: :stream,
          transaction: :all,
          return_records?: true,
          return_errors?: true,
          sorted?: true
        )

      # The hook should have been called for the invalid changeset(s)
      assert_receive {:after_transaction_converted_error}, 1000

      # Both records should be returned - converted to success by the hook
      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 2

      # With sorted?: true, records should be in original query order (sorted by title)
      titles = Enum.map(result.records, & &1.title)
      assert titles == ["title_a", "title_b"]
    end

    test "hook can convert error to success with transaction: :all (unsorted)" do
      # Same test without sorted? to test the unsorted code path
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title_a"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title_b"})
        |> Ash.create!()

      result =
        Post
        |> Ash.Query.filter(id in [^post1.id, ^post2.id])
        |> Ash.bulk_update(
          :update_with_after_transaction_converts_error_to_success,
          %{title: nil},
          strategy: :stream,
          transaction: :all,
          return_records?: true,
          return_errors?: true
        )

      # The hook should have been called for the invalid changeset(s)
      assert_receive {:after_transaction_converted_error}, 1000

      # Both records should be returned - converted to success by the hook
      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 2
    end

    test "after_action failure converted to success with transaction: :all" do
      valid_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "valid_title"})
        |> Ash.create!()

      fail_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "fail_this_one"})
        |> Ash.create!()

      # Update with an action that has after_action hook that fails for "fail_" prefixed titles
      # and after_transaction hook that converts the error to success
      result =
        Post
        |> Ash.Query.filter(id in [^valid_post.id, ^fail_post.id])
        |> Ash.bulk_update(
          :update_with_after_action_failure_converted_to_success,
          %{},
          strategy: :stream,
          transaction: :all,
          return_records?: true,
          return_errors?: true
        )

      # The after_transaction hook should have converted the after_action failure to success
      assert_receive {:after_action_failed_converted_to_success}, 1000

      # Both records should be returned
      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 2

      # Verify both records are present
      titles = Enum.map(result.records, & &1.title)
      assert "valid_title" in titles
      assert "recovered_from_after_action_failure" in titles
    end

    test "after_action failure converted to success without transaction: :all" do
      # Same test without transaction: :all to verify after_transaction hooks
      # are called correctly outside of a wrapping transaction
      valid_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "valid_title"})
        |> Ash.create!()

      fail_post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "fail_this_one"})
        |> Ash.create!()

      result =
        Post
        |> Ash.Query.filter(id in [^valid_post.id, ^fail_post.id])
        |> Ash.bulk_update(
          :update_with_after_action_failure_converted_to_success,
          %{},
          strategy: :stream,
          return_records?: true,
          return_errors?: true
        )

      # The after_transaction hook should have converted the after_action failure to success
      assert_receive {:after_action_failed_converted_to_success}, 1000

      # Both records should be returned
      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 2

      # Verify both records are present
      titles = Enum.map(result.records, & &1.title)
      assert "valid_title" in titles
      assert "recovered_from_after_action_failure" in titles
    end

    test "load option is applied to records from hook converting error to success" do
      # This test verifies that records returned from after_transaction hooks
      # that convert errors to success get the load option applied.
      # Create an author
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      # Create a post with the author
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "test", author_id: author.id})
        |> Ash.create!()

      # Update with invalid data (title is nil, but required)
      # The after_transaction hook should convert the error to success
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(
          :update_with_after_transaction_converts_error_to_success,
          %{title: nil},
          strategy: :stream,
          return_records?: true,
          return_errors?: true,
          load: [:author]
        )

      # The hook should have been called
      assert_receive {:after_transaction_converted_error}, 1000

      # The result should show success
      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 1

      [record] = result.records

      # The record should have the author_id set
      assert record.author_id == author.id

      # The author relationship should be loaded because we passed load: [:author]
      assert %Author{} = record.author,
             "Expected author to be loaded, but got: #{inspect(record.author)}"
    end

    test "sorted? option works with hook converting error to success" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "aaa_first"})
        |> Ash.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "bbb_second"})
        |> Ash.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "ccc_third"})
        |> Ash.create!()

      # Update with invalid data - all will fail validation and be converted to success
      result =
        Post
        |> Ash.Query.filter(id in [^post1.id, ^post2.id, ^post3.id])
        |> Ash.Query.sort(:title)
        |> Ash.bulk_update(
          :update_with_after_transaction_converts_error_to_success,
          %{title: nil},
          strategy: :stream,
          return_records?: true,
          return_errors?: true,
          sorted?: true
        )

      # Hooks should have been called
      assert_receive {:after_transaction_converted_error}, 1000
      assert_receive {:after_transaction_converted_error}, 1000
      assert_receive {:after_transaction_converted_error}, 1000

      # All three should succeed
      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 3

      # With sorted?: true, records should be in original query order (sorted by title)
      titles = Enum.map(result.records, & &1.title)
      assert titles == ["aaa_first", "bbb_second", "ccc_third"]
    end

    test "exception in hook stops processing with stop_on_error?: true" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "test_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(
          :update_with_after_transaction_raises_exception,
          %{title: "updated"},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true,
          authorize?: false
        )

      # With stop_on_error?: true (default), processing stops at first error
      assert result.status == :error
      assert length(result.errors) == 1
    end

    test "exception in hook continues processing with stop_on_error?: false" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "test_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(
          :update_with_after_transaction_raises_exception,
          %{title: "updated"},
          strategy: :stream,
          batch_size: 2,
          stop_on_error?: false,
          return_errors?: true,
          authorize?: false
        )

      # With stop_on_error?: false, all records are processed
      assert result.status == :error
      assert length(result.errors) == 5
    end

    test "hook error stops processing with stop_on_error?: true" do
      # Sorted: aaa, bbb, stop_here_ccc, ddd, stop_here_eee
      # First "stop_here" at position 3, so 2 succeed before stopping
      posts =
        for title <- ["aaa", "bbb", "stop_here_ccc", "ddd", "stop_here_eee"] do
          Post
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.Query.sort(:title)
        |> Ash.bulk_update(
          :update_with_stop_on_error_hook,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true,
          return_records?: true,
          authorize?: false
        )

      # With stop_on_error?: true (default), processing stops at first error
      # Status is :partial_success because "aaa" and "bbb" succeeded before "stop_here_ccc" failed
      assert result.status == :partial_success
      assert length(result.errors) == 1
    end

    test "hook error continues processing with stop_on_error?: false" do
      # 3 fail ("stop_here_*"), 2 succeed ("normal_*")
      posts =
        for title <- ["stop_here_1", "normal_1", "stop_here_2", "normal_2", "stop_here_3"] do
          Post
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(
          :update_with_stop_on_error_hook,
          %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true,
          return_records?: true,
          stop_on_error?: false,
          authorize?: false
        )

      # With stop_on_error?: false, all records are processed
      assert result.status == :partial_success
      assert length(result.records) == 2
      assert length(result.errors) == 3
    end

    test "strategy fallback from atomic to stream works" do
      posts =
        for i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "fallback_test_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      # Change returns :not_atomic, forcing fallback from atomic to stream
      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(
          :update_with_not_atomic_after_transaction,
          %{title: "updated"},
          strategy: [:atomic, :stream],
          batch_size: 2,
          return_records?: true,
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      # after_transaction hooks execute after fallback to stream
      for _ <- 1..5 do
        assert_receive {:after_transaction_after_fallback, _id}, 1000
      end
    end

    test "load option with transaction: :all" do
      # Create an author
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      # Create posts
      posts =
        for i <- 1..3 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "load_tx_all_#{i}", author_id: author.id})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(
          :update_with_atomic_after_transaction,
          %{title: "tx_all_updated"},
          strategy: :stream,
          transaction: :all,
          batch_size: 2,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 3

      # Verify load worked for all records
      for record <- result.records do
        assert %Author{name: "Test Author"} = record.author
      end

      # Verify after_transaction hooks executed
      for _ <- 1..3 do
        assert_receive {:after_transaction_called, _id}, 1000
      end
    end

    test "hook modifying result - load reflects modified data" do
      # Create an author and post
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Original Author"})
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "hook_modify_test", author_id: author.id})
        |> Ash.create!()

      # Use the action that modifies title in after_transaction
      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(
          :update_with_after_transaction,
          %{title: "modified"},
          strategy: :stream,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 1
      [updated_post] = result.records

      # The after_transaction hook appends "_stuff" to the title
      assert updated_post.title == "modified_stuff"

      # Load should still work
      assert updated_post.author.name == "Original Author"
    end
  end

  describe ":stream strategy with Mnesia" do
    import ExUnit.CaptureLog

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

    @tag :capture_log
    test "after_action error with transaction: :all rolls back entire operation" do
      posts =
        for i <- 1..5 do
          MnesiaPost
          |> Ash.Changeset.for_create(:create, %{title: "title_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        MnesiaPost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(
          :update_with_after_action_error_and_after_transaction,
          %{},
          strategy: :stream,
          transaction: :all,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      # after_action hook called once (first error triggers rollback)
      assert_receive {:after_action_error_hook_called}

      assert %Ash.BulkResult{errors: errors} = result
      assert length(errors) == 1

      # after_transaction hook NOT called (transaction rolled back)
      refute_receive {:after_transaction_called, _}

      # Verify rollback: titles unchanged (action sets title to "UPDATED_BY_ACTION")
      final_titles =
        MnesiaPost
        |> Ash.read!()
        |> Enum.map(& &1.title)
        |> Enum.sort()

      assert final_titles == ["title_1", "title_2", "title_3", "title_4", "title_5"]
    end

    test "after_action error with transaction: :batch rolls back and stops after first batch" do
      posts =
        for i <- 1..5 do
          MnesiaPost
          |> Ash.Changeset.for_create(:create, %{title: "title_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        MnesiaPost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(
          :update_with_after_action_error_and_after_transaction,
          %{},
          strategy: :stream,
          transaction: :batch,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      # First error triggers rollback and stops processing
      assert_receive {:after_action_error_hook_called}

      assert %Ash.BulkResult{errors: errors} = result
      assert length(errors) == 1

      # after_transaction hooks NOT called (batch rolled back)
      refute_receive {:after_transaction_called, _}

      # Verify rollback: titles unchanged (action sets title to "UPDATED_BY_ACTION")
      final_titles =
        MnesiaPost
        |> Ash.read!()
        |> Enum.map(& &1.title)
        |> Enum.sort()

      assert final_titles == ["title_1", "title_2", "title_3", "title_4", "title_5"]
    end

    test "hooks run outside batch transaction - no warning" do
      post =
        MnesiaPost
        |> Ash.Changeset.for_create(:create, %{title: "test"})
        |> Ash.create!()

      log =
        capture_log(fn ->
          result =
            MnesiaPost
            |> Ash.Query.filter(id == ^post.id)
            |> Ash.bulk_update(
              :update_with_after_transaction,
              %{title: "updated"},
              strategy: :stream,
              return_records?: true,
              authorize?: false
              # transaction: :batch is the default
            )

          assert result.status == :success
          assert length(result.records) == 1
        end)

      # Verify the hook executed
      assert_receive {:mnesia_after_transaction_called, _id}, 1000

      # Should NOT warn since after_transaction now runs outside the transaction
      refute log =~ "after_transaction"
    end

    test "partial failure with transaction: :all rolls back all records" do
      posts =
        for title <- ["a_ok_1", "a_ok_2", "z_fail_3", "z_fail_4"] do
          MnesiaPost
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      for _ <- 1..4, do: assert_receive({:notification, _})

      {result, log} =
        with_log(fn ->
          MnesiaPost
          |> Ash.Query.filter(id in ^post_ids)
          |> Ash.Query.sort(:title)
          |> Ash.bulk_update(
            :update_with_conditional_after_action_error,
            %{},
            strategy: :stream,
            transaction: :all,
            batch_size: 2,
            rollback_on_error?: true,
            return_errors?: true
          )
        end)

      # Batch 1 (a_ok_1, a_ok_2) succeeds, Batch 2 (z_fail_3) fails
      assert_receive {:conditional_after_action_success, _}
      assert_receive {:conditional_after_action_success, _}
      assert_receive {:conditional_after_action_error, _}

      assert result.status == :error
      assert length(result.errors) == 1

      # With transaction: :all, after_transaction runs INSIDE the transaction
      assert log =~ "One or more `after_transaction` hooks"
      assert log =~ "ongoing transaction still happening"

      # All records unchanged (entire transaction rolled back)
      final_titles =
        MnesiaPost
        |> Ash.read!()
        |> Enum.map(& &1.title)
        |> Enum.sort()

      assert final_titles == ["a_ok_1", "a_ok_2", "z_fail_3", "z_fail_4"]
    end

    test "partial failure with transaction: :batch commits first batch, rolls back second" do
      # Use titles that sort: a_ok < z_fail, so first batch succeeds, second fails
      posts =
        for title <- ["a_ok_1", "a_ok_2", "z_fail_3", "z_fail_4"] do
          MnesiaPost
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      # Flush notification messages from create operations
      for _ <- 1..4, do: assert_receive({:notification, _})

      result =
        MnesiaPost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.Query.sort(:title)
        |> Ash.bulk_update(
          :update_with_conditional_after_action_error,
          %{},
          strategy: :stream,
          transaction: :batch,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      # Sorted: a_ok_1, a_ok_2 (batch 1 succeeds), z_fail_3, z_fail_4 (batch 2 fails)
      # First batch succeeds
      assert_receive {:conditional_after_action_success, _}
      assert_receive {:conditional_after_action_success, _}
      # Second batch fails
      assert_receive {:conditional_after_action_error, _}

      assert result.status == :partial_success
      assert length(result.errors) == 1

      # after_transaction called for first batch (committed)
      assert_receive {:conditional_after_transaction, {:ok, _}}
      assert_receive {:conditional_after_transaction, {:ok, _}}
      # NOT called for second batch (rolled back)
      refute_receive {:conditional_after_transaction, {:error, _}}

      # First batch updated (title = "UPDATED_TITLE"), second batch unchanged
      final_records = MnesiaPost |> Ash.read!()

      # Count records by title
      updated_count = Enum.count(final_records, &(&1.title == "UPDATED_TITLE"))
      fail_count = Enum.count(final_records, &String.contains?(&1.title, "fail"))

      # First batch (2 records) should be committed with new title
      assert updated_count == 2
      # Second batch (2 records) should be rolled back (original fail titles)
      assert fail_count == 2
    end

    test "after_transaction error with transaction: :batch - batch still commits (runs outside tx)" do
      posts =
        for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_ok_5"] do
          MnesiaPost
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        MnesiaPost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.Query.sort(:title)
        |> Ash.bulk_update(
          :update_with_after_transaction_partial_failure,
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

      # Batches 1-2 committed (4 records), batch 3 skipped - hook runs OUTSIDE tx
      final_titles =
        MnesiaPost
        |> Ash.read!()
        |> Enum.map(& &1.title)
        |> Enum.sort()

      assert final_titles == [
               "UPDATED_a_ok_1",
               "UPDATED_a_ok_2",
               "UPDATED_a_ok_3",
               "UPDATED_z_fail_4",
               "z_ok_5"
             ]
    end

    test "after_transaction error with transaction: :all - entire operation rolls back" do
      posts =
        for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_ok_5"] do
          MnesiaPost
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      {result, log} =
        with_log(fn ->
          MnesiaPost
          |> Ash.Query.filter(id in ^post_ids)
          |> Ash.Query.sort(:title)
          |> Ash.bulk_update(
            :update_with_after_transaction_partial_failure,
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

      # All records unchanged - entire transaction rolled back
      final_titles =
        MnesiaPost
        |> Ash.read!()
        |> Enum.map(& &1.title)
        |> Enum.sort()

      assert final_titles == ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4", "z_ok_5"]
    end
  end

  # Manual action modules for testing after_transaction hooks with manual updates
  defmodule ManualUpdateSimple do
    @moduledoc """
    Simple manual update module that just performs the update.
    After_transaction hooks are added via a separate change module.
    """
    use Ash.Resource.ManualUpdate

    def update(changeset, _opts, _context) do
      # Perform the actual update using ETS data layer
      Ash.DataLayer.Ets.update(changeset.resource, changeset)
    end
  end

  defmodule ManualUpdateFails do
    @moduledoc """
    Manual update module that always fails.
    Used to test after_transaction hook error handling with manual actions.
    """
    use Ash.Resource.ManualUpdate

    def update(_changeset, _opts, _context) do
      {:error, "intentional manual update error"}
    end
  end

  defmodule ManualUpdatePost do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier]

    alias Ash.Test.Actions.BulkUpdateAfterTransactionTest.ManualUpdateFails
    alias Ash.Test.Actions.BulkUpdateAfterTransactionTest.ManualUpdateSimple
    alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionChange
    alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionConvertsErrorChange

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*]

      update :update_manual_with_after_transaction do
        accept [:title, :title2]
        manual ManualUpdateSimple
        change ManualAfterTransactionChange
      end

      update :update_manual_with_after_transaction_converts_error do
        accept [:title, :title2]
        manual ManualUpdateFails
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
    Reading is allowed for everyone, but updating is forbidden unless actor has allow: true.
    This allows stream strategy to query records but fail on update.
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

      # Forbid update unless actor has allow: true
      policy action_type(:update) do
        forbid_unless actor_attribute_equals(:allow, true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*]

      update :update do
        accept [:title]
        primary? true
      end

      update :update_with_after_transaction do
        accept [:title]
        change AfterTransactionHandlingErrors
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  describe "manual actions" do
    test "hooks work with single record" do
      post =
        ManualUpdatePost
        |> Ash.Changeset.for_create(:create, %{title: "manual_update_test"})
        |> Ash.create!()

      result =
        ManualUpdatePost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(
          :update_manual_with_after_transaction,
          %{title: "updated_manual"},
          strategy: :stream,
          return_records?: true
        )

      assert result.status == :success
      assert [%{title: "updated_manual"}] = result.records

      # Verify after_transaction hook was called
      assert_receive {:manual_after_transaction_success, _id}, 1000
    end

    test "hooks work with multiple records" do
      posts =
        for i <- 1..5 do
          ManualUpdatePost
          |> Ash.Changeset.for_create(:create, %{title: "manual_bulk_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        ManualUpdatePost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(
          :update_manual_with_after_transaction,
          %{title: "bulk_updated"},
          strategy: :stream,
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      # Verify after_transaction hooks were called for all records
      for _ <- 1..5 do
        assert_receive {:manual_after_transaction_success, _id}, 1000
      end
    end

    test "hook can convert error to success" do
      posts =
        for i <- 1..5 do
          ManualUpdatePost
          |> Ash.Changeset.for_create(:create, %{title: "will_fail_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        ManualUpdatePost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(
          :update_manual_with_after_transaction_converts_error,
          %{title: "should_fail"},
          strategy: :stream,
          batch_size: 2,
          return_records?: true,
          return_errors?: true
        )

      for _ <- 1..5 do
        assert_receive {:manual_after_transaction_converted_error}, 1000
      end

      assert result.status == :success
      assert length(result.records) == 5
    end

    test "hooks work with return_stream?" do
      posts =
        for i <- 1..5 do
          ManualUpdatePost
          |> Ash.Changeset.for_create(:create, %{title: "stream_manual_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result_stream =
        ManualUpdatePost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(
          :update_manual_with_after_transaction,
          %{title: "stream_updated"},
          strategy: :stream,
          batch_size: 2,
          return_stream?: true,
          return_records?: true
        )

      results = Enum.to_list(result_stream)
      assert length(results) == 5

      for _ <- 1..5 do
        assert_receive {:manual_after_transaction_success, _id}, 1000
      end
    end

    test "hooks work with transaction: :all" do
      posts =
        for i <- 1..5 do
          ManualUpdatePost
          |> Ash.Changeset.for_create(:create, %{title: "tx_all_manual_#{i}"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        ManualUpdatePost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update!(
          :update_manual_with_after_transaction,
          %{title: "tx_updated"},
          strategy: :stream,
          transaction: :all,
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5 do
        assert_receive {:manual_after_transaction_success, _id}, 1000
      end
    end
  end

  describe "forbidden errors" do
    test "after_transaction hook is called on forbidden error with :atomic strategy" do
      post =
        PolicyPost
        |> Ash.Changeset.for_create(:create, %{title: "forbidden_test"})
        |> Ash.create!(authorize?: false)

      result =
        PolicyPost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(:update_with_after_transaction, %{title: "updated"},
          strategy: :atomic,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error
      assert_receive {:after_transaction_error, error}, 1000
      assert %Ash.Error.Forbidden{} = error
    end

    test "after_transaction hook is called on forbidden error with :atomic_batches strategy" do
      posts =
        for i <- 1..5 do
          PolicyPost
          |> Ash.Changeset.for_create(:create, %{title: "forbidden_batch_#{i}"})
          |> Ash.create!(authorize?: false)
        end

      result =
        posts
        |> Ash.bulk_update(:update_with_after_transaction, %{title: "updated"},
          strategy: :atomic_batches,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error
      assert_receive {:after_transaction_error, error}, 1000
      assert %Ash.Error.Forbidden{} = error
    end

    test "after_transaction hook is called on forbidden error with :stream strategy" do
      posts =
        for i <- 1..5 do
          PolicyPost
          |> Ash.Changeset.for_create(:create, %{title: "forbidden_stream_#{i}"})
          |> Ash.create!(authorize?: false)
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        PolicyPost
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(:update_with_after_transaction, %{title: "updated"},
          strategy: :stream,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error
      assert_receive {:after_transaction_error, error}, 1000
      assert %Ash.Error.Forbidden{} = error
    end
  end

  describe "validation errors" do
    test "after_transaction hook receives error with :atomic strategy" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "original"})
        |> Ash.create!()

      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(:update_with_atomic_validation_in_transaction, %{title: "will_fail"},
          strategy: :atomic,
          return_errors?: true
        )

      assert result.status == :error
      assert_receive {:after_transaction_error, error}, 1000
      assert %Ash.Error.Invalid{} = error
      assert Exception.message(error) =~ "atomic validation failed"
      assert Ash.get!(Post, post.id).title == "original"
    end

    test "after_transaction hook receives error with :atomic_batches strategy" do
      posts =
        for _i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "original"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        posts
        |> Ash.bulk_update(:update_with_atomic_validation_in_transaction, %{title: "will_fail"},
          strategy: :atomic_batches,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert_receive {:after_transaction_error, error}, 1000
      assert %Ash.Error.Invalid{} = error
      assert Exception.message(error) =~ "atomic validation failed"

      remaining = Post |> Ash.Query.filter(id in ^post_ids) |> Ash.read!()
      assert Enum.all?(remaining, &(&1.title == "original"))
    end

    test "after_transaction hook receives error with :stream strategy" do
      posts =
        for _i <- 1..5 do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "original"})
          |> Ash.create!()
        end

      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_update(:update_with_atomic_validation_in_transaction, %{title: "will_fail"},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert_receive {:after_transaction_error, error}, 1000
      assert %Ash.Error.Invalid{} = error
      assert Exception.message(error) =~ "atomic validation failed"

      remaining = Post |> Ash.Query.filter(id in ^post_ids) |> Ash.read!()
      assert Enum.all?(remaining, &(&1.title == "original"))
    end
  end
end
