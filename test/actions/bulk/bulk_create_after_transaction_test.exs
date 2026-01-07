# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkCreateAfterTransactionTest do
  @moduledoc """
  Tests for after_transaction hooks in bulk create operations.

  Note: Uses `async: false` because of Mnesia.

  Tests are organized by execution mode:
  - `transaction: :batch` - Default batch transaction mode
  - `transaction: :all` - Single transaction wrapping all records
  - `return_stream?` - Streaming mode
  - `upsert` - Upsert-specific tests
  - `manual actions` - Manual action tests
  - `forbidden errors` - Authorization error scenarios
  - `validation errors` - Validation error scenarios
  - `mnesia` - Mnesia data layer tests
  """
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Ash.Test.Domain, as: Domain

  # Shared change modules (alphabetically ordered)
  alias Ash.Test.BulkAfterTransaction.AfterActionFailsWithAfterTransaction
  alias Ash.Test.BulkAfterTransaction.AfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.AfterTransactionConvertsErrorToSuccess
  alias Ash.Test.BulkAfterTransaction.AfterTransactionFailsForSomeRecords
  alias Ash.Test.BulkAfterTransaction.AfterTransactionHandlingErrors
  alias Ash.Test.BulkAfterTransaction.AfterTransactionRaisesException
  alias Ash.Test.BulkAfterTransaction.ConditionalAfterActionErrorWithAfterTransaction
  alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.MnesiaAfterTransactionChange
  alias Ash.Test.BulkAfterTransaction.MultipleAfterTransactionHooks
  alias Ash.Test.BulkAfterTransaction.Notifier

  # ===========================================================================
  # CREATE-SPECIFIC MODULES (not shared)
  # ===========================================================================

  defmodule ValidationFailsForCreate do
    @moduledoc """
    Validation that fails when title == "will_fail".
    For create actions, uses validate/3 callback since creates can't be atomic.
    """
    use Ash.Resource.Validation

    @impl true
    def validate(changeset, _opts, _context) do
      title = Ash.Changeset.get_attribute(changeset, :title)

      if title == "will_fail" do
        {:error, field: :title, message: "validation failed for title"}
      else
        :ok
      end
    end
  end

  defmodule ManualCreateSimple do
    @moduledoc """
    Simple manual create module that just performs the create.
    After_transaction hooks are added via a separate change module.
    """
    use Ash.Resource.ManualCreate

    def create(changeset, _opts, _context) do
      # Perform the actual create using ETS data layer
      Ash.DataLayer.Ets.create(changeset.resource, changeset)
    end

    def bulk_create(changesets, _opts, _context) do
      Enum.map(changesets, fn changeset ->
        case Ash.DataLayer.Ets.create(changeset.resource, changeset) do
          {:ok, record} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_create_index,
                changeset.context.bulk_create.index
              )

            {:ok, record}

          {:error, error} ->
            {:error, error}
        end
      end)
    end
  end

  defmodule ManualCreateConditionalFail do
    @moduledoc """
    Manual create module that fails when title contains "fail".
    Used to test after_transaction hook error handling with manual actions.
    """
    use Ash.Resource.ManualCreate

    def create(changeset, _opts, _context) do
      title = Ash.Changeset.get_attribute(changeset, :title)

      if String.contains?(title || "", "fail") do
        {:error, "intentional manual create error for title containing fail"}
      else
        {:ok,
         %{
           changeset.data
           | id: Ash.UUID.generate(),
             title: title,
             title2: Ash.Changeset.get_attribute(changeset, :title2),
             org_id: Ash.Changeset.get_attribute(changeset, :org_id)
         }}
      end
    end
  end

  # ===========================================================================
  # RESOURCES
  # ===========================================================================

  defmodule Org do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      default_accept :*
      defaults create: :*
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
      has_many :posts, Ash.Test.Actions.BulkCreateAfterTransactionTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier]

    alias Ash.Test.Actions.BulkCreateAfterTransactionTest.Org

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :org_id
      global? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      create :create_with_after_transaction do
        change after_transaction(fn
                 _changeset, {:ok, result}, _context ->
                   {:ok, %{result | title: result.title <> "_stuff"}}

                 _changeset, {:error, error}, _context ->
                   send(self(), {:error, error})
                   {:error, error}
               end)
      end

      create :create_with_after_transaction_hook do
        change AfterTransactionChange
      end

      create :create_with_after_transaction_returns_error do
        change AfterTransactionFailsForSomeRecords
      end

      create :create_with_after_transaction_converts_error_to_success do
        change AfterTransactionConvertsErrorToSuccess
      end

      create :create_with_multiple_hooks do
        change MultipleAfterTransactionHooks
      end

      create :create_with_after_action_failure_converted_to_success do
        change AfterActionFailsWithAfterTransaction
      end

      create :create_with_after_transaction_raises_exception do
        change AfterTransactionRaisesException
      end

      create :upsert_with_after_transaction do
        upsert? true
        upsert_identity :unique_title
        upsert_fields [:title2]

        change after_transaction(fn
                 _changeset, {:ok, result}, _context ->
                   send(self(), {:upsert_after_transaction_called, result.id, result.title})
                   {:ok, result}

                 _changeset, {:error, error}, _context ->
                   send(self(), {:upsert_after_transaction_error, error})
                   {:error, error}
               end)
      end

      create :upsert_with_condition_and_after_transaction do
        upsert? true
        upsert_identity :unique_title
        upsert_fields [:title2]
        upsert_condition expr(false)

        change after_transaction(fn
                 _changeset, {:ok, result}, _context ->
                   send(
                     self(),
                     {:upsert_skipped_after_transaction_called, result.id, result.title}
                   )

                   {:ok, result}

                 _changeset, {:error, error}, _context ->
                   {:error, error}
               end)
      end

      create :create_with_validation_and_after_transaction do
        validate ValidationFailsForCreate
        change AfterTransactionHandlingErrors
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
        allow_nil?(false)
      end

      attribute :title2, :string do
        public?(true)
      end

      attribute :org_id, :uuid do
        public?(true)
      end
    end

    relationships do
      belongs_to :org, Org, public?: true, attribute_writable?: true
      belongs_to :author, Author, public?: true
    end

    identities do
      identity :unique_title, :title do
        pre_check_with Ash.Test.Domain
      end
    end
  end

  defmodule MnesiaPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :mnesia_post_after_transaction_creates
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create]

      create :create_with_after_action_error_and_after_transaction do
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

      create :create_with_after_transaction do
        change MnesiaAfterTransactionChange
      end

      create :create_with_conditional_after_action_error do
        change ConditionalAfterActionErrorWithAfterTransaction
      end

      create :create_with_after_transaction_partial_failure do
        change AfterTransactionFailsForSomeRecords
      end
    end
  end

  defmodule ManualPost do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier]

    alias Ash.Test.Actions.BulkCreateAfterTransactionTest.ManualCreateConditionalFail
    alias Ash.Test.Actions.BulkCreateAfterTransactionTest.ManualCreateSimple
    alias Ash.Test.Actions.BulkCreateAfterTransactionTest.Org
    alias Ash.Test.BulkAfterTransaction.AfterTransactionConvertsErrorToSuccess
    alias Ash.Test.BulkAfterTransaction.ManualAfterTransactionChange

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :org_id
      global? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy]

      create :create do
        accept [:title, :title2, :org_id]
      end

      create :create_manual_with_after_transaction do
        accept [:title, :title2, :org_id]
        manual ManualCreateSimple
        change ManualAfterTransactionChange
      end

      create :create_manual_with_after_transaction_converts_error do
        accept [:title, :title2, :org_id]
        manual ManualCreateConditionalFail
        change AfterTransactionConvertsErrorToSuccess
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
        allow_nil?(false)
      end

      attribute :title2, :string do
        public?(true)
      end

      attribute :org_id, :uuid do
        public?(true)
      end
    end

    relationships do
      belongs_to :org, Org, public?: true, attribute_writable?: true
    end
  end

  defmodule PolicyPost do
    @moduledoc """
    Resource with policies for testing forbidden error scenarios.
    Reading is allowed for everyone, but creating is forbidden unless actor has allow: true.
    """
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    policies do
      # Allow reading for everyone
      policy action_type(:read) do
        authorize_if always()
      end

      # Forbid create unless actor has allow: true
      policy action_type(:create) do
        forbid_unless actor_attribute_equals(:allow, true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy]

      create :create do
        accept [:title]
        primary? true
      end

      create :create_with_after_transaction do
        accept [:title]
        change AfterTransactionHandlingErrors
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  setup do
    %{org: Ash.create!(Org, %{})}
  end

  describe "transaction: :batch" do
    test "hooks handle empty result set", %{org: org} do
      result =
        Ash.bulk_create(
          [],
          Post,
          :create_with_after_transaction_hook,
          tenant: org.id,
          return_records?: true,
          authorize?: false
        )

      assert result.status == :success
      assert result.records == []
      refute_receive {:after_transaction_called, _}
    end

    test "hook error is captured in result", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "z_fail_4"},
            %{title: "z_fail_5"}
          ],
          Post,
          :create_with_after_transaction_returns_error,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      # 4 records processed: 3 succeed, 1 fails (z_fail_5 never reached due to stop_on_error)
      for _ <- 1..3, do: assert_receive({:after_transaction_partial_success, _})
      assert_receive {:after_transaction_partial_failure, _}
      refute_receive {:after_transaction_partial_failure, _}

      assert result.status == :partial_success
      assert result.error_count == 1
      assert length(result.errors) == 1

      # Verify records committed to DB (after_transaction runs after commit)
      posts = Post |> Ash.Query.sort(:title) |> Ash.read!(tenant: org.id)
      assert length(posts) == 4
      assert Enum.map(posts, & &1.title) == ["a_ok_1", "a_ok_2", "a_ok_3", "z_fail_4"]
    end

    test "multiple hooks execute in order", %{org: org} do
      result =
        Ash.bulk_create!(
          [%{title: "test"}],
          Post,
          :create_with_multiple_hooks,
          tenant: org.id,
          return_records?: true,
          authorize?: false
        )

      assert result.status == :success

      assert_receive {:hook_1_executed, id, order1}, 1000
      assert_receive {:hook_2_executed, ^id, order2}, 1000
      assert order1 < order2, "Expected hook_1 to execute before hook_2"
    end

    test "hooks work with return_notifications?: true", %{org: org} do
      result =
        Ash.bulk_create!(
          [
            %{title: "test1"},
            %{title: "test2"},
            %{title: "test3"},
            %{title: "test4"},
            %{title: "test5"}
          ],
          Post,
          :create_with_after_transaction_hook,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_notifications?: true,
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:after_transaction_called, _})

      assert length(result.notifications) == 5
    end

    test "hooks work with notify?: true", %{org: org} do
      result =
        Ash.bulk_create!(
          [
            %{title: "test1"},
            %{title: "test2"},
            %{title: "test3"},
            %{title: "test4"},
            %{title: "test5"}
          ],
          Post,
          :create_with_after_transaction_hook,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          notify?: true,
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:after_transaction_called, _})

      for i <- 1..5 do
        title = "test#{i}"
        assert_received {:notification, %{data: %{title: ^title}}}
      end
    end

    test "with both return_records?: true and return_errors?: true", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "test1"},
            %{title: "test2"},
            %{title: "test3"},
            %{title: "test4"},
            %{title: "test5"}
          ],
          Post,
          :create_with_after_transaction_hook,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5
      assert result.errors == []

      for _ <- 1..5, do: assert_receive({:after_transaction_called, _})
    end

    test "hooks execute even if return_records? is not set", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "test1"},
            %{title: "test2"},
            %{title: "test3"},
            %{title: "test4"},
            %{title: "test5"}
          ],
          Post,
          :create_with_after_transaction_hook,
          tenant: org.id,
          batch_size: 2,
          authorize?: false
        )

      assert result.status == :success

      for _ <- 1..5, do: assert_receive({:after_transaction_called, _})

      assert length(Ash.read!(Post, tenant: org.id)) == 5
    end

    test "hook can convert validation error to success", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: nil},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_transaction_converts_error_to_success,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})
      assert_receive {:after_transaction_converted_error}

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5
      assert Enum.any?(result.records, &(&1.title == "recovered_from_error"))

      # Only 4 records in DB - the fake "recovered_from_error" wasn't persisted
      assert length(Ash.read!(Post, tenant: org.id)) == 4
    end

    test "after_action failure converted to success without transaction: :all", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "fail_this_4"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_action_failure_converted_to_success,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      assert_receive {:after_action_failed_converted_to_success}, 1000

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5
      assert Enum.any?(result.records, &(&1.title == "recovered_from_after_action_failure"))

      # All 5 records in DB - ETS doesn't rollback on after_action failure
      assert length(Ash.read!(Post, tenant: org.id)) == 5
    end

    test "load option is applied to records from after_transaction hooks (mixed success and converted)",
         %{org: org} do
      author = Ash.create!(Author, %{name: "Test Author"})

      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1", author_id: author.id},
            %{title: "a_ok_2", author_id: author.id},
            %{title: "a_ok_3", author_id: author.id},
            %{title: nil, author_id: author.id},
            %{title: "a_ok_5", author_id: author.id}
          ],
          Post,
          :create_with_after_transaction_converts_error_to_success,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          authorize?: false,
          load: [:author]
        )

      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})
      assert_receive {:after_transaction_converted_error}

      assert result.status == :success
      assert length(result.records) == 5

      for record <- result.records do
        assert %Author{name: "Test Author"} = record.author
      end

      assert Enum.any?(result.records, &(&1.title == "recovered_from_error"))
    end

    test "sorted? option works with after_transaction converting error to success", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: nil},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_transaction_converts_error_to_success,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          authorize?: false,
          sorted?: true
        )

      assert_receive {:after_transaction_converted_error}
      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5

      titles = Enum.map(result.records, & &1.title)
      assert titles == ["a_ok_1", "a_ok_2", "a_ok_3", "recovered_from_error", "a_ok_5"]
    end

    test "exception in hook is caught and converted to error", %{org: org} do
      result =
        Ash.bulk_create(
          [%{title: "z_fail_1"}],
          Post,
          :create_with_after_transaction_raises_exception,
          tenant: org.id,
          return_errors?: true,
          return_records?: true
        )

      assert_receive {:before_exception_raise, _}

      assert result.status == :error
      assert result.error_count == 1
      assert [error] = result.errors
      assert Exception.message(error) =~ "Hook intentionally raised exception"
    end

    test "hook raising exception doesn't crash bulk operation with multiple records", %{org: org} do
      result =
        Ash.bulk_create(
          [%{title: "z_fail_1"}, %{title: "z_fail_2"}, %{title: "z_fail_3"}],
          Post,
          :create_with_after_transaction_raises_exception,
          tenant: org.id,
          stop_on_error?: false,
          return_errors?: true,
          return_records?: true
        )

      for _ <- 1..3, do: assert_receive({:before_exception_raise, _})

      assert result.status == :error
      assert result.error_count == 3
    end

    test "stop_on_error?: true stops processing immediately when hook raises exception",
         %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "z_fail_4"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_transaction_raises_exception,
          tenant: org.id,
          batch_size: 2,
          return_errors?: true,
          return_records?: true
        )

      for _ <- 1..3, do: assert_receive({:after_transaction_success, _})
      assert_receive {:before_exception_raise, _}
      refute_receive {:before_exception_raise, _}, 100

      assert result.status == :partial_success
      assert result.error_count == 1
    end

    test "hook returning error is captured in result", %{org: org} do
      result =
        Ash.bulk_create(
          [%{title: "z_fail_1"}],
          Post,
          :create_with_after_transaction_returns_error,
          tenant: org.id,
          return_errors?: true,
          return_records?: true
        )

      assert_receive {:after_transaction_partial_failure, _}

      assert result.status == :error
      assert result.error_count == 1
    end

    test "hook returns error stops processing with default stop_on_error?: true", %{org: org} do
      result =
        Ash.bulk_create(
          [%{title: "a_ok_1"}, %{title: "z_fail_2"}, %{title: "a_ok_3"}],
          Post,
          :create_with_after_transaction_returns_error,
          tenant: org.id,
          return_errors?: true,
          return_records?: true
        )

      assert_receive {:after_transaction_partial_success, _}
      assert_receive {:after_transaction_partial_failure, _}
      refute_receive {:after_transaction_partial_success, _}, 100

      assert result.status == :partial_success
      assert result.error_count == 1
      assert [_error] = result.errors
    end

    test "stop_on_error?: false continues after hook error across batches", %{org: org} do
      result =
        Ash.bulk_create(
          [%{title: "a_ok_1"}, %{title: "z_fail_2"}, %{title: "a_ok_3"}],
          Post,
          :create_with_after_transaction_returns_error,
          tenant: org.id,
          batch_size: 1,
          stop_on_error?: false,
          return_errors?: true,
          return_records?: true
        )

      for _ <- 1..2, do: assert_receive({:after_transaction_partial_success, _})
      assert_receive {:after_transaction_partial_failure, _}

      assert result.status == :partial_success
      assert result.error_count == 1
      assert length(result.records) == 2
    end
  end

  describe "transaction: :all" do
    test "hooks work with transaction: :all on success", %{org: org} do
      result =
        Ash.bulk_create!(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "a_ok_4"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_transaction_hook,
          tenant: org.id,
          transaction: :all,
          batch_size: 2,
          return_records?: true,
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:after_transaction_called, _})
    end

    test "hook can convert error to success with transaction: :all and mixed valid/invalid changesets (sorted)",
         %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: nil},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_transaction_converts_error_to_success,
          tenant: org.id,
          transaction: :all,
          batch_size: 2,
          sorted?: true,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      assert_receive {:after_transaction_converted_error}
      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})

      assert result.status == :success
      assert result.error_count == 0
      titles = Enum.map(result.records, & &1.title)
      assert titles == ["a_ok_1", "a_ok_2", "a_ok_3", "recovered_from_error", "a_ok_5"]
    end

    test "hook can convert error to success with transaction: :all and mixed valid/invalid changesets (unsorted)",
         %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: nil},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_transaction_converts_error_to_success,
          tenant: org.id,
          transaction: :all,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      assert_receive {:after_transaction_converted_error}
      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5
      assert Enum.any?(result.records, &(&1.title == "recovered_from_error"))
    end

    test "after_action failure converted to success in run_batch with mixed valid/invalid",
         %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "fail"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_action_failure_converted_to_success,
          tenant: org.id,
          transaction: :all,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      assert_receive {:after_action_failed_converted_to_success}, 1000

      assert result.status == :success
      assert result.error_count == 0
      assert length(result.records) == 5

      titles = Enum.map(result.records, & &1.title)
      assert "recovered_from_after_action_failure" in titles
      for title <- ["a_ok_1", "a_ok_2", "a_ok_3", "a_ok_5"], do: assert(title in titles)
    end

    test "load option with transaction: :all and after_transaction hooks", %{org: org} do
      author = Ash.create!(Author, %{name: "Test Author"})

      result =
        Ash.bulk_create!(
          [
            %{title: "a_ok_1", author_id: author.id},
            %{title: "a_ok_2", author_id: author.id},
            %{title: "a_ok_3", author_id: author.id},
            %{title: "a_ok_4", author_id: author.id},
            %{title: "a_ok_5", author_id: author.id}
          ],
          Post,
          :create_with_after_transaction_hook,
          tenant: org.id,
          transaction: :all,
          batch_size: 2,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:after_transaction_called, _})

      for record <- result.records do
        assert %Author{name: "Test Author"} = record.author
      end
    end
  end

  describe "return_stream?" do
    test "hooks work with return_stream?", %{org: org} do
      result_stream =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "a_ok_4"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_transaction_hook,
          tenant: org.id,
          batch_size: 2,
          return_stream?: true,
          return_records?: true,
          authorize?: false
        )

      results = Enum.to_list(result_stream)
      assert length(results) == 5

      for _ <- 1..5, do: assert_receive({:after_transaction_called, _})
    end

    test "hooks work with return_stream? on failure", %{org: org} do
      result_stream =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "z_fail_4"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_after_transaction_returns_error,
          tenant: org.id,
          batch_size: 2,
          return_stream?: true,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      results = Enum.to_list(result_stream)

      successes = Enum.filter(results, &match?({:ok, _}, &1))
      errors = Enum.filter(results, &match?({:error, _}, &1))

      assert length(successes) == 4
      assert length(errors) == 1

      for _ <- 1..4, do: assert_receive({:after_transaction_partial_success, _})
      assert_receive {:after_transaction_partial_failure, _}
    end

    test "load option with return_stream? and after_transaction hooks", %{org: org} do
      author = Ash.create!(Author, %{name: "Test Author"})

      result_stream =
        Ash.bulk_create(
          [
            %{title: "a_ok_1", author_id: author.id},
            %{title: "a_ok_2", author_id: author.id},
            %{title: "a_ok_3", author_id: author.id},
            %{title: nil, author_id: author.id},
            %{title: "a_ok_5", author_id: author.id}
          ],
          Post,
          :create_with_after_transaction_converts_error_to_success,
          tenant: org.id,
          batch_size: 2,
          return_stream?: true,
          return_records?: true,
          load: [:author],
          authorize?: false
        )

      results = Enum.to_list(result_stream)
      assert length(results) == 5

      assert_receive {:after_transaction_converted_error}
      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})

      for {:ok, record} <- results do
        assert %Author{name: "Test Author"} = record.author
      end
    end
  end

  describe "upsert" do
    test "hooks execute on upsert (insert and update paths)", %{org: org} do
      result1 =
        Ash.bulk_create!(
          [
            %{title: "a_ok_1", title2: "original"},
            %{title: "a_ok_2", title2: "original"},
            %{title: "a_ok_3", title2: "original"},
            %{title: "a_ok_4", title2: "original"},
            %{title: "a_ok_5", title2: "original"}
          ],
          Post,
          :upsert_with_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true
        )

      assert result1.status == :success
      original_ids = Enum.map(result1.records, & &1.id) |> Enum.sort()
      for _ <- 1..5, do: assert_receive({:upsert_after_transaction_called, _, _})

      result2 =
        Ash.bulk_create!(
          [
            %{title: "a_ok_1", title2: "updated"},
            %{title: "a_ok_2", title2: "updated"},
            %{title: "a_ok_3", title2: "updated"},
            %{title: "a_ok_4", title2: "updated"},
            %{title: "a_ok_5", title2: "updated"}
          ],
          Post,
          :upsert_with_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true
        )

      assert result2.status == :success
      updated_ids = Enum.map(result2.records, & &1.id) |> Enum.sort()

      assert original_ids == updated_ids
      assert Enum.all?(result2.records, &(&1.title2 == "updated"))

      for _ <- 1..5, do: assert_receive({:upsert_after_transaction_called, _, _})
    end

    test "hooks execute on upsert (mixed insert and update in same batch)", %{org: org} do
      result1 =
        Ash.bulk_create!(
          [
            %{title: "a_existing_1", title2: "original"},
            %{title: "a_existing_2", title2: "original"},
            %{title: "a_existing_3", title2: "original"}
          ],
          Post,
          :upsert_with_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true
        )

      assert result1.status == :success
      original_ids = MapSet.new(result1.records, & &1.id)
      for _ <- 1..3, do: assert_receive({:upsert_after_transaction_called, _, _})

      result2 =
        Ash.bulk_create!(
          [
            %{title: "a_existing_1", title2: "updated"},
            %{title: "b_new_1", title2: "fresh"},
            %{title: "a_existing_2", title2: "updated"},
            %{title: "b_new_2", title2: "fresh"},
            %{title: "a_existing_3", title2: "updated"}
          ],
          Post,
          :upsert_with_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true
        )

      assert result2.status == :success
      assert length(result2.records) == 5

      updated_posts = Enum.filter(result2.records, &String.starts_with?(&1.title, "a_existing"))
      new_posts = Enum.filter(result2.records, &String.starts_with?(&1.title, "b_new"))

      assert length(updated_posts) == 3
      assert length(new_posts) == 2
      assert Enum.all?(updated_posts, &MapSet.member?(original_ids, &1.id))
      assert Enum.all?(updated_posts, &(&1.title2 == "updated"))
      assert Enum.all?(new_posts, &(&1.title2 == "fresh"))

      for _ <- 1..5, do: assert_receive({:upsert_after_transaction_called, _, _})
    end

    test "upsert_skipped with mixed insert and skipped in same batch", %{org: org} do
      result1 =
        Ash.bulk_create!(
          [
            %{title: "a_skip_1", title2: "original"},
            %{title: "a_skip_2", title2: "original"},
            %{title: "a_skip_3", title2: "original"}
          ],
          Post,
          :upsert_with_condition_and_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true
        )

      assert result1.status == :success
      original_ids = MapSet.new(result1.records, & &1.id)
      for _ <- 1..3, do: assert_receive({:upsert_skipped_after_transaction_called, _, _})

      result2 =
        Ash.bulk_create!(
          [
            %{title: "a_skip_1", title2: "should_not_update"},
            %{title: "b_new_1", title2: "fresh"},
            %{title: "a_skip_2", title2: "should_not_update"},
            %{title: "b_new_2", title2: "fresh"},
            %{title: "a_skip_3", title2: "should_not_update"}
          ],
          Post,
          :upsert_with_condition_and_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true
        )

      assert result2.status == :success
      assert length(result2.records) == 2
      assert Enum.all?(result2.records, &String.starts_with?(&1.title, "b_new"))

      for id <- original_ids do
        refreshed = Ash.get!(Post, id, tenant: org.id)
        assert refreshed.title2 == "original"
      end

      for _ <- 1..2, do: assert_receive({:upsert_skipped_after_transaction_called, _, _})
      refute_receive {:upsert_skipped_after_transaction_called, _, _}
    end

    test "upsert with return_skipped_upsert?: true and hooks", %{org: org} do
      result1 =
        Ash.bulk_create!(
          [
            %{title: "a_skip_1", title2: "original"},
            %{title: "a_skip_2", title2: "original"},
            %{title: "a_skip_3", title2: "original"}
          ],
          Post,
          :upsert_with_condition_and_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true
        )

      assert result1.status == :success
      original_ids = MapSet.new(result1.records, & &1.id)
      for _ <- 1..3, do: assert_receive({:upsert_skipped_after_transaction_called, _, _})

      result2 =
        Ash.bulk_create!(
          [
            %{title: "a_skip_1", title2: "should_not_update"},
            %{title: "b_new_1", title2: "fresh"},
            %{title: "a_skip_2", title2: "should_not_update"},
            %{title: "b_new_2", title2: "fresh"},
            %{title: "a_skip_3", title2: "should_not_update"}
          ],
          Post,
          :upsert_with_condition_and_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_skipped_upsert?: true
        )

      assert result2.status == :success
      assert length(result2.records) == 5

      skipped_posts = Enum.filter(result2.records, &String.starts_with?(&1.title, "a_skip"))
      new_posts = Enum.filter(result2.records, &String.starts_with?(&1.title, "b_new"))

      assert length(skipped_posts) == 3
      assert length(new_posts) == 2
      assert Enum.all?(skipped_posts, &MapSet.member?(original_ids, &1.id))
      assert Enum.all?(skipped_posts, &(&1.title2 == "original"))
      assert Enum.all?(new_posts, &(&1.title2 == "fresh"))

      for _ <- 1..5, do: assert_receive({:upsert_skipped_after_transaction_called, _, _})
    end
  end

  describe "manual actions" do
    test "after_transaction hooks work with manual action", %{org: org} do
      result =
        Ash.bulk_create!(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "a_ok_4"},
            %{title: "a_ok_5"}
          ],
          ManualPost,
          :create_manual_with_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:manual_after_transaction_success, _})
    end

    test "after_transaction hook can convert error to success in manual action", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "z_fail_4"},
            %{title: "a_ok_5"}
          ],
          ManualPost,
          :create_manual_with_after_transaction_converts_error,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_errors?: true
        )

      assert_receive {:after_transaction_converted_error}
      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})

      assert result.status == :success
      assert length(result.records) == 5
      assert Enum.any?(result.records, &(&1.title == "recovered_from_error"))
    end

    test "manual action after_transaction hooks work with return_stream?", %{org: org} do
      result_stream =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "a_ok_4"},
            %{title: "a_ok_5"}
          ],
          ManualPost,
          :create_manual_with_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_stream?: true,
          return_records?: true
        )

      results = Enum.to_list(result_stream)
      assert length(results) == 5

      for _ <- 1..5, do: assert_receive({:manual_after_transaction_success, _})
    end

    test "manual action after_transaction hooks work with transaction: :all", %{org: org} do
      result =
        Ash.bulk_create!(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "a_ok_4"},
            %{title: "a_ok_5"}
          ],
          ManualPost,
          :create_manual_with_after_transaction,
          tenant: org.id,
          transaction: :all,
          batch_size: 2,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 5

      for _ <- 1..5, do: assert_receive({:manual_after_transaction_success, _})
    end
  end

  describe "forbidden errors" do
    test "after_transaction called on forbidden error with transaction: :batch" do
      result =
        Ash.bulk_create(
          [%{title: "forbidden_1"}, %{title: "forbidden_2"}, %{title: "forbidden_3"}],
          PolicyPost,
          :create_with_after_transaction,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error

      assert_receive {:after_transaction_error, error}
      assert %Ash.Error.Forbidden{} = error
      refute_receive {:after_transaction_error, _}
    end

    test "after_transaction called on forbidden error with transaction: :all" do
      result =
        Ash.bulk_create(
          [%{title: "forbidden_1"}, %{title: "forbidden_2"}, %{title: "forbidden_3"}],
          PolicyPost,
          :create_with_after_transaction,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          transaction: :all,
          return_errors?: true
        )

      assert result.status == :error

      assert_receive {:after_transaction_error, error}
      assert %Ash.Error.Forbidden{} = error
      refute_receive {:after_transaction_error, _}
    end

    test "after_transaction called on forbidden error with return_stream?" do
      result_stream =
        Ash.bulk_create(
          [%{title: "forbidden_1"}, %{title: "forbidden_2"}, %{title: "forbidden_3"}],
          PolicyPost,
          :create_with_after_transaction,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_stream?: true,
          return_errors?: true
        )

      results = Enum.to_list(result_stream)

      errors = Enum.filter(results, fn r -> match?({:error, _}, r) end)
      assert length(errors) == 3

      for _ <- 1..3 do
        assert_receive {:after_transaction_error, error}
        assert %Ash.Error.Forbidden{} = error
      end
    end
  end

  describe "validation errors" do
    test "with transaction: :batch", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "will_fail"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_validation_and_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_errors?: true
        )

      # First batch succeeds, second batch has error, stops processing
      assert result.status == :partial_success

      for _ <- 1..2, do: assert_receive({:after_transaction_success, _})
      assert_receive {:after_transaction_error, _}

      # Only first batch was created
      assert Post |> Ash.read!(tenant: org.id) |> length() == 2
    end

    test "with transaction: :all", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "will_fail"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_validation_and_after_transaction,
          tenant: org.id,
          batch_size: 2,
          transaction: :all,
          return_errors?: true
        )

      # First batch succeeds, second batch has error, stops processing (ETS has no rollback)
      assert result.status == :partial_success

      for _ <- 1..2, do: assert_receive({:after_transaction_success, _})
      assert_receive {:after_transaction_error, _}

      # First batch was created (no rollback with ETS)
      assert Post |> Ash.read!(tenant: org.id) |> length() == 2
    end

    test "mixed valid and invalid with stop_on_error?: false", %{org: org} do
      result =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "will_fail"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_validation_and_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_records?: true,
          return_errors?: true,
          stop_on_error?: false
        )

      assert result.status == :partial_success
      assert result.error_count == 1
      assert length(result.records) == 4

      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})
      assert_receive {:after_transaction_error, _}

      posts = Post |> Ash.read!(tenant: org.id)
      assert length(posts) == 4
      refute Enum.any?(posts, fn p -> p.title == "will_fail" end)
    end

    test "with return_stream?", %{org: org} do
      result_stream =
        Ash.bulk_create(
          [
            %{title: "a_ok_1"},
            %{title: "a_ok_2"},
            %{title: "a_ok_3"},
            %{title: "will_fail"},
            %{title: "a_ok_5"}
          ],
          Post,
          :create_with_validation_and_after_transaction,
          tenant: org.id,
          batch_size: 2,
          return_stream?: true,
          return_records?: true,
          return_errors?: true
        )

      results = Enum.to_list(result_stream)
      successes = Enum.filter(results, fn r -> match?({:ok, _}, r) end)
      errors = Enum.filter(results, fn r -> match?({:error, _}, r) end)

      # return_stream? continues processing all batches
      assert length(successes) == 4
      assert length(errors) == 1

      for _ <- 1..4, do: assert_receive({:after_transaction_success, _})
      assert_receive {:after_transaction_error, _}

      posts = Post |> Ash.read!(tenant: org.id)
      assert length(posts) == 4
    end
  end

  describe "mnesia" do
    setup do
      capture_log(fn ->
        Ash.DataLayer.Mnesia.start(Domain, [MnesiaPost])
      end)

      on_exit(fn ->
        capture_log(fn ->
          :mnesia.stop()
          :mnesia.delete_schema([node()])
        end)
      end)
    end

    test "after_transaction hooks run outside batch transaction - no warning" do
      log =
        capture_log(fn ->
          result =
            Ash.bulk_create(
              [
                %{title: "a_ok_1"},
                %{title: "a_ok_2"},
                %{title: "a_ok_3"},
                %{title: "z_fail_4"},
                %{title: "a_ok_5"}
              ],
              MnesiaPost,
              :create_with_conditional_after_action_error,
              batch_size: 2,
              return_records?: true,
              return_errors?: true,
              authorize?: false
            )

          # Batch 1 succeeds, Batch 2 fails (rolls back), stop_on_error stops processing
          assert result.status == :partial_success
        end)

      # Batch 1: 2 success
      assert_receive {:conditional_after_action_success, _}
      assert_receive {:conditional_after_action_success, _}
      # Batch 2: a_ok_3 succeeds then z_fail_4 fails, batch rolls back
      assert_receive {:conditional_after_action_success, _}
      assert_receive {:conditional_after_action_error, _}

      # after_transaction called for batch 1 (committed)
      assert_receive {:conditional_after_transaction, {:ok, _}}
      assert_receive {:conditional_after_transaction, {:ok, _}}

      # Verify db state - only batch 1 committed
      assert length(Ash.read!(MnesiaPost)) == 2

      # Should NOT warn since after_transaction runs outside the transaction
      refute log =~ "after_transaction"
    end

    test "after_transaction hooks with transaction: :all logs warning when run inside transaction" do
      log =
        capture_log(fn ->
          result =
            Ash.bulk_create(
              [%{title: "test1"}, %{title: "test2"}],
              MnesiaPost,
              :create_with_after_transaction,
              transaction: :all,
              return_records?: true,
              authorize?: false
            )

          assert result.status == :success
          assert length(result.records) == 2
        end)

      assert_receive {:mnesia_after_transaction_called, _id1}, 1000
      assert_receive {:mnesia_after_transaction_called, _id2}, 1000

      assert log =~ "One or more `after_transaction` hooks"
      assert log =~ "ongoing transaction still happening"
    end

    @tag :capture_log
    test "after_transaction error with rollback_on_error? rolls back entire transaction" do
      result =
        [
          %{title: "a_ok_1"},
          %{title: "a_ok_2"},
          %{title: "a_ok_3"},
          %{title: "z_fail_4"},
          %{title: "a_ok_5"}
        ]
        |> Ash.bulk_create(
          MnesiaPost,
          :create_with_after_transaction_partial_failure,
          transaction: :all,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      assert result.status == :error
      assert length(result.errors) == 1

      for _ <- 1..3, do: assert_receive({:after_transaction_partial_success, _})
      assert_receive {:after_transaction_partial_failure, _}
      refute_receive {:after_transaction_partial_success, _}

      # All records rolled back - hook runs INSIDE tx with :all
      assert [] == Ash.read!(MnesiaPost)
    end

    test "after_action error with transaction: :batch rolls back and stops after first batch" do
      result =
        [%{title: "title_1"}, %{title: "title_2"}, %{title: "title_3"}, %{title: "title_4"}]
        |> Ash.bulk_create(
          MnesiaPost,
          :create_with_after_action_error_and_after_transaction,
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

      # Verify rollback: no records created
      assert [] == MnesiaPost |> Ash.read!()
    end

    test "partial failure with transaction: :all rolls back all records" do
      {result, log} =
        with_log(fn ->
          [%{title: "a_ok_1"}, %{title: "a_ok_2"}, %{title: "z_fail_3"}, %{title: "z_fail_4"}]
          |> Ash.bulk_create(
            MnesiaPost,
            :create_with_conditional_after_action_error,
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

      # No records should exist (entire transaction rolled back)
      assert [] == MnesiaPost |> Ash.read!()
    end

    test "partial failure with transaction: :batch commits first batch, rolls back second" do
      # Use titles that sort: a_ok < z_fail, so first batch succeeds, second fails
      result =
        [%{title: "a_ok_1"}, %{title: "a_ok_2"}, %{title: "z_fail_3"}, %{title: "z_fail_4"}]
        |> Ash.bulk_create(
          MnesiaPost,
          :create_with_conditional_after_action_error,
          transaction: :batch,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      # First batch succeeds
      assert_receive {:conditional_after_action_success, _}
      assert_receive {:conditional_after_action_success, _}
      # Second batch fails
      assert_receive {:conditional_after_action_error, _}

      # partial_success because first batch succeeded, second batch failed
      assert result.status == :partial_success
      assert length(result.errors) == 1

      # after_transaction called for first batch (committed)
      assert_receive {:conditional_after_transaction, {:ok, _}}
      assert_receive {:conditional_after_transaction, {:ok, _}}
      # NOT called for second batch (rolled back)
      refute_receive {:conditional_after_transaction, {:error, _}}

      # First batch created (2 records), second batch rolled back (0 additional records)
      created_records = MnesiaPost |> Ash.read!()
      assert length(created_records) == 2

      # Only the ok records should exist
      created_titles = Enum.map(created_records, & &1.title) |> Enum.sort()
      assert created_titles == ["a_ok_1", "a_ok_2"]
    end

    test "after_transaction error on 4th record with transaction: :batch - batch 2 still commits" do
      result =
        [
          %{title: "a_ok_1"},
          %{title: "a_ok_2"},
          %{title: "a_ok_3"},
          %{title: "z_fail_4"},
          %{title: "a_ok_5"}
        ]
        |> Ash.bulk_create(
          MnesiaPost,
          :create_with_after_transaction_partial_failure,
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

      # 4 records persisted - hook runs OUTSIDE tx with :batch
      assert length(Ash.read!(MnesiaPost)) == 4
    end
  end
end
