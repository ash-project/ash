# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkDestroyBeforeHooksTest do
  @moduledoc """
  Tests for before_action and before_transaction hooks in bulk destroy operations.

  Hooks added by a change's `atomic/3` callback run on the atomic path with
  `changeset.data` set to `%Ash.Changeset.OriginalDataNotAvailable{reason: :atomic_query_destroy}`.

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
  import Ash.Expr

  alias Ash.Test.Domain, as: Domain

  alias Ash.Test.BulkTestChanges.AfterTransactionHandlingErrors
  alias Ash.Test.BulkTestChanges.AroundActionAtomicChange
  alias Ash.Test.BulkTestChanges.BeforeActionAddsAfterActionChange
  alias Ash.Test.BulkTestChanges.BeforeActionAddsAfterTransactionChange
  alias Ash.Test.BulkTestChanges.BeforeActionChange
  alias Ash.Test.BulkTestChanges.BeforeActionErrorChange
  alias Ash.Test.BulkTestChanges.BeforeActionRaises
  alias Ash.Test.BulkTestChanges.BeforeActionReturnsErrorTuple
  alias Ash.Test.BulkTestChanges.BeforeActionWithNotificationChange
  alias Ash.Test.BulkTestChanges.BeforeTransactionAddsAfterTransactionChange
  alias Ash.Test.BulkTestChanges.BeforeTransactionChange
  alias Ash.Test.BulkTestChanges.BeforeTransactionErrorChange
  alias Ash.Test.BulkTestChanges.HookOrderChange
  alias Ash.Test.BulkTestChanges.MultipleBeforeActionHooks
  alias Ash.Test.BulkTestChanges.Notifier
  alias Ash.Test.BulkTestChanges.OriginalTitlePolicyCheck

  alias Ash.Changeset.OriginalDataNotAvailable

  defmodule BeforeActionFilterChange do
    @moduledoc "before_action hook that narrows the records the destroy applies to."
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.before_action(changeset, fn changeset ->
        Ash.Changeset.filter(changeset, expr(title == "match"))
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule BeforeActionArgumentChange do
    @moduledoc "before_action hook that reads an action argument."
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.before_action(changeset, fn changeset ->
        reason = Ash.Changeset.get_argument(changeset, :reason)
        send(self(), {:before_action_argument, changeset.action.name, reason})
        changeset
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule NotAtomicChange do
    @moduledoc "A change without `atomic/3`, forcing the stream strategy."
    use Ash.Resource.Change

    def change(changeset, _opts, _context), do: changeset
  end

  defmodule EagerFailValidation do
    @moduledoc "Fails while the changeset is being built, before any hook runs."
    use Ash.Resource.Validation

    @impl true
    def validate(_, _, _), do: {:error, field: :title, message: "always fails"}

    @impl true
    def atomic(_, _, _), do: {:error, field: :title, message: "always fails atomically"}
  end

  defmodule StoredTitleValidation do
    @moduledoc "Fails for records whose stored title is \"will_fail\"."
    use Ash.Resource.Validation

    @impl true
    def validate(changeset, _opts, _context) do
      if Ash.Changeset.get_attribute(changeset, :title) == "will_fail" do
        {:error, field: :title, message: "stored title validation failed"}
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
           message: "stored title validation failed"
         })
       )}
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
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        pagination keyset?: true, required?: false
      end

      destroy :destroy_with_before_action do
        change BeforeActionChange
      end

      destroy :destroy_with_before_action_argument do
        argument :reason, :string, allow_nil?: false
        change BeforeActionArgumentChange
      end

      destroy :destroy_with_before_action_filter do
        change BeforeActionFilterChange
      end

      destroy :destroy_with_before_action_error do
        change BeforeActionErrorChange
        change AfterTransactionHandlingErrors
      end

      destroy :destroy_with_before_action_error_tuple do
        change BeforeActionReturnsErrorTuple
      end

      destroy :destroy_with_before_action_raises do
        change BeforeActionRaises
      end

      destroy :destroy_with_before_action_notification do
        change BeforeActionWithNotificationChange
      end

      destroy :destroy_with_before_action_adding_after_action do
        change BeforeActionAddsAfterActionChange
      end

      destroy :destroy_with_before_action_adding_after_transaction do
        change BeforeActionAddsAfterTransactionChange
      end

      destroy :destroy_with_multiple_before_action_hooks do
        change MultipleBeforeActionHooks
      end

      destroy :destroy_with_all_hooks do
        change HookOrderChange
      end

      destroy :destroy_with_before_transaction do
        change BeforeTransactionChange
      end

      destroy :destroy_with_before_transaction_error do
        change BeforeTransactionAddsAfterTransactionChange
        change BeforeTransactionErrorChange
      end

      destroy :destroy_with_before_hooks_and_eager_validation do
        change BeforeTransactionChange
        change BeforeActionChange
        validate EagerFailValidation
        change AfterTransactionHandlingErrors
      end

      destroy :destroy_with_before_hooks_and_stored_title_validation do
        change BeforeTransactionChange
        change BeforeActionChange
        validate StoredTitleValidation
        change AfterTransactionHandlingErrors
      end

      destroy :destroy_with_around_action do
        change AroundActionAtomicChange
      end

      destroy :destroy_with_around_action_fallback do
        require_atomic? false
        change AroundActionAtomicChange
      end

      destroy :destroy_with_before_hooks_not_atomic do
        require_atomic? false
        change NotAtomicChange
        change BeforeTransactionChange
        change BeforeActionChange
      end

      destroy :soft_with_before_hooks do
        soft? true
        change set_attribute(:title2, "soft_deleted")
        change BeforeTransactionChange
        change BeforeActionChange
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true
    end

    calculations do
      calculate :shout, :string, expr(title <> "!")
    end
  end

  defmodule MnesiaAudit do
    @moduledoc "Written to by before_action hooks to observe rollbacks."
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :mnesia_audit_before_hooks_destroy
    end

    attributes do
      uuid_primary_key :id
      attribute :note, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create, :update]
    end
  end

  defmodule MnesiaPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia, notifiers: [Notifier]

    mnesia do
      table :mnesia_post_before_hooks_destroy
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create, :update]

      destroy :destroy_with_side_effect_before_action do
        require_atomic? false

        # Writes an audit record, then fails for records whose title says so.
        # Used to verify that the audit write is rolled back with the destroy.
        change fn changeset, _ ->
          Ash.Changeset.before_action(changeset, fn changeset ->
            Ash.create!(MnesiaAudit, %{note: "audit_" <> changeset.data.title})

            if String.starts_with?(changeset.data.title, "fail") do
              Ash.Changeset.add_error(changeset, field: :title, message: "failed on purpose")
            else
              changeset
            end
          end)
        end
      end

      destroy :destroy_with_before_transaction do
        require_atomic? false
        change BeforeTransactionChange
      end
    end
  end

  defmodule ManualDestroySimple do
    @moduledoc false
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _opts, _context) do
      case Ash.DataLayer.Ets.destroy(changeset.resource, changeset) do
        :ok -> {:ok, changeset.data}
        {:error, error} -> {:error, error}
      end
    end
  end

  defmodule ManualDestroyPost do
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
      defaults [:read, :update, create: :*]

      destroy :destroy_manual_with_before_hooks do
        manual ManualDestroySimple
        change BeforeTransactionChange
        change BeforeActionChange
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  defmodule PolicyPost do
    @moduledoc """
    Reading and creating are allowed for everyone, destroying only for actors with
    `allow: true`, so the forbidden error is decided before any hook could run.
    """
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    policies do
      policy action(:destroy_with_initial_data_before_hooks) do
        authorize_if OriginalTitlePolicyCheck
      end

      policy action_type(:read) do
        authorize_if always()
      end

      policy action_type(:create) do
        authorize_if always()
      end

      policy action_type(:destroy) do
        forbid_unless actor_attribute_equals(:allow, true)
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:update, create: :*]

      read :read do
        primary? true
        pagination keyset?: true, required?: false
      end

      destroy :destroy_with_initial_data_before_hooks do
        require_atomic? false
        change BeforeTransactionChange
        change BeforeActionChange
        change AfterTransactionHandlingErrors
      end

      destroy :destroy do
        primary? true
      end

      destroy :destroy_with_before_hooks do
        change BeforeTransactionChange
        change BeforeActionChange
        change AfterTransactionHandlingErrors
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  defp create_posts(resource \\ Post, count, prefix \\ "title") do
    for i <- 1..count do
      resource
      |> Ash.Changeset.for_create(:create, %{title: "#{prefix}_#{i}"})
      |> Ash.create!(authorize?: false)
    end
  end

  defp query_for(posts, resource \\ Post) do
    ids = Enum.map(posts, & &1.id)
    Ash.Query.filter(resource, id in ^ids)
  end

  defp remaining(posts, resource \\ Post) do
    posts |> query_for(resource) |> Ash.read!(authorize?: false)
  end

  describe ":atomic strategy" do
    test "before_action and before_transaction hooks execute once with the destroy sentinel" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_all_hooks, %{}, strategy: :atomic)

      assert result.status == :success
      assert remaining(posts) == []

      assert_received {:hook, :before_transaction}
      assert_received {:hook, :before_action}
      refute_received {:hook, :before_transaction}
      refute_received {:hook, :before_action}
    end

    test "hooks run in the same order as on the non-atomic path" do
      posts = create_posts(1)

      posts
      |> query_for()
      |> Ash.bulk_destroy!(:destroy_with_all_hooks, %{}, strategy: :atomic)

      assert_received {:hook, first}
      assert_received {:hook, second}
      assert_received {:hook, third}
      assert_received {:hook, fourth}
      refute_received {:hook, _}

      assert [first, second, third, fourth] ==
               [:before_transaction, :before_action, :after_action, :after_transaction]
    end

    test "hooks receive the destroy sentinel as data" do
      posts = create_posts(2)

      posts
      |> query_for()
      |> Ash.bulk_destroy!(:destroy_with_before_action, %{},
        strategy: :atomic,
        return_records?: true
      )

      assert_received {:before_action_called,
                       %OriginalDataNotAvailable{reason: :atomic_query_destroy}}

      refute_received {:before_action_called, _}

      posts = create_posts(2)

      posts
      |> query_for()
      |> Ash.bulk_destroy!(:destroy_with_before_transaction, %{}, strategy: :atomic)

      assert_received {:before_transaction_called,
                       %OriginalDataNotAvailable{reason: :atomic_query_destroy}}

      refute_received {:before_transaction_called, _}
    end

    test "hook receives the action and arguments" do
      posts = create_posts(2)

      posts
      |> query_for()
      |> Ash.bulk_destroy!(:destroy_with_before_action_argument, %{reason: "cleanup"},
        strategy: :atomic
      )

      assert_received {:before_action_argument, :destroy_with_before_action_argument, "cleanup"}
      assert remaining(posts) == []
    end

    test "filters added in a before_action hook narrow the destroyed records" do
      match = Post |> Ash.Changeset.for_create(:create, %{title: "match"}) |> Ash.create!()
      other = Post |> Ash.Changeset.for_create(:create, %{title: "other"}) |> Ash.create!()

      result =
        [match, other]
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_action_filter, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert [%{title: "match"}] = result.records
      assert [%{title: "other"}] = remaining([match, other])
    end

    test "hooks execute without return_records?" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_action, %{}, strategy: :atomic)

      assert result.status == :success
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert remaining(posts) == []
    end

    test "empty result set still runs the hooks and succeeds" do
      result =
        Post
        |> Ash.Query.filter(title == "does_not_exist")
        |> Ash.bulk_destroy!(:destroy_with_all_hooks, %{}, strategy: :atomic)

      assert result.status == :success
      assert_received {:hook, :before_transaction}
      assert_received {:hook, :before_action}
    end

    test "multiple before_action hooks execute in order, honouring prepend?" do
      posts = create_posts(1)

      posts
      |> query_for()
      |> Ash.bulk_destroy!(:destroy_with_multiple_before_action_hooks, %{}, strategy: :atomic)

      assert_received {:before_action_hook, first}
      assert_received {:before_action_hook, second}
      assert_received {:before_action_hook, third}
      assert [first, second, third] == [0, 1, 2]
    end

    test "an invalid changeset returned from before_action is captured and nothing is destroyed" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_before_action_error, %{},
          strategy: :atomic,
          return_errors?: true,
          return_records?: true
        )

      assert %Ash.BulkResult{status: :error, errors: [error], records: []} = result
      assert result.error_count == 1
      assert %Ash.Error.Invalid{} = error
      assert Exception.message(error) =~ "before_action failed"
      assert length(remaining(posts)) == 2
    end

    test "an error tuple returned from before_action is captured in the result" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_before_action_error_tuple, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert %Ash.BulkResult{status: :error, errors: [error]} = result
      assert Exception.message(error) =~ "before_action error tuple"
      assert length(remaining(posts)) == 2
    end

    test "an invalid changeset returned from before_transaction is captured and nothing is destroyed" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_before_transaction_error, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert %Ash.BulkResult{status: :error, errors: [error]} = result
      assert Exception.message(error) =~ "before_transaction failed"
      assert length(remaining(posts)) == 2
    end

    test "after_transaction hooks added by a before_transaction hook run on failure" do
      posts = create_posts(1)

      posts
      |> query_for()
      |> Ash.bulk_destroy(:destroy_with_before_transaction_error, %{},
        strategy: :atomic,
        return_errors?: true
      )

      assert_received {:after_transaction_from_before_transaction, {:error, _}}
    end

    test "after_action hooks added by a before_action hook run with the destroyed records" do
      posts = create_posts(3)
      ids = posts |> Enum.map(& &1.id) |> Enum.sort()

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_action_adding_after_action, %{},
          strategy: :atomic
        )

      assert result.status == :success

      received =
        for _ <- 1..3 do
          assert_received {:after_action_from_before_action, id}
          id
        end

      assert Enum.sort(received) == ids
    end

    test "after_transaction hooks added by a before_action hook run on success" do
      posts = create_posts(2)

      posts
      |> query_for()
      |> Ash.bulk_destroy!(:destroy_with_before_action_adding_after_transaction, %{},
        strategy: :atomic
      )

      assert_received {:after_transaction_from_before_action, {:ok, _}}
    end

    test "notifications returned by a before_action hook are returned with return_notifications?" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_action_notification, %{},
          strategy: :atomic,
          return_notifications?: true
        )

      assert result.status == :success

      assert [%Ash.Notifier.Notification{metadata: %{source: :before_action}}] =
               Enum.filter(result.notifications, &(&1.metadata[:source] == :before_action))

      assert Enum.count(result.notifications, &is_nil(&1.metadata[:source])) == 2
    end

    test "notifications returned by a before_action hook are sent with notify?" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_action_notification, %{},
          strategy: :atomic,
          notify?: true
        )

      assert result.status == :success
      assert_received {:notification, %{metadata: %{source: :before_action}}}
      assert_received {:notification, %{data: %Post{}}}
      assert_received {:notification, %{data: %Post{}}}
    end

    test "load option works with hooks" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_action, %{},
          strategy: :atomic,
          return_records?: true,
          load: [:shout]
        )

      assert Enum.all?(result.records, &(&1.shout == &1.title <> "!"))
    end

    test "hooks are not run when the atomic changeset is invalid before execution" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_before_hooks_and_eager_validation, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert result.status == :error
      assert [error] = result.errors
      assert Exception.message(error) =~ "always fails"

      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert_received {:after_transaction_error, _}
      assert length(remaining(posts)) == 2
    end

    test "around_action hooks added in atomic/3 make the action not atomic" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_around_action, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert %Ash.BulkResult{status: :error, errors: [error]} = result
      assert Exception.message(error) =~ "around_action"
      refute_received :around_action_called
      assert length(remaining(posts)) == 2
    end

    test "strategy fallback with require_atomic?: false runs around hooks on the stream path" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_around_action_fallback, %{},
          strategy: [:atomic, :stream]
        )

      assert result.status == :success
      assert_received :around_action_called
      assert_received :around_action_called
      assert remaining(posts) == []
    end

    test "strategy fallback to stream runs before hooks with the real records" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_hooks_not_atomic, %{},
          strategy: [:atomic, :stream]
        )

      assert result.status == :success

      for _ <- 1..2 do
        assert_received {:before_transaction_called, %Post{}}
        assert_received {:before_action_called, %Post{}}
      end

      assert remaining(posts) == []
    end
  end

  describe ":atomic_batches strategy" do
    test "hooks execute once per batch with the destroy sentinel" do
      posts = create_posts(5)

      result =
        Ash.bulk_destroy!(posts, :destroy_with_all_hooks, %{},
          strategy: :atomic_batches,
          batch_size: 2
        )

      assert result.status == :success

      for _ <- 1..3 do
        assert_received {:hook, :before_transaction}
        assert_received {:hook, :before_action}
      end

      refute_received {:hook, :before_transaction}
      refute_received {:hook, :before_action}
      assert remaining(posts) == []
    end

    test "before_action receives the sentinel once per batch when records are returned" do
      posts = create_posts(5)

      result =
        Ash.bulk_destroy!(posts, :destroy_with_before_action, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_records?: true
        )

      assert length(result.records) == 5

      for _ <- 1..3 do
        assert_received {:before_action_called,
                         %OriginalDataNotAvailable{reason: :atomic_query_destroy}}
      end

      refute_received {:before_action_called, _}
    end

    test "hook error is captured and nothing is destroyed" do
      posts = create_posts(5)

      result =
        Ash.bulk_destroy(posts, :destroy_with_before_action_error, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert Enum.all?(result.errors, &(Exception.message(&1) =~ "before_action failed"))
      assert length(remaining(posts)) == 5
      assert_received {:after_transaction_error, _}
    end

    test "hooks work with notify?: true" do
      posts = create_posts(3)

      result =
        Ash.bulk_destroy!(posts, :destroy_with_before_action, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          notify?: true
        )

      assert result.status == :success

      for _ <- 1..3 do
        assert_received {:notification, %{data: %Post{}}}
      end
    end

    test "load option works with hooks" do
      posts = create_posts(3)

      result =
        Ash.bulk_destroy!(posts, :destroy_with_before_action, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_records?: true,
          load: [:shout]
        )

      assert Enum.all?(result.records, &(&1.shout == &1.title <> "!"))
    end

    test "after_action hooks added by a before_action hook run for every record" do
      posts = create_posts(5)

      Ash.bulk_destroy!(posts, :destroy_with_before_action_adding_after_action, %{},
        strategy: :atomic_batches,
        batch_size: 2
      )

      for _ <- 1..5 do
        assert_received {:after_action_from_before_action, _}
      end
    end
  end

  describe ":stream strategy" do
    test "hooks execute once per record with the real record" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_all_hooks, %{}, strategy: :stream)

      assert result.status == :success

      for _ <- 1..3 do
        assert_received {:hook, :before_transaction}
        assert_received {:hook, :before_action}
      end

      assert remaining(posts) == []
    end

    test "hooks see the record as data" do
      posts = create_posts(3)

      posts
      |> query_for()
      |> Ash.bulk_destroy!(:destroy_with_before_action, %{}, strategy: :stream)

      for _ <- 1..3 do
        assert_received {:before_action_called, %Post{}}
      end
    end

    test "with return_stream?: true streams results" do
      posts = create_posts(3)

      results =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_before_action, %{},
          strategy: :stream,
          return_stream?: true,
          return_records?: true
        )
        |> Enum.to_list()

      assert length(results) == 3
      assert Enum.all?(results, &match?({:ok, %Post{}}, &1))
    end

    test "hook error stops processing with stop_on_error?: true" do
      posts = create_posts(5)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_before_action_error, %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert length(result.errors) == 1
      assert length(remaining(posts)) == 5
    end

    test "hook error continues processing with stop_on_error?: false" do
      posts = create_posts(5)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_before_action_error, %{},
          strategy: :stream,
          batch_size: 2,
          stop_on_error?: false,
          return_errors?: true
        )

      assert result.status == :error
      assert length(result.errors) == 5
    end

    test "hooks work with return_notifications?: true" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_action_notification, %{},
          strategy: :stream,
          return_notifications?: true
        )

      assert Enum.count(result.notifications, &(&1.metadata[:source] == :before_action)) == 2
    end

    test "hooks work with transaction: :all" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:destroy_with_before_action, %{},
          strategy: :stream,
          transaction: :all
        )

      assert result.status == :success
      assert remaining(posts) == []
    end
  end

  describe ":stream strategy with Mnesia" do
    setup do
      capture_log(fn ->
        Ash.DataLayer.Mnesia.start(Domain, [MnesiaPost, MnesiaAudit])
      end)

      for resource <- [MnesiaPost, MnesiaAudit] do
        resource |> Ash.read!() |> Enum.each(&Ash.destroy!/1)
      end

      :ok
    end

    @tag :capture_log
    test "before_action error with transaction: :all rolls back the hook's side effects" do
      posts =
        for title <- ["ok_1", "ok_2", "fail_3", "ok_4"] do
          MnesiaPost
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      result =
        posts
        |> query_for(MnesiaPost)
        |> Ash.bulk_destroy(:destroy_with_side_effect_before_action, %{},
          strategy: :stream,
          transaction: :all,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      assert %Ash.BulkResult{status: :error} = result

      assert length(remaining(posts, MnesiaPost)) == 4
      assert Ash.read!(MnesiaAudit) == []
    end

    @tag :capture_log
    test "before_action error with transaction: :batch rolls back only the failing batch" do
      posts =
        for title <- ["ok_1", "ok_2", "fail_3", "ok_4"] do
          MnesiaPost
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.create!()
        end

      result =
        posts
        |> query_for(MnesiaPost)
        |> Ash.bulk_destroy(:destroy_with_side_effect_before_action, %{},
          strategy: :stream,
          transaction: :batch,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      assert %Ash.BulkResult{errors: [error]} = result
      assert Exception.message(error) =~ "failed on purpose"

      destroyed =
        posts
        |> Enum.map(& &1.title)
        |> Kernel.--(Enum.map(remaining(posts, MnesiaPost), & &1.title))
        |> Enum.map(&"audit_#{&1}")
        |> Enum.sort()

      audits = MnesiaAudit |> Ash.read!() |> Enum.map(& &1.note) |> Enum.sort()

      # The batch holding fail_3 rolled back, including the audit rows its hooks wrote,
      # so the failing record has no audit row and every audit row that survived belongs
      # to a record whose destroy committed in another batch. Which records share a
      # batch with fail_3 depends on the read order, so only consistency is asserted.
      refute "audit_fail_3" in audits
      assert audits == destroyed
    end

    test "before_transaction hooks run outside of the transaction without a warning" do
      post =
        MnesiaPost
        |> Ash.Changeset.for_create(:create, %{title: "title"})
        |> Ash.create!()

      log =
        capture_log(fn ->
          result =
            MnesiaPost
            |> Ash.Query.filter(id == ^post.id)
            |> Ash.bulk_destroy!(:destroy_with_before_transaction, %{},
              strategy: :stream,
              transaction: :batch
            )

          assert result.status == :success
        end)

      refute log =~ "before_transaction"
      assert_received {:before_transaction_called, %MnesiaPost{}}
      assert Ash.read!(MnesiaPost) == []
    end
  end

  describe "soft destroy" do
    test "before hooks execute on an atomic soft destroy" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:soft_with_before_hooks, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert result.status == :success
      assert length(result.records) == 3

      # a soft destroy is an update underneath, so the update sentinel is used
      assert_received {:before_transaction_called,
                       %OriginalDataNotAvailable{reason: :atomic_query_update}}

      assert_received {:before_action_called,
                       %OriginalDataNotAvailable{reason: :atomic_query_update}}

      remaining = remaining(posts)
      assert length(remaining) == 3
      assert Enum.all?(remaining, &(&1.title2 == "soft_deleted"))
    end

    test "before hooks execute on a streamed soft destroy" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy!(:soft_with_before_hooks, %{},
          strategy: :stream,
          batch_size: 2
        )

      assert result.status == :success

      for _ <- 1..3 do
        assert_received {:before_transaction_called, %Post{}}
        assert_received {:before_action_called, %Post{}}
      end

      assert Enum.all?(remaining(posts), &(&1.title2 == "soft_deleted"))
    end
  end

  describe "manual actions" do
    test "before hooks run with the real record on a manual destroy" do
      post =
        ManualDestroyPost
        |> Ash.Changeset.for_create(:create, %{title: "manual"})
        |> Ash.create!()

      result =
        ManualDestroyPost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_destroy!(:destroy_manual_with_before_hooks, %{},
          strategy: :stream,
          return_records?: true
        )

      assert [%{title: "manual"}] = result.records
      assert_received {:before_transaction_called, %ManualDestroyPost{}}
      assert_received {:before_action_called, %ManualDestroyPost{}}
      assert Ash.read!(ManualDestroyPost) == []
    end

    test "before hooks run for every record with multiple batches" do
      posts = create_posts(ManualDestroyPost, 5, "manual")

      result =
        posts
        |> query_for(ManualDestroyPost)
        |> Ash.bulk_destroy!(:destroy_manual_with_before_hooks, %{},
          strategy: :stream,
          batch_size: 2
        )

      assert result.status == :success

      for _ <- 1..5 do
        assert_received {:before_action_called, %ManualDestroyPost{}}
      end

      assert remaining(posts, ManualDestroyPost) == []
    end
  end

  describe "policy fallback requiring original data" do
    for strategy <- [:atomic, :atomic_batches] do
      @strategy strategy

      test "#{@strategy} falls back to per-record before hooks when a policy needs original data" do
        posts = create_posts(PolicyPost, 2, "allowed")
        actor = %{allow: true}

        changeset =
          Ash.Changeset.fully_atomic_changeset(
            PolicyPost,
            :destroy_with_initial_data_before_hooks,
            %{},
            actor: actor
          )

        assert %Ash.Changeset{valid?: true} = changeset

        assert {:ok, false, %Ash.Error.Forbidden.InitialDataRequired{}} =
                 Ash.can(changeset, actor,
                   run_queries?: false,
                   return_forbidden_error?: true,
                   pre_flight?: false,
                   atomic_changeset: changeset,
                   no_check?: true,
                   alter_source?: true
                 )

        input = if @strategy == :atomic, do: query_for(posts, PolicyPost), else: posts

        result =
          Ash.bulk_destroy!(input, :destroy_with_initial_data_before_hooks, %{},
            strategy: [@strategy, :stream],
            batch_size: 1,
            authorize?: true,
            actor: actor,
            return_records?: true
          )

        assert result.status == :success
        assert Enum.sort(Enum.map(result.records, & &1.id)) == Enum.sort(Enum.map(posts, & &1.id))

        for post <- posts do
          id = post.id
          title = post.title
          assert_received {:before_transaction_called, %PolicyPost{id: ^id, title: ^title}}
          assert_received {:before_action_called, %PolicyPost{id: ^id, title: ^title}}
        end

        refute_received {:before_transaction_called, _}
        refute_received {:before_action_called, _}
        remaining = query_for(posts, PolicyPost) |> Ash.read!(authorize?: false)
        assert remaining == []
      end
    end

    test "a policy denying the loaded record prevents before hooks after fallback" do
      [post] = create_posts(PolicyPost, 1, "denied")
      input = query_for([post], PolicyPost)

      result =
        Ash.bulk_destroy(input, :destroy_with_initial_data_before_hooks, %{},
          strategy: [:atomic, :stream],
          authorize?: true,
          actor: %{allow: true},
          return_errors?: true
        )

      assert result.status == :error
      assert [%Ash.Error.Forbidden{}] = result.errors
      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert Ash.get!(PolicyPost, post.id, authorize?: false).title == post.title
    end
  end

  describe "forbidden errors" do
    test "before hooks are not called on a forbidden error with :atomic strategy" do
      [post] = create_posts(PolicyPost, 1, "forbidden")

      result =
        PolicyPost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_destroy(:destroy_with_before_hooks, %{},
          strategy: :atomic,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, %Ash.Error.Forbidden{}}
      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert length(remaining([post], PolicyPost)) == 1
    end

    test "before hooks are not called on a forbidden error with :atomic_batches strategy" do
      posts = create_posts(PolicyPost, 5, "forbidden")

      result =
        Ash.bulk_destroy(posts, :destroy_with_before_hooks, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, %Ash.Error.Forbidden{}}
      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert length(remaining(posts, PolicyPost)) == 5
    end

    test "before hooks are not called on a forbidden error with :stream strategy" do
      posts = create_posts(PolicyPost, 5, "forbidden")

      result =
        posts
        |> query_for(PolicyPost)
        |> Ash.bulk_destroy(:destroy_with_before_hooks, %{},
          strategy: :stream,
          batch_size: 2,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, %Ash.Error.Forbidden{}}
      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert length(remaining(posts, PolicyPost)) == 5
    end

    test "before hooks run when the actor is allowed with :atomic strategy" do
      [post] = create_posts(PolicyPost, 1, "allowed")

      result =
        PolicyPost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_destroy!(:destroy_with_before_hooks, %{},
          strategy: :atomic,
          authorize?: true,
          actor: %{allow: true}
        )

      assert result.status == :success
      assert_received {:before_transaction_called, %OriginalDataNotAvailable{}}
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert_received {:after_transaction_success, _}
      assert remaining([post], PolicyPost) == []
    end
  end

  describe "validation errors" do
    # The validation checks the stored title, so it can only be decided by the data
    # layer on the atomic path, but it is decided while building each changeset on the
    # stream path.
    defp create_failing_posts(count) do
      for _ <- 1..count do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "will_fail"})
        |> Ash.create!()
      end
    end

    test "hooks run before a validation that fails in the data layer with :atomic strategy" do
      [post] = create_failing_posts(1)

      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_destroy(:destroy_with_before_hooks_and_stored_title_validation, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, error}
      assert Exception.message(error) =~ "stored title validation failed"
      assert_received {:before_transaction_called, %OriginalDataNotAvailable{}}
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert length(remaining([post])) == 1
    end

    test "hooks run before a validation that fails in the data layer with :atomic_batches strategy" do
      posts = create_failing_posts(5)

      result =
        Ash.bulk_destroy(posts, :destroy_with_before_hooks_and_stored_title_validation, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, _}
      assert_received {:before_transaction_called, %OriginalDataNotAvailable{}}
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert length(remaining(posts)) == 5
    end

    test "hooks are not run for a changeset that fails validation with :stream strategy" do
      posts = create_failing_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_destroy(:destroy_with_before_hooks_and_stored_title_validation, %{},
          strategy: :stream,
          return_errors?: true
        )

      assert result.status == :error
      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert_received {:after_transaction_error, _}
      assert length(remaining(posts)) == 3
    end
  end
end
