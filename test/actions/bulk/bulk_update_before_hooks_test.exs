# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkUpdateBeforeHooksTest do
  @moduledoc """
  Tests for before_action and before_transaction hooks in bulk update operations.

  Hooks added by a change's `atomic/3` callback run on the atomic path with
  `changeset.data` set to `%Ash.Changeset.OriginalDataNotAvailable{}`.

  Note: Uses `async: false` because of Mnesia.

  Tests are organized by strategy:
  - :atomic strategy
  - :atomic_batches strategy
  - :stream strategy
  - :stream strategy with Mnesia
  - single record atomic upgrade
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

  defmodule BeforeActionAtomicUpdateChange do
    @moduledoc "before_action hook that adds an atomic update expression."
    use Ash.Resource.Change

    def change(changeset, opts, _context) do
      Ash.Changeset.before_action(changeset, fn changeset ->
        if opts[:defaulted?] do
          Ash.Changeset.atomic_update(changeset, :defaulted_counter, expr(defaulted_counter + 10))
        else
          Ash.Changeset.atomic_update(changeset, :counter, expr(counter + 10))
        end
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule BeforeTransactionAtomicUpdateChange do
    @moduledoc "before_transaction hook that adds an atomic update expression."
    use Ash.Resource.Change

    def change(changeset, opts, _context) do
      Ash.Changeset.before_transaction(changeset, fn changeset ->
        cond do
          opts[:defaulted?] ->
            Ash.Changeset.atomic_update(
              changeset,
              :defaulted_counter,
              expr(defaulted_counter + 10)
            )

          opts[:constrained?] ->
            Ash.Changeset.atomic_update(
              changeset,
              :constrained_counter,
              expr(constrained_counter + 10)
            )

          true ->
            Ash.Changeset.atomic_update(changeset, :counter, expr(counter + 10))
        end
      end)
    end

    def atomic(changeset, opts, context), do: {:ok, change(changeset, opts, context)}
  end

  defmodule BeforeActionObservesTouchedTransaction do
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.before_action(changeset, fn changeset ->
        send(
          self(),
          {:audit_transaction?,
           Ash.DataLayer.in_transaction?(Ash.Test.Actions.BulkUpdateBeforeHooksTest.MnesiaAudit)}
        )

        changeset
      end)
    end

    def atomic(changeset, opts, context), do: {:ok, change(changeset, opts, context)}
  end

  defmodule BeforeActionFilterChange do
    @moduledoc "before_action hook that narrows the records the action applies to."
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
        suffix = Ash.Changeset.get_argument(changeset, :suffix)
        send(self(), {:before_action_action, changeset.action.name})
        Ash.Changeset.force_change_attribute(changeset, :title2, "arg_" <> suffix)
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

  defmodule AtomicValidationFailsInTransaction do
    @moduledoc "Fails in the data layer when title == \"will_fail\"."
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

      update :update_with_before_action do
        change {BeforeActionChange, attribute: :title2, value: "before_action"}
      end

      for {phase, change_module} <- [
            {:before_action, BeforeActionChange},
            {:before_transaction, BeforeTransactionChange}
          ] do
        update :"update_with_#{phase}_restoring_title" do
          accept [:title]
          change {change_module, attribute: :title, value: "title_1"}
        end

        update :"update_with_#{phase}_timestamp" do
          accept [:title]
          change {change_module, attribute: :updated_at, value: ~U[2000-01-01 00:00:00.000000Z]}
        end
      end

      update :update_with_before_action_argument do
        argument :suffix, :string, allow_nil?: false
        change BeforeActionArgumentChange
      end

      update :update_with_before_action_atomic_update do
        change BeforeActionAtomicUpdateChange
      end

      update :update_with_before_transaction_atomic_update do
        accept []
        change BeforeTransactionAtomicUpdateChange
      end

      update :update_with_before_transaction_constrained_atomic_update do
        accept [:title]
        change {BeforeTransactionAtomicUpdateChange, constrained?: true}
      end

      update :update_with_before_action_defaulted_counter do
        accept [:title]
        change {BeforeActionAtomicUpdateChange, defaulted?: true}
      end

      update :update_with_before_transaction_defaulted_counter do
        accept [:title]
        change {BeforeTransactionAtomicUpdateChange, defaulted?: true}
      end

      update :update_with_touched_resource_before_action do
        accept [:title]
        touches_resources [Ash.Test.Actions.BulkUpdateBeforeHooksTest.MnesiaAudit]
        change BeforeActionObservesTouchedTransaction
      end

      update :update_with_before_action_filter do
        change BeforeActionFilterChange
        change {BeforeActionChange, attribute: :title2, value: "filtered"}
      end

      update :update_with_before_action_error do
        change BeforeActionErrorChange
        change AfterTransactionHandlingErrors
      end

      update :update_with_before_action_error_tuple do
        change BeforeActionReturnsErrorTuple
      end

      update :update_with_before_action_raises do
        change BeforeActionRaises
      end

      update :update_with_before_action_notification do
        change BeforeActionWithNotificationChange
      end

      update :update_with_before_action_adding_after_action do
        change BeforeActionAddsAfterActionChange
      end

      update :update_with_before_action_adding_after_transaction do
        change BeforeActionAddsAfterTransactionChange
      end

      update :update_with_multiple_before_action_hooks do
        change MultipleBeforeActionHooks
      end

      update :update_with_all_hooks do
        change HookOrderChange
      end

      update :update_with_before_transaction do
        change {BeforeTransactionChange, attribute: :title2, value: "before_transaction"}
      end

      update :update_with_before_transaction_error do
        change BeforeTransactionAddsAfterTransactionChange
        change BeforeTransactionErrorChange
      end

      update :update_with_before_hooks_and_eager_validation do
        change BeforeTransactionChange
        change BeforeActionChange
        validate EagerFailValidation
        change AfterTransactionHandlingErrors
      end

      update :update_with_before_hooks_and_atomic_validation do
        accept [:title]
        change BeforeTransactionChange
        change BeforeActionChange
        validate AtomicValidationFailsInTransaction
        change AfterTransactionHandlingErrors
      end

      update :update_with_around_action do
        change AroundActionAtomicChange
      end

      update :update_with_around_action_fallback do
        require_atomic? false
        change AroundActionAtomicChange
      end

      update :update_with_before_hooks_not_atomic do
        require_atomic? false
        change NotAtomicChange
        change {BeforeTransactionChange, attribute: :title2, value: "before_transaction"}
        change {BeforeActionChange, attribute: :title3, value: "before_action"}
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true
      attribute :title3, :string, public?: true
      attribute :counter, :integer, public?: true, default: 0, allow_nil?: false
      attribute :constrained_counter, :integer, public?: true, default: 0, constraints: [max: 5]

      attribute :defaulted_counter, :integer,
        public?: true,
        default: 0,
        update_default: fn ->
          send(self(), :defaulted_counter_evaluated)
          0
        end,
        constraints: [max: 5]

      timestamps()
    end

    calculations do
      calculate :shout, :string, expr(title <> "!")
    end
  end

  defmodule SimplePost do
    @moduledoc "A minimal resource: no notifiers, validations, relationships or policies."
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :body, :string, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :update_with_before_hooks do
        change {BeforeTransactionChange, attribute: :body, value: "before_transaction"}
        change {BeforeActionChange, attribute: :title, value: "before_action"}
      end
    end
  end

  defmodule MnesiaAudit do
    @moduledoc "Written to by before_action hooks to observe rollbacks."
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :mnesia_audit_before_hooks
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
      table :mnesia_post_before_hooks
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create, :update]

      update :update_with_side_effect_before_action do
        require_atomic? false

        # Writes an audit record, then fails for records whose title says so.
        # Used to verify that the audit write is rolled back with the update.
        change fn changeset, _ ->
          Ash.Changeset.before_action(changeset, fn changeset ->
            Ash.create!(MnesiaAudit, %{note: "audit_" <> changeset.data.title})

            if String.starts_with?(changeset.data.title, "fail") do
              Ash.Changeset.add_error(changeset, field: :title, message: "failed on purpose")
            else
              Ash.Changeset.force_change_attribute(changeset, :title2, "updated")
            end
          end)
        end
      end

      update :update_with_before_transaction do
        require_atomic? false
        change {BeforeTransactionChange, attribute: :title2, value: "before_transaction"}
      end
    end
  end

  defmodule ManualUpdateSimple do
    @moduledoc false
    use Ash.Resource.ManualUpdate

    def update(changeset, _opts, _context) do
      Ash.DataLayer.Ets.update(changeset.resource, changeset)
    end
  end

  defmodule ManualUpdatePost do
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
      defaults [:read, :destroy, create: :*]

      update :update_manual_with_before_hooks do
        accept [:title]
        manual ManualUpdateSimple
        change {BeforeTransactionChange, attribute: :title2, value: "before_transaction"}
        change {BeforeActionChange, attribute: :title3, value: "before_action"}
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true
      attribute :title3, :string, public?: true
    end
  end

  defmodule PolicyPost do
    @moduledoc """
    Reading and creating are allowed for everyone, updating only for actors with
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
      policy action(:update_with_initial_data_before_hooks) do
        authorize_if OriginalTitlePolicyCheck
      end

      policy action_type(:read) do
        authorize_if always()
      end

      policy action_type(:create) do
        authorize_if always()
      end

      policy action_type(:update) do
        forbid_unless actor_attribute_equals(:allow, true)
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:destroy, create: :*]

      read :read do
        primary? true
        pagination keyset?: true, required?: false
      end

      update :update_with_initial_data_before_hooks do
        accept [:title]
        require_atomic? false
        change BeforeTransactionChange
        change BeforeActionChange
        change AfterTransactionHandlingErrors
      end

      update :update do
        accept [:title]
        primary? true
      end

      update :update_with_before_hooks do
        accept [:title]
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

  describe ":atomic strategy" do
    for phase <- [:before_action, :before_transaction] do
      @phase phase

      test "#{phase} replacing a lazy update default enforces attribute constraints" do
        [post] = create_posts(1)

        result =
          Ash.bulk_update(Post, :"update_with_#{@phase}_defaulted_counter", %{title: "changed"},
            strategy: :atomic,
            return_errors?: true
          )

        assert result.status == :error
        assert result.error_count == 1

        assert Enum.any?(
                 result.errors,
                 &(Exception.message(&1) =~ "must be less than or equal to 5")
               )

        unchanged = Ash.get!(Post, post.id)
        assert unchanged.defaulted_counter == 0
        assert unchanged.title == post.title
      end
    end

    test "atomic updates added by before_transaction enforce attribute constraints" do
      [post] = create_posts(1)
      input = Post

      result =
        Ash.bulk_update(
          input,
          :update_with_before_transaction_constrained_atomic_update,
          %{title: "changed"},
          strategy: :atomic,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1

      assert Enum.any?(
               result.errors,
               &(Exception.message(&1) =~ "must be less than or equal to 5")
             )

      unchanged = Ash.get!(Post, post.id)
      assert unchanged.constrained_counter == 0
      assert unchanged.title == post.title
    end

    test "before_action and before_transaction hooks execute once with the sentinel" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_all_hooks, %{}, strategy: :atomic)

      assert result.status == :success

      assert_received {:hook, :before_transaction}
      assert_received {:hook, :before_action}
      refute_received {:hook, :before_transaction}
      refute_received {:hook, :before_action}
    end

    test "hooks run in the same order as on the non-atomic path" do
      posts = create_posts(1)

      posts
      |> query_for()
      |> Ash.bulk_update!(:update_with_all_hooks, %{}, strategy: :atomic)

      assert_received {:hook, first}
      assert_received {:hook, second}
      assert_received {:hook, third}
      assert_received {:hook, fourth}
      refute_received {:hook, _}

      assert [first, second, third, fourth] ==
               [:before_transaction, :before_action, :after_action, :after_transaction]
    end

    test "hook receives the sentinel as data and the action and arguments" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action_argument, %{suffix: "value"},
          strategy: :atomic,
          return_records?: true
        )

      assert Enum.map(result.records, & &1.title2) == ["arg_value", "arg_value"]
      assert_received {:before_action_action, :update_with_before_action_argument}
    end

    test "attribute changes made in a before_action hook are applied" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert Enum.map(result.records, & &1.title2) == ["before_action", "before_action"]

      assert_received {:before_action_called,
                       %OriginalDataNotAvailable{reason: :atomic_query_update}}

      refute_received {:before_action_called, _}

      assert query_for(posts) |> Ash.read!() |> Enum.map(& &1.title2) ==
               ["before_action", "before_action"]
    end

    test "attribute changes made in a before_transaction hook are applied" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_transaction, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert Enum.map(result.records, & &1.title2) ==
               ["before_transaction", "before_transaction"]

      assert_received {:before_transaction_called,
                       %OriginalDataNotAvailable{reason: :atomic_query_update}}

      refute_received {:before_transaction_called, _}
    end

    test "atomic updates added in a before_action hook are applied" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action_atomic_update, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert Enum.map(result.records, & &1.counter) == [10, 10]
    end

    test "a before_transaction atomic update is persisted without a filter or returned records" do
      [post] = create_posts(1)

      result =
        Ash.bulk_update!(Post, :update_with_before_transaction_atomic_update, %{},
          strategy: :atomic
        )

      assert result.status == :success
      updated = Ash.get!(Post, post.id)
      assert updated.counter == 10
      assert DateTime.compare(updated.updated_at, post.updated_at) == :gt
    end

    test "a before_transaction atomic update applies update defaults when records are returned" do
      [post] = create_posts(1)

      result =
        Ash.bulk_update!(Post, :update_with_before_transaction_atomic_update, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert result.status == :success
      assert [updated] = result.records
      assert updated.counter == 10
      assert DateTime.compare(updated.updated_at, post.updated_at) == :gt
    end

    test "a change made only in a before hook bumps the update timestamp" do
      [post] = create_posts(1)

      result =
        [post]
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action, %{},
          strategy: :atomic,
          return_records?: true
        )

      [updated] = result.records
      assert DateTime.compare(updated.updated_at, post.updated_at) == :gt
    end

    test "filters added in a before_action hook narrow the affected records" do
      match = Post |> Ash.Changeset.for_create(:create, %{title: "match"}) |> Ash.create!()
      other = Post |> Ash.Changeset.for_create(:create, %{title: "other"}) |> Ash.create!()

      result =
        [match, other]
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action_filter, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert [%{title: "match", title2: "filtered"}] = result.records
      assert Ash.get!(Post, other.id).title2 == nil
    end

    test "hooks execute without return_records?" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action, %{}, strategy: :atomic)

      assert result.status == :success
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert posts |> query_for() |> Ash.read!() |> Enum.all?(&(&1.title2 == "before_action"))
    end

    test "empty result set still runs the hooks and succeeds" do
      result =
        Post
        |> Ash.Query.filter(title == "does_not_exist")
        |> Ash.bulk_update!(:update_with_all_hooks, %{}, strategy: :atomic)

      assert result.status == :success
      assert_received {:hook, :before_transaction}
      assert_received {:hook, :before_action}
    end

    test "multiple before_action hooks execute in order, honouring prepend?" do
      posts = create_posts(1)

      posts
      |> query_for()
      |> Ash.bulk_update!(:update_with_multiple_before_action_hooks, %{}, strategy: :atomic)

      assert_received {:before_action_hook, first}
      assert_received {:before_action_hook, second}
      assert_received {:before_action_hook, third}
      assert [first, second, third] == [0, 1, 2]
    end

    test "an invalid changeset returned from before_action is captured in the result" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update(:update_with_before_action_error, %{},
          strategy: :atomic,
          return_errors?: true,
          return_records?: true
        )

      assert %Ash.BulkResult{status: :error, errors: [error], records: []} = result
      assert result.error_count == 1
      assert %Ash.Error.Invalid{} = error
      assert Exception.message(error) =~ "before_action failed"

      assert query_for(posts) |> Ash.read!() |> Enum.map(& &1.title2) == [nil, nil]
    end

    test "an error tuple returned from before_action is captured in the result" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update(:update_with_before_action_error_tuple, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert %Ash.BulkResult{status: :error, errors: [error]} = result
      assert Exception.message(error) =~ "before_action error tuple"
    end

    test "an invalid changeset returned from before_transaction is captured in the result" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update(:update_with_before_transaction_error, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert %Ash.BulkResult{status: :error, errors: [error]} = result
      assert Exception.message(error) =~ "before_transaction failed"
      assert query_for(posts) |> Ash.read!() |> Enum.map(& &1.title2) == [nil, nil]
    end

    test "after_transaction hooks added by a before_transaction hook run on failure" do
      posts = create_posts(1)

      posts
      |> query_for()
      |> Ash.bulk_update(:update_with_before_transaction_error, %{},
        strategy: :atomic,
        return_errors?: true
      )

      assert_received {:after_transaction_from_before_transaction, {:error, _}}
    end

    test "after_action hooks added by a before_action hook run with the records" do
      posts = create_posts(3)
      ids = posts |> Enum.map(& &1.id) |> Enum.sort()

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action_adding_after_action, %{},
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
      |> Ash.bulk_update!(:update_with_before_action_adding_after_transaction, %{},
        strategy: :atomic
      )

      assert_received {:after_transaction_from_before_action, {:ok, _}}
    end

    test "return_records?: true and return_errors?: true work together on success" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action, %{},
          strategy: :atomic,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :success
      assert result.errors == []
      assert length(result.records) == 2
    end

    test "notifications returned by a before_action hook are returned with return_notifications?" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action_notification, %{},
          strategy: :atomic,
          return_notifications?: true
        )

      assert result.status == :success

      assert [%Ash.Notifier.Notification{metadata: %{source: :before_action}}] =
               Enum.filter(result.notifications, &(&1.metadata[:source] == :before_action))

      # the resource notifications for the updated records are returned too
      assert Enum.count(result.notifications, &is_nil(&1.metadata[:source])) == 2
    end

    test "notifications returned by a before_action hook are sent with notify?" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action_notification, %{},
          strategy: :atomic,
          notify?: true
        )

      assert result.status == :success
      assert_received {:notification, %{metadata: %{source: :before_action}}}
      assert_received {:notification, %{data: %Post{}}}
      assert_received {:notification, %{data: %Post{}}}
    end

    test "hooks work with notify?: true" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action, %{},
          strategy: :atomic,
          notify?: true
        )

      assert result.status == :success
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert_received {:notification, %{data: %Post{title2: "before_action"}}}
      assert_received {:notification, %{data: %Post{title2: "before_action"}}}
    end

    test "load option works with hooks" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action, %{},
          strategy: :atomic,
          return_records?: true,
          load: [:shout]
        )

      assert Enum.all?(result.records, &(&1.shout == &1.title <> "!"))
      assert Enum.all?(result.records, &(&1.title2 == "before_action"))
    end

    test "hooks work with a resource without notifiers, validations or policies" do
      post =
        SimplePost
        |> Ash.Changeset.for_create(:create, %{title: "simple"})
        |> Ash.create!()

      result =
        SimplePost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:update_with_before_hooks, %{},
          strategy: :atomic,
          return_records?: true
        )

      assert [%{title: "before_action", body: "before_transaction"}] = result.records
      assert_received {:before_transaction_called, %OriginalDataNotAvailable{}}
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
    end

    test "hooks are not run when the atomic changeset is invalid before execution" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update(:update_with_before_hooks_and_eager_validation, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert result.status == :error
      assert [error] = result.errors
      assert Exception.message(error) =~ "always fails"

      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      # after_transaction hooks still run for the failed changeset
      assert_received {:after_transaction_error, _}
    end

    test "around_action hooks added in atomic/3 make the action not atomic" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update(:update_with_around_action, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert %Ash.BulkResult{status: :error, errors: [error]} = result
      assert Exception.message(error) =~ "around_action"
      refute_received :around_action_called
    end

    test "strategy fallback with require_atomic?: false runs around hooks on the stream path" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_around_action_fallback, %{},
          strategy: [:atomic, :stream]
        )

      assert result.status == :success
      assert_received :around_action_called
      assert_received :around_action_called
    end

    test "strategy fallback to stream runs before hooks with the real records" do
      posts = create_posts(2)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_hooks_not_atomic, %{},
          strategy: [:atomic, :stream],
          return_records?: true
        )

      assert Enum.all?(
               result.records,
               &(&1.title2 == "before_transaction" and &1.title3 == "before_action")
             )

      for _ <- 1..2 do
        assert_received {:before_transaction_called, %Post{}}
        assert_received {:before_action_called, %Post{}}
      end
    end
  end

  describe "update defaults after before hooks" do
    for strategy <- [:atomic, :atomic_batches], phase <- [:before_action, :before_transaction] do
      @strategy strategy
      @phase phase

      test "#{phase} changes advance the timestamp with unchanged input using #{strategy}" do
        [post] = create_posts(1)
        input = if @strategy == :atomic, do: query_for([post]), else: [post]

        result =
          Ash.bulk_update!(input, :"update_with_#{@phase}", %{title: post.title},
            strategy: @strategy,
            return_records?: true
          )

        assert [updated] = result.records
        assert updated.title == post.title
        assert updated.title2 == Atom.to_string(@phase)
        assert DateTime.compare(updated.updated_at, post.updated_at) == :gt
        assert Ash.get!(Post, post.id).updated_at == updated.updated_at
        assert_received :defaulted_counter_evaluated
        refute_received :defaulted_counter_evaluated
      end

      test "#{phase} restores the original value without advancing the timestamp using #{strategy}" do
        [post] = create_posts(1)
        input = if @strategy == :atomic, do: query_for([post]), else: [post]

        result =
          Ash.bulk_update!(input, :"update_with_#{@phase}_restoring_title", %{title: "changed"},
            strategy: @strategy,
            return_records?: true
          )

        assert [updated] = result.records
        assert updated.title == post.title
        assert updated.updated_at == post.updated_at
      end

      test "#{phase} can replace the timestamp default using #{strategy}" do
        [post] = create_posts(1)
        input = if @strategy == :atomic, do: query_for([post]), else: [post]

        result =
          Ash.bulk_update!(input, :"update_with_#{@phase}_timestamp", %{title: "changed"},
            strategy: @strategy,
            return_records?: true
          )

        assert [updated] = result.records
        assert updated.title == "changed"
        assert updated.updated_at == ~U[2000-01-01 00:00:00.000000Z]
      end
    end
  end

  describe ":atomic_batches strategy" do
    test "atomic updates added by before_transaction enforce attribute constraints" do
      [post] = create_posts(1)
      input = [post]

      result =
        Ash.bulk_update(
          input,
          :update_with_before_transaction_constrained_atomic_update,
          %{title: "changed"},
          strategy: :atomic_batches,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1

      assert Enum.any?(
               result.errors,
               &(Exception.message(&1) =~ "must be less than or equal to 5")
             )

      unchanged = Ash.get!(Post, post.id)
      assert unchanged.constrained_counter == 0
      assert unchanged.title == post.title
    end

    test "hooks execute once per batch with the sentinel" do
      posts = create_posts(5)

      result =
        Ash.bulk_update!(posts, :update_with_all_hooks, %{},
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
    end

    test "before_action runs once per batch and changes the returned records" do
      posts = create_posts(5)

      result =
        Ash.bulk_update!(posts, :update_with_before_action, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_records?: true
        )

      assert length(result.records) == 5
      assert Enum.all?(result.records, &(&1.title2 == "before_action"))

      for _ <- 1..3 do
        assert_received {:before_action_called,
                         %OriginalDataNotAvailable{reason: :atomic_query_update}}
      end

      refute_received {:before_action_called, _}
    end

    test "before_transaction changes are applied" do
      posts = create_posts(3)

      result =
        Ash.bulk_update!(posts, :update_with_before_transaction, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_records?: true
        )

      assert Enum.all?(result.records, &(&1.title2 == "before_transaction"))
    end

    test "hook error is captured and nothing is written" do
      posts = create_posts(5)

      result =
        Ash.bulk_update(posts, :update_with_before_action_error, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert Enum.all?(result.errors, &(Exception.message(&1) =~ "before_action failed"))
      assert query_for(posts) |> Ash.read!() |> Enum.all?(&is_nil(&1.title2))
      assert_received {:after_transaction_error, _}
    end

    test "hooks work with notify?: true" do
      posts = create_posts(3)

      result =
        Ash.bulk_update!(posts, :update_with_before_action, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          notify?: true
        )

      assert result.status == :success

      for _ <- 1..3 do
        assert_received {:notification, %{data: %Post{title2: "before_action"}}}
      end
    end

    test "load option works with hooks" do
      posts = create_posts(3)

      result =
        Ash.bulk_update!(posts, :update_with_before_action, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_records?: true,
          load: [:shout]
        )

      assert Enum.all?(result.records, &(&1.shout == &1.title <> "!"))
    end

    test "after_action hooks added by a before_action hook run for every record" do
      posts = create_posts(5)

      Ash.bulk_update!(posts, :update_with_before_action_adding_after_action, %{},
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
        |> Ash.bulk_update!(:update_with_all_hooks, %{}, strategy: :stream)

      assert result.status == :success

      for _ <- 1..3 do
        assert_received {:hook, :before_transaction}
        assert_received {:hook, :before_action}
      end
    end

    test "hook changes are applied" do
      posts = create_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action, %{},
          strategy: :stream,
          return_records?: true
        )

      assert Enum.all?(result.records, &(&1.title2 == "before_action"))

      for _ <- 1..3 do
        assert_received {:before_action_called, %Post{}}
      end
    end

    test "with return_stream?: true streams results" do
      posts = create_posts(3)

      results =
        posts
        |> query_for()
        |> Ash.bulk_update!(:update_with_before_action, %{},
          strategy: :stream,
          return_stream?: true,
          return_records?: true
        )
        |> Enum.to_list()

      assert length(results) == 3
      assert Enum.all?(results, &match?({:ok, %Post{title2: "before_action"}}, &1))
    end

    test "hook error stops processing with stop_on_error?: true" do
      posts = create_posts(5)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update(:update_with_before_action_error, %{},
          strategy: :stream,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert length(result.errors) == 1
    end

    test "hook error continues processing with stop_on_error?: false" do
      posts = create_posts(5)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update(:update_with_before_action_error, %{},
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
        |> Ash.bulk_update!(:update_with_before_action_notification, %{},
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
        |> Ash.bulk_update!(:update_with_before_action, %{},
          strategy: :stream,
          transaction: :all,
          return_records?: true
        )

      assert Enum.all?(result.records, &(&1.title2 == "before_action"))
    end
  end

  describe "atomic before hooks touching another resource" do
    setup do
      capture_log(fn -> Ash.DataLayer.Mnesia.start(Domain, [MnesiaAudit]) end)
      :ok
    end

    test "query updates open the declared touched resource transaction before before_action" do
      [post] = create_posts(1)

      result =
        Ash.bulk_update!(Post, :update_with_touched_resource_before_action, %{title: "updated"},
          strategy: :atomic
        )

      assert result.status == :success
      assert_received {:audit_transaction?, true}
      refute_received {:audit_transaction?, _}
      assert Ash.get!(Post, post.id).title == "updated"
    end

    test "single-record atomic upgrades open the declared touched resource transaction" do
      [post] = create_posts(1)

      updated =
        post
        |> Ash.Changeset.for_update(:update_with_touched_resource_before_action, %{
          title: "updated"
        })
        |> Ash.update!()

      assert updated.title == "updated"
      assert_received {:audit_transaction?, true}
      refute_received {:audit_transaction?, _}
      assert Ash.get!(Post, post.id).title == "updated"
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
        |> Ash.bulk_update(:update_with_side_effect_before_action, %{},
          strategy: :stream,
          transaction: :all,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      assert %Ash.BulkResult{status: :error} = result

      assert MnesiaPost |> Ash.read!() |> Enum.map(& &1.title2) |> Enum.uniq() == [nil]
      assert Ash.read!(MnesiaAudit) == []
    end

    @tag :capture_log
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
        |> Ash.bulk_update(:update_with_side_effect_before_action, %{},
          strategy: :stream,
          transaction: :batch,
          batch_size: 2,
          rollback_on_error?: true,
          return_errors?: true
        )

      assert %Ash.BulkResult{errors: [error]} = result
      assert Exception.message(error) =~ "failed on purpose"

      updated =
        MnesiaPost
        |> Ash.read!()
        |> Enum.filter(&(&1.title2 == "updated"))
        |> Enum.map(&"audit_#{&1.title}")
        |> Enum.sort()

      audits = MnesiaAudit |> Ash.read!() |> Enum.map(& &1.note) |> Enum.sort()

      # The batch holding fail_3 rolled back, including the audit rows its hooks wrote,
      # so the failing record has no audit row and every audit row that survived belongs
      # to a record whose update committed in another batch. Which records share a
      # batch with fail_3 depends on the read order, so only consistency is asserted.
      refute "audit_fail_3" in audits
      assert audits == updated
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
            |> Ash.bulk_update!(:update_with_before_transaction, %{},
              strategy: :stream,
              transaction: :batch,
              return_records?: true
            )

          assert [%{title2: "before_transaction"}] = result.records
        end)

      refute log =~ "before_transaction"
      assert_received {:before_transaction_called, %MnesiaPost{}}
    end
  end

  describe "single record atomic upgrade" do
    test "hooks run with the real record as data" do
      [%{id: id} = post] = create_posts(1)

      updated =
        post
        |> Ash.Changeset.for_update(:update_with_all_hooks, %{})
        |> Ash.update!()

      assert updated.id == id

      assert_received {:hook, :before_transaction}
      assert_received {:hook, :before_action}
      assert_received {:hook, :after_action}
      assert_received {:hook, :after_transaction}
    end

    test "hook changes are applied and the hook sees the record" do
      [%{id: id} = post] = create_posts(1)

      assert %{title2: "before_action"} =
               post
               |> Ash.Changeset.for_update(:update_with_before_action, %{})
               |> Ash.update!()

      assert_received {:before_action_called, %Post{id: ^id}}
    end

    test "arguments are available in the hook" do
      [post] = create_posts(1)

      assert %{title2: "arg_single"} =
               post
               |> Ash.Changeset.for_update(:update_with_before_action_argument, %{
                 suffix: "single"
               })
               |> Ash.update!()
    end

    test "a hook error fails the update and leaves the record unchanged" do
      [%{id: id} = post] = create_posts(1)

      assert {:error, %Ash.Error.Invalid{} = error} =
               post
               |> Ash.Changeset.for_update(:update_with_before_action_error, %{})
               |> Ash.update()

      assert Exception.message(error) =~ "before_action failed"
      assert Ash.get!(Post, id).title2 == nil
      assert_received {:after_transaction_error, _}
    end

    test "a before_transaction error fails the update" do
      [post] = create_posts(1)

      assert {:error, error} =
               post
               |> Ash.Changeset.for_update(:update_with_before_transaction_error, %{})
               |> Ash.update()

      assert Exception.message(error) =~ "before_transaction failed"
      assert_received {:after_transaction_from_before_transaction, {:error, _}}
    end

    test "hooks added directly to the changeset still prevent the atomic upgrade" do
      [post] = create_posts(1)

      assert {:error, %Ash.Error.Framework{errors: [%Ash.Error.Framework.MustBeAtomic{} = error]}} =
               post
               |> Ash.Changeset.for_update(:update_with_before_action, %{})
               |> Ash.Changeset.before_transaction(& &1)
               |> Ash.update()

      assert error.reason =~ "hooks in phases [:before_transaction]"
    end
  end

  describe "manual actions" do
    test "before hooks run with the real record on a manual update" do
      post =
        ManualUpdatePost
        |> Ash.Changeset.for_create(:create, %{title: "manual"})
        |> Ash.create!()

      result =
        ManualUpdatePost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:update_manual_with_before_hooks, %{title: "updated"},
          strategy: :stream,
          return_records?: true
        )

      assert [%{title: "updated", title2: "before_transaction", title3: "before_action"}] =
               result.records

      assert_received {:before_transaction_called, %ManualUpdatePost{}}
      assert_received {:before_action_called, %ManualUpdatePost{}}
    end

    test "before hooks run for every record with multiple batches" do
      posts = create_posts(ManualUpdatePost, 5, "manual")

      result =
        posts
        |> query_for(ManualUpdatePost)
        |> Ash.bulk_update!(:update_manual_with_before_hooks, %{title: "updated"},
          strategy: :stream,
          batch_size: 2,
          return_records?: true
        )

      assert length(result.records) == 5

      for _ <- 1..5 do
        assert_received {:before_action_called, %ManualUpdatePost{}}
      end
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
            :update_with_initial_data_before_hooks,
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
          Ash.bulk_update!(input, :update_with_initial_data_before_hooks, %{title: "updated"},
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
        assert Enum.map(remaining, & &1.title) == ["updated", "updated"]
      end
    end

    test "a policy denying the loaded record prevents before hooks after fallback" do
      [post] = create_posts(PolicyPost, 1, "denied")
      input = query_for([post], PolicyPost)

      result =
        Ash.bulk_update(input, :update_with_initial_data_before_hooks, %{title: "updated"},
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
        |> Ash.bulk_update(:update_with_before_hooks, %{title: "updated"},
          strategy: :atomic,
          authorize?: true,
          actor: %{allow: false},
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, %Ash.Error.Forbidden{}}
      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert Ash.get!(PolicyPost, post.id, authorize?: false).title == "forbidden_1"
    end

    test "before hooks are not called on a forbidden error with :atomic_batches strategy" do
      posts = create_posts(PolicyPost, 5, "forbidden")

      result =
        Ash.bulk_update(posts, :update_with_before_hooks, %{title: "updated"},
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
    end

    test "before hooks are not called on a forbidden error with :stream strategy" do
      posts = create_posts(PolicyPost, 5, "forbidden")

      result =
        posts
        |> query_for(PolicyPost)
        |> Ash.bulk_update(:update_with_before_hooks, %{title: "updated"},
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
    end

    test "before hooks run when the actor is allowed with :atomic strategy" do
      [post] = create_posts(PolicyPost, 1, "allowed")

      result =
        PolicyPost
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update!(:update_with_before_hooks, %{title: "updated"},
          strategy: :atomic,
          authorize?: true,
          actor: %{allow: true},
          return_records?: true
        )

      assert [%{title: "updated"}] = result.records
      assert_received {:before_transaction_called, %OriginalDataNotAvailable{}}
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert_received {:after_transaction_success, _}
    end
  end

  describe "validation errors" do
    # The validation checks the *stored* title, so with no title in the input it can only
    # be decided by the data layer on the atomic path, but it is decided while building
    # each changeset on the stream path.
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
        |> Ash.bulk_update(:update_with_before_hooks_and_atomic_validation, %{},
          strategy: :atomic,
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, error}
      assert Exception.message(error) =~ "atomic validation failed"
      assert_received {:before_transaction_called, %OriginalDataNotAvailable{}}
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert Ash.get!(Post, post.id).title2 == nil
    end

    test "hooks run before a validation that fails in the data layer with :atomic_batches strategy" do
      posts = create_failing_posts(5)

      result =
        Ash.bulk_update(posts, :update_with_before_hooks_and_atomic_validation, %{},
          strategy: :atomic_batches,
          batch_size: 2,
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, _}
      assert_received {:before_transaction_called, %OriginalDataNotAvailable{}}
      assert_received {:before_action_called, %OriginalDataNotAvailable{}}
      assert posts |> query_for() |> Ash.read!() |> Enum.all?(&is_nil(&1.title2))
    end

    test "hooks are not run when the validation fails while building the changeset with :atomic strategy" do
      [post] = create_posts(1)

      result =
        Post
        |> Ash.Query.filter(id == ^post.id)
        |> Ash.bulk_update(:update_with_before_hooks_and_atomic_validation, %{title: "will_fail"},
          strategy: :atomic,
          return_errors?: true
        )

      assert result.status == :error
      assert_received {:after_transaction_error, _}
      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert Ash.get!(Post, post.id).title == "title_1"
    end

    test "hooks are not run for a changeset that fails validation with :stream strategy" do
      posts = create_failing_posts(3)

      result =
        posts
        |> query_for()
        |> Ash.bulk_update(:update_with_before_hooks_and_atomic_validation, %{},
          strategy: :stream,
          return_errors?: true
        )

      assert result.status == :error
      refute_received {:before_transaction_called, _}
      refute_received {:before_action_called, _}
      assert_received {:after_transaction_error, _}
    end
  end
end
