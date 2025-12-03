# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkUpdateTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  require Ash.Query
  import Ash.Expr

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  defmodule AddAfterToTitle do
    use Ash.Resource.Change

    @impl true
    def change(changeset, _, _) do
      changeset
    end

    @impl true
    def batch_change(changesets, _opts, _context) do
      changesets
    end

    @impl true
    def after_batch(results, _, _) do
      Stream.map(results, fn {_changeset, result} ->
        {:ok, %{result | title: result.title <> "_after"}}
      end)
    end
  end

  defmodule AddBeforeToTitle do
    use Ash.Resource.Change

    @impl true
    def batch_change(changesets, _opts, _context) do
      changesets
    end

    @impl true
    def before_batch(changesets, _, _) do
      Stream.map(changesets, fn changeset ->
        title = Ash.Changeset.get_attribute(changeset, :title)
        Ash.Changeset.force_change_attribute(changeset, :title, "before_" <> title)
      end)
    end
  end

  defmodule RecordBatchSizes do
    use Ash.Resource.Change

    @impl true
    def batch_change(changesets, _, _) do
      batch_size = length(changesets)

      Stream.map(changesets, fn changeset ->
        Ash.Changeset.force_change_attribute(changeset, :change_batch_size, batch_size)
      end)
    end

    @impl true
    def before_batch(changesets, _, _) do
      batch_size = length(changesets)

      Stream.map(changesets, fn changeset ->
        Ash.Changeset.force_change_attribute(changeset, :before_batch_size, batch_size)
      end)
    end

    @impl true
    def after_batch(results, _, _) do
      batch_size = length(results)

      Stream.map(results, fn {_, result} ->
        {:ok, %{result | after_batch_size: batch_size}}
      end)
    end
  end

  defmodule AtomicallyRequireActor do
    use Ash.Resource.Change

    def change(changeset, _, context) do
      if context.actor do
        changeset
      else
        Ash.Changeset.add_error(changeset, "actor is required")
      end
    end

    def atomic(changeset, _, context) do
      if context.actor do
        {:atomic, changeset, %{}}
      else
        {:atomic, Ash.Changeset.add_error(changeset, "actor is required"), %{}}
      end
    end
  end

  defmodule ManualUpdate do
    use Ash.Resource.ManualUpdate

    def update(changeset, _opts, _context) do
      {:ok, changeset.data}
    end
  end

  defmodule Address do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: :embedded

    attributes do
      attribute :line1, :string, allow_nil?: false, public?: true
      attribute :line2, :string, allow_nil?: true, public?: true
      attribute :city, :string, allow_nil?: false, public?: true
      attribute :state, :string, allow_nil?: false, public?: true
      attribute :zip_code, :string, allow_nil?: false, public?: true
      attribute :country, :string, allow_nil?: false, public?: true
    end
  end

  defmodule Contact do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: :embedded

    # actions do
    #   defaults [:read, :create, :update, :destroy]
    # end

    attributes do
      attribute :email, :string, allow_nil?: false, public?: true
      attribute :address, Address, allow_nil?: false, public?: true
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

      attribute :contact, Contact do
        public? true
      end
    end

    relationships do
      has_many :posts, Ash.Test.Actions.BulkUpdateTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      attribute :type, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :source_post, Ash.Test.Actions.BulkUpdateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_post, Ash.Test.Actions.BulkUpdateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule AtomicOnlyValidation do
    use Ash.Resource.Validation

    @impl true
    def atomic(_, _, _) do
      :ok
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier],
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    validations do
      validate AtomicOnlyValidation,
        on: [:update]
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      read :query_with_after_action do
        pagination keyset?: true

        prepare fn query, _ ->
          Ash.Query.after_action(query, fn _query, records ->
            {:ok, records}
          end)
        end
      end

      update :update_manual do
        manual ManualUpdate
      end

      update :update_with_change do
        require_atomic? false

        change fn changeset, _ ->
          title = Ash.Changeset.get_attribute(changeset, :title)
          Ash.Changeset.force_change_attribute(changeset, :title, title <> "_stuff")
        end
      end

      update :update_with_argument do
        require_atomic? false

        argument :a_title, :string do
          allow_nil? false
        end

        change set_attribute(:title2, arg(:a_title))
      end

      update :update_with_before_transaction do
        require_atomic? false

        change before_transaction(fn changeset, _context ->
                 title = Ash.Changeset.get_attribute(changeset, :title)

                 Ash.Changeset.force_change_attribute(
                   changeset,
                   :title,
                   "before_transaction_" <> title
                 )
               end)
      end

      update :update_with_after_action do
        require_atomic? false

        change after_action(fn _changeset, result, _context ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      update :update_with_after_batch do
        require_atomic? false
        change AddAfterToTitle
        change AddBeforeToTitle
      end

      update :update_with_batch_sizes do
        require_atomic? false
        change RecordBatchSizes
      end

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

      update :update_with_policy do
        require_atomic? false
        argument :authorize?, :boolean, allow_nil?: false

        change set_context(%{authorize?: arg(:authorize?)})
        change AtomicallyRequireActor
      end

      update :update_with_policy_user_sets_context do
        require_atomic? false

        change AtomicallyRequireActor
      end

      update :update_with_policy_without_requiring_actor do
        argument :authorize?, :boolean, allow_nil?: false

        change set_context(%{authorize?: arg(:authorize?)})
      end

      update :update_with_match do
        validate match(:title4, "^[a-z]+$"), message: "Title must be lowercase"
      end

      update :update_with_filter do
        change filter(expr(title == "foo"))
      end
    end

    identities do
      identity :unique_title, :title do
        pre_check_with Ash.Test.Actions.BulkUpdateTest.Domain
      end
    end

    calculations do
      calculate :hidden_calc, :string, expr("something") do
        public?(true)
      end
    end

    field_policies do
      field_policy [:hidden_calc, :hidden_attribute] do
        forbid_if always()
      end

      field_policy :* do
        authorize_if always()
      end
    end

    code_interface do
      define :update_with_policy_without_requiring_actor
    end

    policies do
      policy action(:update_with_policy) do
        authorize_if context_equals(:authorize?, true)
      end

      policy action(:update_with_policy_without_requiring_actor) do
        authorize_if context_equals(:authorize?, true)
      end

      policy action(:update_with_policy_user_sets_context) do
        authorize_if context_equals(:authorize?, true)
      end

      policy always() do
        authorize_if always()
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true
      attribute :title3, :string, public?: true
      attribute :title4, :string, public?: true
      attribute :hidden_attribute, :string, public?: true

      attribute :score, :integer, public?: true, constraints: [min: 0]

      attribute :before_batch_size, :integer
      attribute :after_batch_size, :integer
      attribute :change_batch_size, :integer

      timestamps()
    end

    relationships do
      belongs_to :author, Author, public?: true

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id,
        public?: true
    end
  end

  defmodule MnesiaPost do
    @doc false

    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia, notifiers: [Notifier]

    mnesia do
      table :mnesia_post_updates
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true

      timestamps()
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create, :update]

      update :error_in_before_action_on_title2 do
        atomic_upgrade? false

        change before_action(fn changeset, context ->
                 if changeset.data.title == "title2" do
                   Ash.Changeset.add_error(changeset, "not good")
                 else
                   changeset
                 end
               end)
      end

      update :update_with_after_action do
        atomic_upgrade? false

        change after_action(fn changeset, result, context ->
                 result
                 |> Ash.Changeset.for_update(:other_update, %{}, Ash.Context.to_opts(context))
                 |> Ash.update()
               end)
      end

      update :other_update

      update :update_with_nested_bulk_update do
        atomic_upgrade? false

        change after_action(fn changeset, result, context ->
                 other_posts = MnesiaPost |> Ash.read!()

                 other_posts
                 |> Enum.reject(fn post -> post.id == result.id end)
                 |> Ash.bulk_update!(:update, %{title2: "nested_update"},
                   notify?: true,
                   strategy: :stream,
                   return_records?: false
                 )

                 {:ok, result}
               end)
      end
    end
  end

  defmodule Tenant do
    @doc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id, writable?: true
    end

    defimpl Ash.ToTenant do
      def to_tenant(tenant, _resource), do: tenant.id
    end
  end

  defmodule MultitenantTagLink do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy :attribute
      attribute :tenant_id
    end

    attributes do
      attribute :type, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :tenant, Tenant, allow_nil?: false, primary_key?: true

      belongs_to :source_tag, Ash.Test.Actions.BulkUpdateTest.MultitenantTag,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_tag, Ash.Test.Actions.BulkUpdateTest.MultitenantTag,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule MultitenantTag do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :tenant_id
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :add_related_tags do
        require_atomic? false

        argument :related_tags, {:array, :string} do
          allow_nil? false
          constraints min_length: 1, items: [min_length: 1]
        end

        change manage_relationship(:related_tags,
                 on_lookup: :relate,
                 on_no_match: :create,
                 value_is_key: :name,
                 use_identities: [:name]
               )
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: false, public?: true

      timestamps()
    end

    relationships do
      belongs_to :tenant, Tenant, allow_nil?: false

      many_to_many :related_tags, __MODULE__,
        through: MultitenantTagLink,
        source_attribute_on_join_resource: :source_tag_id,
        destination_attribute_on_join_resource: :destination_tag_id,
        public?: true
    end

    identities do
      identity :name, [:name], pre_check_with: Domain
    end
  end

  setup do
    on_exit(fn ->
      capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)

    capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Domain, [MnesiaPost])
    end)

    :ok
  end

  test "returns updated records" do
    assert %Ash.BulkResult{
             records: [%{title2: "updated value"}, %{title2: "updated value"}] = records
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update, %{title2: "updated value"},
               resource: Post,
               strategy: :atomic_batches,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )

    Enum.each(records, fn record ->
      refute Map.has_key?(record.__metadata__, :bulk_action_ref)
    end)
  end

  test "sends notifications" do
    assert %Ash.BulkResult{records: [%{title2: "updated value"}, %{title2: "updated value"}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update, %{title2: "updated value"},
               resource: Post,
               strategy: :atomic_batches,
               return_records?: true,
               notify?: true,
               return_errors?: true,
               authorize?: false
             )

    assert_received {:notification, %{data: %{title: "title2"}}}
    assert_received {:notification, %{data: %{title: "title2"}}}
  end

  test "sends notifications with stream strategy in transactions" do
    assert %Ash.BulkResult{records: [%{title2: "updated value"}, %{title2: "updated value"}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], MnesiaPost, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update, %{title2: "updated value"},
               resource: MnesiaPost,
               strategy: :stream,
               allow_stream_with: :full_read,
               return_records?: true,
               notify?: true,
               return_errors?: true,
               authorize?: false
             )

    assert_received {:notification,
                     %{
                       data: %{title: "title1"},
                       changeset: %{attributes: %{title2: "updated value"}}
                     }}

    assert_received {:notification,
                     %{
                       data: %{title: "title2"},
                       changeset: %{attributes: %{title2: "updated value"}}
                     }}
  end

  test "sends only the correct amount notifications with stream strategy in transactions, when run in a before action hook" do
    MnesiaPost
    |> Ash.Changeset.for_create(:create, %{title: "title_pre"})
    |> Ash.Changeset.before_action(fn changeset ->
      Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], MnesiaPost, :create,
        return_stream?: true,
        return_records?: true,
        authorize?: false
      )
      |> Stream.map(fn {:ok, result} ->
        result
      end)
      |> Ash.bulk_update!(:update, %{title2: "updated value"},
        resource: MnesiaPost,
        strategy: :stream,
        allow_stream_with: :full_read,
        return_records?: true,
        notify?: true,
        return_errors?: true,
        authorize?: false
      )

      changeset
    end)
    |> Ash.create!()

    assert_received {:notification, %{changeset: %{action: %{type: :create}}}}

    assert_received {:notification,
                     %{
                       data: %{title: "title1"},
                       changeset: %{attributes: %{title2: "updated value"}}
                     }}

    assert_received {:notification,
                     %{
                       data: %{title: "title2"},
                       changeset: %{attributes: %{title2: "updated value"}}
                     }}

    refute_received {:notification, _}
  end

  test "rolls back on errors in a `before_action` hook" do
    Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], MnesiaPost, :create,
      return_stream?: true,
      return_records?: true,
      authorize?: false
    )
    |> Stream.map(fn {:ok, result} ->
      result
    end)
    |> Ash.bulk_update(:error_in_before_action_on_title2, %{title2: "updated value"},
      resource: MnesiaPost,
      strategy: :stream,
      allow_stream_with: :full_read,
      return_records?: true,
      notify?: true,
      return_errors?: true,
      authorize?: false
    )

    # the first mutation didn't happen
    assert MnesiaPost |> Ash.read!() |> Enum.map(& &1.title) |> Enum.sort() == [
             "title1",
             "title2"
           ]
  end

  test "sends notifications with after action hooks in stream strategy in transactions" do
    assert %Ash.BulkResult{records: [%{title2: "updated value"}, %{title2: "updated value"}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], MnesiaPost, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_with_after_action, %{title2: "updated value"},
               resource: MnesiaPost,
               strategy: :stream,
               allow_stream_with: :full_read,
               return_records?: true,
               notify?: true,
               return_errors?: true,
               authorize?: false
             )

    assert_received {:notification,
                     %{
                       data: %{title: "title1"},
                       changeset: %{attributes: %{title2: "updated value"}}
                     }}

    assert_received {:notification,
                     %{
                       data: %{title: "title2"},
                       changeset: %{attributes: %{title2: "updated value"}}
                     }}

    assert_received {:notification, %{changeset: %{action: %{name: :other_update}}}}
  end

  test "sends notifications with after action hooks with regular updates" do
    Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], MnesiaPost, :create,
      return_stream?: true,
      return_records?: true,
      authorize?: false
    )
    |> Enum.each(fn {:ok, result} ->
      result
      |> Ash.Changeset.for_update(:update_with_after_action, %{title2: "updated value"})
      |> Ash.update!()
    end)

    assert_received {:notification,
                     %{
                       data: %{title: "title1"},
                       changeset: %{attributes: %{title2: "updated value"}}
                     }}

    assert_received {:notification,
                     %{
                       data: %{title: "title2"},
                       changeset: %{attributes: %{title2: "updated value"}}
                     }}

    assert_received {:notification, %{changeset: %{action: %{name: :other_update}}}}
  end

  test "notifications can be returned" do
    assert %Ash.BulkResult{
             records: [%{title2: "updated value"}, %{title2: "updated value"}],
             notifications: [
               %{data: %{title2: "updated value"}},
               %{data: %{title2: "updated value"}}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update, %{title2: "updated value"},
               resource: Post,
               strategy: :atomic_batches,
               return_records?: true,
               return_notifications?: true,
               return_errors?: true,
               authorize?: false
             )
  end

  test "runs changes" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff", title2: "updated value"},
               %{title: "title2_stuff", title2: "updated value"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_with_change, %{title2: "updated value"},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "manual updates are supported" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1"},
               %{title: "title2"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_manual, %{},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "accepts arguments" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "updated value"},
               %{title: "title2", title2: "updated value"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_with_argument, %{a_title: "updated value"},
               resource: Post,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "runs after batch hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "before_title1_after", title2: "updated value"},
               %{title: "before_title2_after", title2: "updated value"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_with_after_batch, %{title2: "updated value"},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "runs after batch hooks with legacy data layers (no refs)" do
    Application.put_env(:ash, :test_bulk_index_only, true)

    try do
      assert %Ash.BulkResult{
               records: [
                 %{title: "before_title1_after", title2: "updated value"},
                 %{title: "before_title2_after", title2: "updated value"}
               ]
             } =
               Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Enum.to_list()
               |> Ash.bulk_update!(:update_with_after_batch, %{title2: "updated value"},
                 resource: Post,
                 strategy: :stream,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false
               )
               |> Map.update!(:records, fn records ->
                 Enum.sort_by(records, & &1.title)
               end)
    after
      Application.delete_env(:ash, :test_bulk_index_only)
    end
  end

  test "runs changes in batches" do
    create_records = fn count ->
      Stream.iterate(1, &(&1 + 1))
      |> Stream.map(fn i -> %{title: "title#{i}"} end)
      |> Stream.take(count)
      |> Ash.bulk_create!(Post, :create, return_stream?: true, return_records?: true)
      |> Stream.map(fn {:ok, result} -> result end)
    end

    update_records = fn records, opts ->
      opts = [resource: Post, return_records?: true, strategy: :stream] ++ opts
      Ash.bulk_update!(records, :update_with_batch_sizes, %{}, opts)
    end

    batch_size_frequencies = fn %Ash.BulkResult{records: records} ->
      records
      |> Enum.map(&Map.take(&1, [:before_batch_size, :after_batch_size, :change_batch_size]))
      |> Enum.frequencies()
    end

    assert create_records.(101)
           |> update_records.([])
           |> batch_size_frequencies.() == %{
             %{change_batch_size: 100, before_batch_size: 100, after_batch_size: 100} => 100,
             %{change_batch_size: 1, before_batch_size: 1, after_batch_size: 1} => 1
           }

    assert create_records.(10)
           |> update_records.(batch_size: 3)
           |> batch_size_frequencies.() == %{
             %{change_batch_size: 3, before_batch_size: 3, after_batch_size: 3} => 9,
             %{change_batch_size: 1, before_batch_size: 1, after_batch_size: 1} => 1
           }
  end

  test "will return error count" do
    assert %Ash.BulkResult{
             error_count: 2
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update(:update, %{title2: %{invalid: :value}},
               resource: Post,
               strategy: :stream,
               stop_on_error?: false,
               return_records?: true,
               authorize?: false
             )
  end

  test "will return errors on request" do
    assert %Ash.BulkResult{
             error_count: 1,
             errors: [%Ash.Error.Invalid{}]
           } =
             Ash.bulk_create!([%{title: "title1"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update(:update, %{title2: %{invalid: :value}},
               strategy: :stream,
               resource: Post,
               return_errors?: true,
               authorize?: false
             )
  end

  test "runs match validation with atomic_batches strategy" do
    assert %Ash.BulkResult{
             error_count: 1,
             errors: [
               %{errors: [%{field: :title4, message: "Title must be lowercase"}]}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update(:update_with_match, %{title4: "INVALID"},
               strategy: :atomic,
               resource: Post,
               return_errors?: true,
               authorize?: false
             )
  end

  test "runs before transaction hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "before_transaction_title1", title2: "updated value"},
               %{title: "before_transaction_title2", title2: "updated value"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_with_before_transaction, %{title2: "updated value"},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "runs after action hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff", title2: "updated value"},
               %{title: "title2_stuff", title2: "updated value"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_with_after_action, %{title2: "updated value"},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "runs after transaction hooks on success" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff", title2: "updated value"},
               %{title: "title2_stuff", title2: "updated value"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_with_after_transaction, %{title2: "updated value"},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "runs after transaction hooks on failure" do
    assert %Ash.BulkResult{error_count: 2} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update(:update_with_after_transaction, %{title2: 3},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true,
               stop_on_error?: false,
               authorize?: false
             )

    assert_receive {:error, _}
    assert_receive {:error, _}
  end

  test "works with empty list without the need to define a domain" do
    assert %Ash.BulkResult{records: []} =
             Ash.bulk_update!([], :update, %{title2: 3}, return_records?: true)
  end

  test "handles numeric constraints correctly" do
    assert %Ash.BulkResult{
             records: [%{score: 3}, %{score: 3}]
           } =
             Ash.bulk_create!(
               [%{title: "title1", score: 1}, %{title: "title2", score: 2}],
               Post,
               :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update, %{score: 3},
               resource: Post,
               strategy: :atomic,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
  end

  describe "authorization" do
    test "policy success results in successes" do
      assert %Ash.BulkResult{records: [_, _], errors: []} =
               Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_update(
                 :update_with_policy,
                 %{title2: "updated value", authorize?: true},
                 authorize?: true,
                 actor: %{foo: :bar},
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )
    end

    test "policy success results in successes with user-supplied context" do
      Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
        return_stream?: true,
        return_records?: true,
        authorize?: false
      )
      |> Stream.run()

      assert %Ash.BulkResult{records: [_, _], errors: []} =
               Ash.bulk_update!(
                 Post |> Ash.Query.for_read(:query_with_after_action, %{}),
                 :update_with_policy_user_sets_context,
                 %{title2: "updated value"},
                 authorize?: true,
                 strategy: :stream,
                 actor: %{foo: :bar},
                 context: %{authorize?: true},
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )
    end

    test "field authorization is run" do
      assert %Ash.BulkResult{
               records: [
                 %{
                   hidden_attribute: %Ash.ForbiddenField{},
                   hidden_calc: %Ash.ForbiddenField{}
                 },
                 %{
                   hidden_attribute: %Ash.ForbiddenField{},
                   hidden_calc: %Ash.ForbiddenField{}
                 }
               ],
               errors: []
             } =
               Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_update(
                 :update_with_policy,
                 %{title2: "updated value", authorize?: true},
                 authorize?: true,
                 actor: %{foo: :bar},
                 resource: Post,
                 return_records?: true,
                 return_errors?: true,
                 load: [:hidden_calc]
               )
    end

    test "policy failure results in failures" do
      assert %Ash.BulkResult{errors: [%Ash.Error.Forbidden{}], records: []} =
               Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_update(
                 :update_with_policy,
                 %{title2: "updated value", authorize?: false},
                 strategy: :atomic,
                 authorize?: true,
                 stop_on_error?: false,
                 authorize_with: :error,
                 actor: %{foo: :bar},
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )

      assert %Ash.BulkResult{
               errors: [%Ash.Error.Forbidden{}, %Ash.Error.Forbidden{}],
               records: []
             } =
               Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_update(
                 :update_with_policy,
                 %{title2: "updated value", authorize?: false},
                 strategy: :stream,
                 actor: %{foo: :bar},
                 authorize?: true,
                 stop_on_error?: false,
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )
    end

    test "policy failure results in failures when using code interface" do
      Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
        return_stream?: true,
        return_records?: true
      )
      |> Enum.each(fn {:ok, post} ->
        assert_raise Ash.Error.Forbidden, fn ->
          Post.update_with_policy_without_requiring_actor!(post.id, %{
            title2: "updated value",
            authorize?: false
          })
        end
      end)
    end

    test "respects filter when using atomic" do
      assert %Ash.BulkResult{
               records: [
                 %{title: "foo", title2: "updated value"}
               ]
             } =
               Ash.bulk_create!([%{title: "foo"}, %{title: "bar"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_update!(:update_with_filter, %{title2: "updated value"},
                 resource: Post,
                 strategy: :atomic_batches,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false
               )
    end
  end

  test "respects filter when using stream" do
    assert %Ash.BulkResult{
             records: [
               %{title: "foo", title2: "updated value"}
             ]
           } =
             Ash.bulk_create!([%{title: "foo"}, %{title: "bar"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(:update_with_filter, %{title2: "updated value"},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
  end

  describe "load" do
    test "allows loading has_many relationship" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Name"})
        |> Ash.create!()

      for n <- [2, 1] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post #{n}"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()
      end

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      assert %Ash.BulkResult{records: [author]} =
               Ash.bulk_update!([author], :update, %{name: "Updated Name"},
                 resource: Author,
                 strategy: :atomic_batches,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 load: [posts: load_query]
               )

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading paginated has_many relationship" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Name"})
        |> Ash.create!()

      for n <- [2, 1] do
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Post #{n}"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Ash.create!()
      end

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      assert %Ash.BulkResult{records: [author]} =
               Ash.bulk_update!([author], :update, %{name: "Updated Name 1"},
                 resource: Author,
                 strategy: :atomic_batches,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 load: [posts: offset_pagination_query]
               )

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = author.posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      assert %Ash.BulkResult{records: [author]} =
               Ash.bulk_update!([author], :update, %{name: "Updated Name 2"},
                 resource: Author,
                 strategy: :atomic_batches,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 load: [posts: keyset_pagination_query]
               )

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = author.posts
    end

    test "allows loading many_to_many relationship" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      assert %Ash.BulkResult{records: [post]} =
               Ash.bulk_update!([post], :update, %{title: "Updated Title"},
                 resource: Post,
                 strategy: :atomic_batches,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 load: [related_posts: load_query]
               )

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end

    test "allows loading paginated many_to_many relationship" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      assert %Ash.BulkResult{records: [post]} =
               Ash.bulk_update!([post], :update, %{title: "Updated Title 1"},
                 resource: Post,
                 strategy: :atomic_batches,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 load: [related_posts: offset_pagination_query]
               )

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = post.related_posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      assert %Ash.BulkResult{records: [post]} =
               Ash.bulk_update!([post], :update, %{title: "Updated Title 2"},
                 resource: Post,
                 strategy: :atomic_batches,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 load: [related_posts: keyset_pagination_query]
               )

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = post.related_posts
    end

    test "allows loading paginated many_to_many relationship for multitenant resources" do
      tenant = Ash.create!(Tenant, %{})
      tag = Ash.create!(MultitenantTag, %{name: "tag"}, tenant: tenant)
      _ = Ash.create!(MultitenantTag, %{name: "existing"}, tenant: tenant)

      offset_pagination_query =
        MultitenantTag
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.select([:name])
        |> Ash.Query.page(count: true, limit: 1)

      assert %Ash.BulkResult{records: [tag]} =
               Ash.bulk_update!([tag], :add_related_tags, %{related_tags: ["existing"]},
                 resource: MultitenantTag,
                 strategy: :stream,
                 tenant: tenant,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 load: [related_tags: offset_pagination_query]
               )

      assert %Ash.Page.Keyset{
               results: [%MultitenantTag{name: "existing", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 1,
               more?: false
             } = tag.related_tags

      keyset_pagination_query =
        MultitenantTag
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.select([:name])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      assert %Ash.BulkResult{records: [tag]} =
               Ash.bulk_update!([tag], :add_related_tags, %{related_tags: ["new"]},
                 resource: MultitenantTag,
                 tenant: tenant,
                 strategy: :stream,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 load: [related_tags: keyset_pagination_query]
               )

      assert %Ash.Page.Keyset{
               results: [%MultitenantTag{name: "new"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = tag.related_tags
    end
  end

  describe "embedded attribute update" do
    test "allows partially updating an embedded attribute" do
      address = %{
        line1: "123 Memory Lane",
        city: "Springfield",
        state: "NY",
        zip_code: "06101",
        country: "US"
      }

      contact = %{
        email: "ashley@example.com",
        address: address
      }

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Name", contact: contact})
        |> Ash.create!()

      assert %Ash.BulkResult{records: [author], error_count: 0, errors: []} =
               Ash.bulk_update!([author], :update, %{contact: %{email: "brooklyn@example.com"}},
                 resource: Author,
                 strategy: :stream,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false
               )

      assert author.contact.email == "brooklyn@example.com"
    end
  end

  describe "nested bulk operations" do
    setup do
      Ash.bulk_create!(
        [%{title: "test1"}, %{title: "test2"}, %{title: "test3"}],
        MnesiaPost,
        :create,
        return_stream?: true,
        return_records?: true,
        authorize?: false
      )
      |> Enum.map(fn {:ok, result} -> result end)

      :ok
    end

    test "supports bulk updates in after_action callbacks" do
      assert %Ash.BulkResult{} =
               MnesiaPost
               |> Ash.Query.filter(expr(title == "test1"))
               |> Ash.bulk_update!(:update_with_nested_bulk_update, %{title2: "trigger_nested"},
                 strategy: :stream,
                 notify?: true,
                 return_records?: false,
                 authorize?: false
               )
    end

    test "supports nested operations with atomic strategy" do
      assert %Ash.BulkResult{} =
               MnesiaPost
               |> Ash.Query.filter(expr(title == "test1"))
               |> Ash.bulk_update!(:update_with_nested_bulk_update, %{title2: "trigger_nested"},
                 strategy: :atomic_batches,
                 notify?: true,
                 return_records?: false,
                 authorize?: false
               )
    end

    test "maintains notification isolation between operations" do
      assert %Ash.BulkResult{notifications: notifications} =
               MnesiaPost
               |> Ash.Query.filter(expr(title == "test1"))
               |> Ash.bulk_update!(:update_with_nested_bulk_update, %{title2: "trigger_nested"},
                 strategy: :stream,
                 notify?: true,
                 return_notifications?: true,
                 return_records?: false,
                 authorize?: false
               )

      assert is_list(notifications)
      refute Enum.empty?(notifications)
    end
  end
end
