# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkCreateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr
  import ExUnit.CaptureLog

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  defmodule AddAfterToTitle do
    use Ash.Resource.Change

    @impl Ash.Resource.Change
    def batch_change(changesets, _, _) do
      changesets
    end

    @impl Ash.Resource.Change
    def before_batch(changesets, _, _) do
      changesets
    end

    @impl Ash.Resource.Change
    def after_batch(results, _, _) do
      Stream.map(results, fn {_changeset, result} ->
        {:ok, %{result | title: result.title <> "_after"}}
      end)
    end
  end

  defmodule AddBeforeToTitle do
    use Ash.Resource.Change

    @impl Ash.Resource.Change
    def batch_change(changesets, _, _) do
      changesets
    end

    @impl Ash.Resource.Change
    def before_batch(changesets, _, _) do
      changesets
      |> Stream.map(fn changeset ->
        title = Ash.Changeset.get_attribute(changeset, :title)
        Ash.Changeset.force_change_attribute(changeset, :title, "before_" <> title)
      end)
    end

    @impl Ash.Resource.Change
    def after_batch(_, _, _) do
      :ok
    end
  end

  defmodule ChangeTitleBeforeAction do
    use Ash.Resource.Change

    @impl Ash.Resource.Change
    def change(changeset, _opts, _context) do
      Ash.Changeset.before_action(changeset, fn changeset ->
        Ash.Changeset.force_change_attribute(
          changeset,
          :title,
          "before_" <> Ash.Changeset.get_attribute(changeset, :title)
        )
      end)
    end
  end

  defmodule ChangeMessage do
    use Ash.Resource.Change

    @impl Ash.Resource.Change
    def change(changeset, opts, context) do
      send(self(), {:change, changeset, opts, context})
      changeset
    end

    @impl Ash.Resource.Change
    def batch_change(changesets, opts, context) do
      send(self(), {:batch_change, changesets, opts, context})
      changesets
    end

    @impl Ash.Resource.Change
    def after_batch(changesets_and_results, opts, context) do
      send(self(), {:after_batch, changesets_and_results, opts, context})
      :ok
    end
  end

  defmodule Org do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

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

      create :create_with_posts do
        argument :post_ids, {:array, :uuid} do
          allow_nil? false
          constraints min_length: 1
        end

        change manage_relationship(:post_ids, :posts, type: :append)
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      has_many :posts, Ash.Test.Actions.BulkCreateTest.Post,
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
      belongs_to :source_post, Ash.Test.Actions.BulkCreateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_post, Ash.Test.Actions.BulkCreateTest.Post,
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

    alias Ash.Test.Actions.BulkCreateTest.Org

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :org_id
      global? true
    end

    calculations do
      calculate :hidden_calc, :string, expr("something") do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      create :create_with_before_action do
        accept [:title]
        change ChangeTitleBeforeAction
      end

      create :create_with_actor_referencing_upsert_condition do
        upsert? true
        upsert_condition expr(upsert_conflict(:title) != ^actor(:title))
      end

      create :create_with_related_posts do
        argument :related_post_ids, {:array, :uuid} do
          allow_nil? false
          constraints min_length: 1
        end

        change manage_relationship(:related_post_ids, :related_posts, type: :append)
      end

      create :create_with_change do
        change fn changeset, _ ->
                 title = Ash.Changeset.get_attribute(changeset, :title)
                 Ash.Changeset.force_change_attribute(changeset, :title, title <> "_stuff")
               end,
               only_when_valid?: true
      end

      create :create_with_argument do
        argument :a_title, :string do
          allow_nil? false
        end

        change set_attribute(:title, arg(:a_title))
      end

      create :create_with_before_transaction do
        change before_transaction(fn changeset, _context ->
                 title = Ash.Changeset.get_attribute(changeset, :title)

                 Ash.Changeset.force_change_attribute(
                   changeset,
                   :title,
                   "before_transaction_" <> title
                 )
               end)
      end

      create :create_with_after_action do
        change after_action(fn _changeset, result, _context ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      create :create_with_after_batch do
        change {AddAfterToTitle, thing: context(:thing)}
        change AddBeforeToTitle
      end

      create :create_with_after_transaction do
        change after_transaction(fn
                 _changeset, {:ok, result}, _context ->
                   {:ok, %{result | title: result.title <> "_stuff"}}

                 _changeset, {:error, error}, _context ->
                   send(self(), {:error, error})
                   {:error, error}
               end)
      end

      create :create_with_policy do
        argument :authorize?, :boolean, allow_nil?: false

        change set_context(%{authorize?: arg(:authorize?)})
      end

      create :create_with_atomic_only_validation do
        validate AtomicOnlyValidation
      end

      create :create_message do
        change ChangeMessage
      end

      create :create_with_nested_bulk_update do
        change after_action(fn changeset, result, context ->
                 other_posts = Post |> Ash.read!(tenant: changeset.tenant, authorize?: false)

                 other_posts
                 |> Enum.reject(fn post -> post.id == result.id end)
                 |> Ash.bulk_update!(:update, %{title2: "nested_update"},
                   notify?: true,
                   strategy: :stream,
                   return_records?: false,
                   tenant: changeset.tenant,
                   authorize?: false
                 )

                 {:ok, result}
               end)
      end

      create :create_with_nested_bulk_create do
        change after_action(fn changeset, result, context ->
                 Ash.bulk_create!([%{title: "nested_post"}], Post, :create,
                   notify?: true,
                   return_records?: false,
                   tenant: changeset.tenant,
                   authorize?: false
                 )

                 {:ok, result}
               end)
      end
    end

    identities do
      identity :unique_title, :title do
        pre_check_with Ash.Test.Actions.BulkCreateTest.Domain
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

    policies do
      policy action(:create_with_policy) do
        authorize_if context_equals(:authorize?, true)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :title2, :string, public?: true
      attribute :title3, :string, public?: true
      attribute :hidden_attribute, :string, public?: true

      timestamps()
    end

    relationships do
      belongs_to :org, Org do
        public?(true)
        allow_nil? false
        attribute_public? false
      end

      belongs_to :author, Author, public?: true

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id,
        public?: true
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

      belongs_to :source_tag, Ash.Test.Actions.BulkCreateTest.MultitenantTag,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_tag, Ash.Test.Actions.BulkCreateTest.MultitenantTag,
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

      create :create_with_related_tags do
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

  defmodule Product do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept [:gtin, :title]
      defaults [:read, :update, :destroy]

      create :create do
        primary? true

        argument :price, :map
        change manage_relationship(:price, type: :create)

        upsert? true
        upsert_fields {:replace_all_except, [:gtin, :id, :inserted_at]}
        upsert_identity :unique_gtin
        upsert_condition expr(false)
      end
    end

    attributes do
      uuid_v7_primary_key :id

      attribute :gtin, :string do
        allow_nil? false
        public? true
      end

      attribute :title, :string, public?: true

      timestamps()
    end

    relationships do
      has_one :price, Ash.Test.Actions.BulkCreateTest.Price, public?: true
    end

    identities do
      identity :unique_gtin, :gtin do
        pre_check_with Ash.Test.Actions.BulkCreateTest.Domain
      end
    end
  end

  defmodule Price do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept [:max, :min, :rrp]

      defaults [:read, :update, :destroy]

      create :create do
        primary? true
        upsert? true
        upsert_fields {:replace_all_except, [:inserted_at]}
      end
    end

    attributes do
      attribute :max, :integer, public?: true
      attribute :min, :integer, public?: true
      attribute :rrp, :integer, public?: true
      timestamps()
    end

    relationships do
      belongs_to :product, Product do
        allow_nil? false
        primary_key? true
        public? false
      end
    end
  end

  test "returns created records" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{records: [%{title: "title1"}, %{title: "title2"}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_records?: true,
               return_errors?: true,
               authorize?: false,
               sorted?: true,
               tenant: org.id
             )
  end

  test "runs before action hooks" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{records: [%{title: "before_title1"}, %{title: "before_title2"}]} =
             Ash.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_before_action,
               return_records?: true,
               return_errors?: true,
               authorize?: false,
               sorted?: true,
               tenant: org.id
             )
  end

  test "runs changes" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff", __metadata__: %{tenant: tenant}},
               %{title: "title2_stuff", __metadata__: %{tenant: tenant}}
             ]
           } =
             Ash.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_change,
               tenant: org.id,
               return_records?: true,
               authorize?: false,
               sorted?: true
             )

    assert tenant == org.id
  end

  test "accepts arguments" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{records: [%{title: "title1"}, %{title: "title2"}]} =
             Ash.bulk_create!(
               [%{a_title: "title1"}, %{a_title: "title2"}],
               Post,
               :create_with_argument,
               return_records?: true,
               tenant: org.id,
               sorted?: true,
               authorize?: false
             )
  end

  test "runs after batch hooks" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [%{title: "before_title1_after"}, %{title: "before_title2_after"}]
           } =
             Ash.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_after_batch,
               tenant: org.id,
               return_records?: true,
               sorted?: true,
               authorize?: false
             )

    assert %{title: "before_title_after"} =
             Ash.create!(
               Post,
               %{title: "title"},
               action: :create_with_after_batch,
               tenant: org.id,
               authorize?: false
             )
  end

  test "runs after batch hooks with legacy data layers (no refs)" do
    Application.put_env(:ash, :test_bulk_index_only, true)

    try do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert %Ash.BulkResult{
               records: [%{title: "before_title1_after"}, %{title: "before_title2_after"}]
             } =
               Ash.bulk_create!(
                 [%{title: "title1"}, %{title: "title2"}],
                 Post,
                 :create_with_after_batch,
                 tenant: org.id,
                 return_records?: true,
                 sorted?: true,
                 authorize?: false
               )

      assert %{title: "before_title_after"} =
               Ash.create!(
                 Post,
                 %{title: "title"},
                 action: :create_with_after_batch,
                 tenant: org.id,
                 authorize?: false
               )
    after
      Application.delete_env(:ash, :test_bulk_index_only)
    end
  end

  test "will return error count" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{records: [%{title: "title1_stuff"}], error_count: 1, errors: nil} =
             Ash.bulk_create(
               [%{title: "title1"}, %{title: %{foo: :bar}}],
               Post,
               :create_with_change,
               tenant: org.id,
               return_records?: true,
               stop_on_error?: false,
               return_errors?: false,
               sorted?: true,
               authorize?: false
             )
  end

  test "will return errors on request" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [%{title: "title1_stuff"}],
             error_count: 1,
             errors: [%Ash.Error.Invalid{}]
           } =
             Ash.bulk_create(
               [%{title: "title1"}, %{title: %{foo: :bar}}],
               Post,
               :create_with_change,
               tenant: org.id,
               return_records?: true,
               stop_on_error?: false,
               return_errors?: true,
               sorted?: true,
               authorize?: false
             )
  end

  test "supplies arguments to after_batch" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{status: :success} =
             Ash.bulk_create!(
               [%{title: "title1"}],
               Post,
               :create_message,
               tenant: org.id,
               return_records?: false,
               return_errors?: true,
               authorize?: false
             )

    assert_received {:after_batch, [{%Ash.Changeset{}, %Post{}}], _opts, _context}
  end

  test "properly sets the status to `:partial_success` without `return_records?`" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             error_count: 1,
             status: :partial_success,
             errors: [%Ash.Error.Invalid{}]
           } =
             Ash.bulk_create(
               [%{title: "title1"}, %{title: %{foo: :bar}}],
               Post,
               :create_with_change,
               tenant: org.id,
               return_errors?: true,
               stop_on_error?: false,
               sorted?: true,
               authorize?: false
             )
  end

  test "can upsert with list" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "changes", title3: "wont"},
               %{title: "title2", title2: "changes", title3: "wont"},
               %{title: "title3", title2: "changes", title3: "wont"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "changes", title3: "wont"},
                 %{title: "title2", title2: "changes", title3: "wont"},
                 %{title: "title3", title2: "changes", title3: "wont"}
               ],
               Post,
               :create,
               tenant: org.id,
               return_errors?: true,
               return_records?: true,
               sorted?: true,
               return_errors?: true,
               authorize?: false
             )

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "did_change", title3: "wont"},
               %{title: "title2", title2: "did_change", title3: "wont"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "did_change", title3: "oh no"},
                 %{title: "title2", title2: "did_change", title3: "what happened"},
                 %{title: "title3", title2: "shouldn't even", title3: "be in result"}
               ],
               Post,
               :create,
               return_errors?: true,
               return_records?: true,
               tenant: org.id,
               upsert?: true,
               return_errors?: true,
               upsert_identity: :unique_title,
               upsert_fields: [:title2],
               upsert_condition: expr(upsert_conflict(:title) != "title3"),
               sorted?: true,
               authorize?: false
             )
  end

  test "can upsert with an actor reference in the upsert condition" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "changes", title3: "wont"},
               %{title: "title2", title2: "changes", title3: "wont"},
               %{title: "title3", title2: "changes", title3: "wont"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "changes", title3: "wont"},
                 %{title: "title2", title2: "changes", title3: "wont"},
                 %{title: "title3", title2: "changes", title3: "wont"}
               ],
               Post,
               :create,
               tenant: org.id,
               return_errors?: true,
               return_records?: true,
               sorted?: true,
               return_errors?: true,
               authorize?: false
             )

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "did_change", title3: "wont"},
               %{title: "title2", title2: "did_change", title3: "wont"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "did_change", title3: "oh no"},
                 %{title: "title2", title2: "did_change", title3: "what happened"},
                 %{title: "title3", title2: "shouldn't even", title3: "be in result"}
               ],
               Post,
               :create_with_actor_referencing_upsert_condition,
               return_errors?: true,
               return_records?: true,
               tenant: org.id,
               upsert?: true,
               return_errors?: true,
               upsert_identity: :unique_title,
               upsert_fields: [:title2],
               actor: %{title: "title3"},
               upsert_condition: expr(upsert_conflict(:title) != ^actor(:title)),
               sorted?: true,
               authorize?: false
             )
  end

  test "respects upsert_condition despite we have a relationship" do
    product =
      Ash.bulk_create(
        [
          %{
            gtin: "1234567891011",
            title: "Nano Bubble Gum",
            price: %{
              min: 123,
              max: 456,
              rrp: 789
            }
          }
        ],
        Product,
        :create,
        return_errors?: true,
        return_records?: true
      )
      |> then(fn result -> List.first(result.records) end)

    assert product.gtin == "1234567891011"
    assert product.title == "Nano Bubble Gum"
    assert product.price.min == 123
    assert product.price.max == 456
    assert product.price.rrp == 789

    result =
      Ash.bulk_create(
        [
          %{
            gtin: "1234567891011",
            title: "Nano Bubble Gum",
            price: %{
              # Note: we try to change the price here.
              min: 234,
              max: 567,
              rrp: 890
            }
          }
        ],
        Product,
        :create,
        return_errors?: true,
        return_records?: true,
        upsert?: true,
        # But the upsert_condition says "no!".
        upsert_condition: expr(false)
      )

    assert result.records == []
  end

  test "returns skipped upserts when upsert_condition prevents update and return_skipped_upserts? is true" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{records: [%{title: "title1", title2: "initial"}]} =
             Ash.bulk_create!(
               [%{title: "title1", title2: "initial"}],
               Post,
               :create,
               tenant: org.id,
               return_records?: true,
               authorize?: false
             )

    result =
      Ash.bulk_create(
        [%{title: "title1", title2: "attempted_change"}],
        Post,
        :create,
        tenant: org.id,
        return_records?: true,
        upsert?: true,
        upsert_identity: :unique_title,
        upsert_fields: [:title2],
        upsert_condition: expr(false),
        return_skipped_upsert?: true,
        authorize?: false
      )

    assert %Ash.BulkResult{records: [returned_record]} = result
    assert returned_record.title == "title1"
    # The title2 should still be "initial" since the upsert was skipped
    assert returned_record.title2 == "initial"
  end

  test "can upsert with :replace" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "changes", title3: "wont"},
               %{title: "title2", title2: "changes", title3: "wont"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "changes", title3: "wont"},
                 %{title: "title2", title2: "changes", title3: "wont"}
               ],
               Post,
               :create,
               tenant: org.id,
               return_records?: true,
               sorted?: true,
               authorize?: false
             )

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "did_change", title3: "wont"},
               %{title: "title2", title2: "did_change", title3: "wont"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "did_change", title3: "oh no"},
                 %{title: "title2", title2: "did_change", title3: "what happened"}
               ],
               Post,
               :create,
               return_records?: true,
               upsert?: true,
               tenant: org.id,
               upsert_identity: :unique_title,
               upsert_fields: {:replace, [:title2]},
               sorted?: true,
               authorize?: false
             )
  end

  test "can upsert with :replace_all" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "changes", title3: "changes"},
               %{title: "title2", title2: "changes", title3: "changes"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "changes", title3: "changes"},
                 %{title: "title2", title2: "changes", title3: "changes"}
               ],
               Post,
               :create,
               return_records?: true,
               tenant: org.id,
               sorted?: true,
               authorize?: false
             )

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "did_change", title3: "did_change"},
               %{title: "title2", title2: "did_change", title3: "did_change"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "did_change", title3: "did_change"},
                 %{title: "title2", title2: "did_change", title3: "did_change"}
               ],
               Post,
               :create,
               return_records?: true,
               tenant: org.id,
               upsert?: true,
               upsert_identity: :unique_title,
               upsert_fields: :replace_all,
               sorted?: true,
               authorize?: false
             )
  end

  test "can upsert with :replace_all_except" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "changes", title3: "wont"},
               %{title: "title2", title2: "changes", title3: "wont"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "changes", title3: "wont"},
                 %{title: "title2", title2: "changes", title3: "wont"}
               ],
               Post,
               :create,
               tenant: org.id,
               return_records?: true,
               sorted?: true,
               authorize?: false
             )

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "did_change", title3: "wont"},
               %{title: "title2", title2: "did_change", title3: "wont"}
             ]
           } =
             Ash.bulk_create!(
               [
                 %{title: "title1", title2: "did_change", title3: "oh no"},
                 %{title: "title2", title2: "did_change", title3: "what happened"}
               ],
               Post,
               :create,
               return_records?: true,
               tenant: org.id,
               upsert?: true,
               upsert_identity: :unique_title,
               upsert_fields: {:replace_all_except, [:title, :title3]},
               sorted?: true,
               authorize?: false
             )
  end

  test "runs before transaction hooks" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [
               %{title: "before_transaction_title1"},
               %{title: "before_transaction_title2"}
             ]
           } =
             Ash.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_before_transaction,
               tenant: org.id,
               return_records?: true,
               sorted?: true,
               authorize?: false
             )
  end

  test "runs after action hooks" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{records: [%{title: "title1_stuff"}, %{title: "title2_stuff"}]} =
             Ash.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_after_action,
               tenant: org.id,
               return_records?: true,
               sorted?: true,
               authorize?: false
             )
  end

  test "runs after transaction hooks on success" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [%{title: "title1_stuff"}, %{title: "title2_stuff"}]
           } =
             Ash.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_after_transaction,
               tenant: org.id,
               return_records?: true,
               return_errors?: true,
               sorted?: true,
               authorize?: false
             )
  end

  test "runs after transaction hooks on failure" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert %Ash.BulkResult{error_count: 2} =
             Ash.bulk_create(
               [%{title: 1}, %{title: 2}],
               Post,
               :create_with_after_transaction,
               sorted?: true,
               stop_on_error?: false,
               authorize?: false,
               tenant: org.id
             )

    assert_receive {:error, _error}
    assert_receive {:error, _error}
  end

  describe "authorization" do
    test "policy success results in successes" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert %Ash.BulkResult{records: [%{title: "title1"}, %{title: "title2"}]} =
               Ash.bulk_create!(
                 [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}],
                 Post,
                 :create_with_policy,
                 tenant: org.id,
                 authorize?: true,
                 return_errors?: true,
                 return_records?: true,
                 sorted?: true
               )
    end

    test "field authorization is run" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert %Ash.BulkResult{
               records: [
                 %{hidden_attribute: %Ash.ForbiddenField{}, hidden_calc: %Ash.ForbiddenField{}},
                 %{hidden_attribute: %Ash.ForbiddenField{}, hidden_calc: %Ash.ForbiddenField{}}
               ]
             } =
               Ash.bulk_create!(
                 [
                   %{title: "title1", authorize?: true},
                   %{title: "title2", authorize?: true}
                 ],
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 tenant: org.id,
                 return_records?: true,
                 sorted?: true,
                 load: [:hidden_calc]
               )
    end

    test "policy failure results in failures" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert %Ash.BulkResult{errors: [%Ash.Error.Forbidden{}, %Ash.Error.Forbidden{}]} =
               Ash.bulk_create(
                 [
                   %{title: "title1", authorize?: false},
                   %{title: "title2", authorize?: false}
                 ],
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 tenant: org.id,
                 return_records?: true,
                 stop_on_error?: false,
                 return_errors?: true,
                 sorted?: true
               )
    end
  end

  describe "streaming" do
    test "a stream is returned when inputs are empty" do
      assert [] =
               []
               |> Ash.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 return_stream?: true,
                 return_records?: true
               )
               |> Enum.to_list()
    end

    test "by default nothing is returned in the stream" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      message =
        capture_log(fn ->
          assert [] =
                   [
                     %{title: "title1", authorize?: true},
                     %{title: "title2", authorize?: true}
                   ]
                   |> Ash.bulk_create!(
                     Post,
                     :create_with_policy,
                     authorize?: true,
                     tenant: org.id,
                     return_stream?: true
                   )
                   |> Enum.to_list()
        end)

      assert message =~
               "Bulk action was called with :return_stream? set to true, but no other :return_*? options were set."

      message =
        capture_log(fn ->
          assert [] =
                   [
                     %{title: "title1", authorize?: true},
                     %{title: "title2", authorize?: true}
                   ]
                   |> Ash.bulk_create!(
                     Post,
                     :create_with_policy,
                     authorize?: true,
                     tenant: org.id,
                     return_stream?: true,
                     return_nothing?: true
                   )
                   |> Enum.to_list()
        end)

      assert message == ""
    end

    test "batch size is honored while streaming" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert [_] =
               [
                 %{title: "title1", authorize?: true},
                 %{title: "title2", authorize?: true},
                 %{title: "title3", authorize?: true}
               ]
               |> Ash.bulk_create!(
                 Post,
                 :create_with_policy,
                 tenant: org.id,
                 authorize?: true,
                 batch_size: 2,
                 return_records?: true,
                 return_stream?: true
               )
               |> Enum.take(1)

      assert Ash.count!(Post, authorize?: false) == 2
    end

    test "by returning notifications, you get the notifications in the stream" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert [{:notification, _}, {:notification, _}] =
               [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}]
               |> Ash.bulk_create!(
                 Post,
                 :create_with_policy,
                 tenant: org.id,
                 authorize?: true,
                 return_stream?: true,
                 return_notifications?: true
               )
               |> Enum.to_list()
    end

    test "notifications are sent with notify?: true" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert [{:ok, %{title: "title1"}}, {:ok, %{title: "title2"}}] =
               [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}]
               |> Ash.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 tenant: org.id,
                 notify?: true,
                 return_stream?: true,
                 return_records?: true
               )
               |> Enum.to_list()
               |> Enum.sort_by(fn
                 {:ok, v} ->
                   v.title

                 _ ->
                   nil
               end)

      assert_received {:notification, %{data: %{title: "title1"}}}
      assert_received {:notification, %{data: %{title: "title2"}}}
    end

    test "by returning records, you get the records in the stream" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert [{:ok, %{title: "title1"}}, {:ok, %{title: "title2"}}] =
               [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}]
               |> Ash.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 tenant: org.id,
                 return_stream?: true,
                 return_records?: true
               )
               |> Enum.to_list()
               |> Enum.sort_by(fn
                 {:ok, v} ->
                   v.title

                 _ ->
                   nil
               end)
    end

    test "by returning notifications and records, you get them both in the stream" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert [
               {:notification, _},
               {:notification, _},
               {:ok, %{title: "title1"}},
               {:ok, %{title: "title2"}}
             ] =
               [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}]
               |> Ash.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 tenant: org.id,
                 return_stream?: true,
                 return_notifications?: true,
                 return_records?: true
               )
               |> Enum.to_list()
               |> Enum.sort_by(fn
                 {:ok, v} ->
                   v.title

                 {:notification, _} ->
                   true

                 _ ->
                   nil
               end)
    end

    test "any errors are also returned in the stream" do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert [
               {:error, %Ash.Error.Forbidden{}},
               {:notification, _},
               {:ok, %{title: "title1"}}
             ] =
               [
                 %{title: "title1", authorize?: true},
                 %{title: "title2", authorize?: false}
               ]
               |> Ash.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 tenant: org.id,
                 return_stream?: true,
                 return_notifications?: true,
                 return_records?: true,
                 return_errors?: true
               )
               |> Enum.to_list()
               |> Enum.sort_by(fn
                 {:ok, v} ->
                   v.title

                 {:notification, _} ->
                   true

                 {:error, _} ->
                   false

                 _ ->
                   nil
               end)
    end
  end

  describe "load" do
    test "allows loading has_many relationship" do
      org = Ash.create!(Org, %{})
      post1 = Ash.create!(Post, %{title: "Post 1"}, tenant: org.id, authorize?: false)
      post2 = Ash.create!(Post, %{title: "Post 2"}, tenant: org.id, authorize?: false)

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      assert %Ash.BulkResult{records: [author]} =
               Ash.bulk_create!(
                 [%{name: "Author", post_ids: [post2.id, post1.id]}],
                 Author,
                 :create_with_posts,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 tenant: org.id,
                 load: [posts: load_query]
               )

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading paginated has_many relationship" do
      org = Ash.create!(Org, %{})
      post1 = Ash.create!(Post, %{title: "Post 1"}, tenant: org.id, authorize?: false)
      post2 = Ash.create!(Post, %{title: "Post 2"}, tenant: org.id, authorize?: false)

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      assert %Ash.BulkResult{records: [author]} =
               Ash.bulk_create!(
                 [%{name: "Author 1", post_ids: [post2.id, post1.id]}],
                 Author,
                 :create_with_posts,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 tenant: org.id,
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
               Ash.bulk_create!(
                 [%{name: "Author 2", post_ids: [post2.id, post1.id]}],
                 Author,
                 :create_with_posts,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 tenant: org.id,
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
      org = Ash.create!(Org, %{})
      related_post1 = Ash.create!(Post, %{title: "Related 1"}, tenant: org.id, authorize?: false)
      related_post2 = Ash.create!(Post, %{title: "Related 2"}, tenant: org.id, authorize?: false)

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      assert %Ash.BulkResult{records: [post]} =
               Ash.bulk_create!(
                 [%{title: "Title", related_post_ids: [related_post2.id, related_post1.id]}],
                 Post,
                 :create_with_related_posts,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 tenant: org.id,
                 load: [related_posts: load_query]
               )

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end

    test "allows loading paginated many_to_many relationship" do
      org = Ash.create!(Org, %{})
      related_post1 = Ash.create!(Post, %{title: "Related 1"}, tenant: org.id, authorize?: false)
      related_post2 = Ash.create!(Post, %{title: "Related 2"}, tenant: org.id, authorize?: false)

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      assert %Ash.BulkResult{records: [post]} =
               Ash.bulk_create!(
                 [%{title: "Post 1", related_post_ids: [related_post2.id, related_post1.id]}],
                 Post,
                 :create_with_related_posts,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 tenant: org.id,
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
               Ash.bulk_create!(
                 [%{title: "Post 2", related_post_ids: [related_post2.id, related_post1.id]}],
                 Post,
                 :create_with_related_posts,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 tenant: org.id,
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
      _ = Ash.create!(MultitenantTag, %{name: "foo"}, tenant: tenant)

      offset_pagination_query =
        MultitenantTag
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.select([:name])
        |> Ash.Query.page(count: true, limit: 1)

      assert %Ash.BulkResult{records: [tag]} =
               Ash.bulk_create!(
                 [%{name: "tag 1", related_tags: ["foo", "bar"]}],
                 MultitenantTag,
                 :create_with_related_tags,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 tenant: tenant,
                 load: [related_tags: offset_pagination_query]
               )

      assert %Ash.Page.Keyset{
               results: [%MultitenantTag{name: "bar", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = tag.related_tags

      keyset_pagination_query =
        MultitenantTag
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.select([:name])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      assert %Ash.BulkResult{records: [tag]} =
               Ash.bulk_create!(
                 [%{name: "tag 2", related_tags: ["foo", "bar"]}],
                 MultitenantTag,
                 :create_with_related_tags,
                 return_records?: true,
                 return_errors?: true,
                 authorize?: false,
                 tenant: tenant,
                 load: [related_tags: keyset_pagination_query]
               )

      assert %Ash.Page.Keyset{
               results: [%MultitenantTag{name: "foo"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = tag.related_tags
    end
  end

  describe "nested bulk operations" do
    setup do
      org =
        Org
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      Ash.bulk_create!(
        [%{title: "setup1"}, %{title: "setup2"}, %{title: "setup3"}],
        Post,
        :create,
        return_stream?: true,
        return_records?: true,
        authorize?: false,
        tenant: org.id
      )
      |> Enum.map(fn {:ok, result} -> result end)

      {:ok, %{org: org}}
    end

    test "supports bulk_update in after_action callbacks", %{org: org} do
      assert %Ash.BulkResult{} =
               Ash.bulk_create!(
                 [%{title: "trigger_nested"}],
                 Post,
                 :create_with_nested_bulk_update,
                 notify?: true,
                 return_records?: false,
                 authorize?: false,
                 tenant: org.id
               )
    end

    test "supports bulk_create in after_action callbacks", %{org: org} do
      assert %Ash.BulkResult{} =
               Ash.bulk_create!(
                 [%{title: "trigger_nested"}],
                 Post,
                 :create_with_nested_bulk_create,
                 notify?: true,
                 return_records?: false,
                 authorize?: false,
                 tenant: org.id
               )
    end
  end

  test "shows an error if an atomic only validation is used in a create" do
    assert_raise Ash.Error.Framework,
                 ~r/Create actions cannot be made atomic/,
                 fn ->
                   [%{title: "title1"}, %{title: "title2"}]
                   |> Ash.bulk_create!(
                     Post,
                     :create_with_atomic_only_validation,
                     authorize?: true,
                     return_errors?: true
                   )
                 end
  end
end
