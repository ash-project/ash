# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkDestroyTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  require Ash.Query
  alias Ash.Test.BulkTestChanges.ConditionalAfterActionErrorWithAfterTransaction
  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  defmodule AddAfterToTitle do
    use Ash.Resource.Change

    def change(changeset, _, _) do
      changeset
    end

    def atomic(_, _, _), do: :ok

    def batch_change(changesets, _, _) do
      changesets
    end

    def after_batch(results, _, _) do
      Stream.map(results, fn {_changeset, result} ->
        {:ok, %{result | title: result.title <> "_after"}}
      end)
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
      has_many :posts, Ash.Test.Actions.BulkDestroyTest.Post,
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
      belongs_to :source_post, Ash.Test.Actions.BulkDestroyTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_post, Ash.Test.Actions.BulkDestroyTest.Post,
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
      notifiers: [Notifier],
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    code_interface do
      define :destroy_with_policy
    end

    validations do
      validate AtomicOnlyValidation,
        on: [:destroy]
    end

    actions do
      default_accept :*
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        pagination keyset?: true, required?: false
      end

      destroy :destroy_with_change do
        require_atomic? false

        change fn changeset, _ ->
          title = Ash.Changeset.get_attribute(changeset, :title)
          Ash.Changeset.force_change_attribute(changeset, :title, title <> "_stuff")
        end
      end

      destroy :destroy_with_validation do
        validate attribute_does_not_equal(:title, "can't delete")
      end

      destroy :destroy_with_argument do
        require_atomic? false

        argument :a_title, :string do
          allow_nil? false
        end
      end

      destroy :destroy_with_after_action do
        require_atomic? false

        change after_action(fn _changeset, result, _context ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      destroy :destroy_with_after_batch do
        require_atomic? false
        change AddAfterToTitle
      end

      destroy :destroy_with_after_transaction do
        require_atomic? false

        argument :a_title, :string

        change after_transaction(fn
                 _changeset, {:ok, result}, _context ->
                   {:ok, %{result | title: result.title <> "_stuff"}}

                 _changeset, {:error, error}, _context ->
                   send(self(), {:error, error})
               end)
      end

      destroy :destroy_with_policy do
        require_atomic? false
        argument :authorize?, :boolean, allow_nil?: false

        change set_context(%{authorize?: arg(:authorize?)})
      end

      destroy :destroy_with_filter do
        change filter(expr(title == "foo"))
      end

      destroy :soft do
        require_atomic? false
        soft? true
        change set_attribute(:title2, "archived")
      end

      destroy :forbidden_destroy do
      end

      destroy :destroy_with_conditional_after_action_error do
        change ConditionalAfterActionErrorWithAfterTransaction
      end

      destroy :destroy_with_nested_bulk_create do
        require_atomic? false

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

      destroy :destroy_with_nested_bulk_update do
        require_atomic? false

        change after_action(fn changeset, result, context ->
                 other_posts = Post |> Ash.read!(tenant: changeset.tenant, authorize?: false)

                 other_posts
                 |> Enum.take(2)
                 |> Ash.bulk_update!(:update, %{title2: "nested_update"},
                   notify?: true,
                   return_records?: false,
                   authorize?: false
                 )

                 {:ok, result}
               end)
      end

      destroy :destroy_with_nested_bulk_destroy do
        require_atomic? false

        change after_action(fn changeset, result, context ->
                 other_posts = Post |> Ash.read!(tenant: changeset.tenant, authorize?: false)

                 other_posts
                 |> Enum.take(1)
                 |> Ash.bulk_destroy!(:destroy, %{},
                   notify?: true,
                   return_records?: false,
                   authorize?: false
                 )

                 {:ok, result}
               end)
      end
    end

    identities do
      identity :unique_title, :title do
        pre_check_with Ash.Test.Actions.BulkUpdateTest.Domain
      end
    end

    policies do
      policy action(:forbidden_destroy) do
        authorize_if never()
      end

      policy action(:destroy_with_policy) do
        authorize_if context_equals(:authorize?, true)
      end

      policy action(:read) do
        authorize_if always()
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
      table :mnesia_post_destroys
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true

      timestamps()
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create, :update]
    end
  end

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

  test "returns destroyed records" do
    assert %Ash.BulkResult{records: [%{}, %{}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy, %{},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true
             )

    assert [] = Ash.read!(Post)
  end

  test "sends notifications" do
    assert %Ash.BulkResult{records: [%{}, %{}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy, %{},
               resource: Post,
               strategy: :stream,
               notify?: true,
               return_records?: true,
               return_errors?: true
             )

    assert_received {:notification, %{data: %{title: "title1"}}}
    assert_received {:notification, %{data: %{title: "title2"}}}
  end

  test "sends notifications with stream strategy in transactions" do
    assert %Ash.BulkResult{records: [%{}, %{}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], MnesiaPost, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy, %{},
               resource: MnesiaPost,
               strategy: :stream,
               allow_stream_with: :full_read,
               return_records?: true,
               notify?: true,
               return_errors?: true
             )

    assert_received {:notification,
                     %{
                       data: %{title: "title1"}
                     }}

    assert_received {:notification,
                     %{
                       data: %{title: "title2"}
                     }}
  end

  test "doesn't send notifications if not asked to" do
    assert %Ash.BulkResult{records: [%{}, %{}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy, %{},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true
             )

    refute_received {:notification, _}
  end

  test "notifications can be returned" do
    assert %Ash.BulkResult{records: [%{}, %{}], notifications: [%{}, %{}]} =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy, %{},
               resource: Post,
               strategy: :stream,
               return_notifications?: true,
               return_records?: true,
               return_errors?: true
             )
  end

  test "runs changes" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff"},
               %{title: "title2_stuff"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy_with_change, %{},
               resource: Post,
               strategy: [:stream],
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Ash.read!(Post)
  end

  test "accepts arguments" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: nil},
               %{title: "title2", title2: nil}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy_with_argument, %{a_title: "a value"},
               resource: Post,
               strategy: [:stream],
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Ash.read!(Post)
  end

  test "runs validations" do
    assert_raise Ash.Error.Invalid, ~r/must not equal "can't delete"/, fn ->
      assert %Ash.BulkResult{
               records: [
                 %{title: "title1", title2: nil},
                 %{title: "title2", title2: nil}
               ]
             } =
               Ash.bulk_create!([%{title: "can't delete"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_destroy!(:destroy_with_validation, %{},
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )
    end
  end

  test "runs after batch hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_after"},
               %{title: "title2_after"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy_with_after_batch, %{},
               resource: Post,
               strategy: [:atomic],
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Ash.read!(Post)
  end

  test "runs after batch hooks with legacy data layers (no refs)" do
    Application.put_env(:ash, :test_bulk_index_only, true)

    try do
      assert %Ash.BulkResult{
               records: [
                 %{title: "title1_after"},
                 %{title: "title2_after"}
               ]
             } =
               Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_destroy!(:destroy_with_after_batch, %{},
                 resource: Post,
                 strategy: [:atomic],
                 return_records?: true,
                 return_errors?: true
               )
               |> Map.update!(:records, fn records ->
                 Enum.sort_by(records, & &1.title)
               end)

      assert [] = Ash.read!(Post)
    after
      Application.delete_env(:ash, :test_bulk_index_only)
    end
  end

  test "will return errors on request" do
    assert %Ash.BulkResult{
             error_count: 1,
             errors: [%Ash.Error.Invalid{}]
           } =
             Ash.bulk_create!([%{title: "title1"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy(:destroy_with_argument, %{a_title: %{invalid: :value}},
               resource: Post,
               strategy: :stream,
               return_errors?: true
             )

    assert [_] = Ash.read!(Post)
  end

  test "runs after action hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff"},
               %{title: "title2_stuff"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy_with_after_action, %{},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Ash.read!(Post)
  end

  test "runs after transaction hooks on success" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff"},
               %{title: "title2_stuff"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy_with_after_transaction, %{},
               strategy: :stream,
               resource: Post,
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Ash.read!(Post)
  end

  test "runs after transaction hooks on failure" do
    assert %Ash.BulkResult{
             error_count: 1,
             errors: [%Ash.Error.Invalid{}]
           } =
             Ash.bulk_create!([%{title: "title1"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy(:destroy_with_after_transaction, %{a_title: %{invalid: :value}},
               resource: Post,
               strategy: :stream,
               return_errors?: true
             )

    assert_receive {:error, _error}
  end

  test "soft destroys" do
    assert %Ash.BulkResult{
             records: [
               %{title2: "archived"},
               %{title2: "archived"}
             ]
           } =
             Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:soft, %{},
               strategy: [:stream],
               resource: Post,
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "works with empty list without the need to define a domain" do
    assert %Ash.BulkResult{records: []} =
             Ash.bulk_destroy!([], :destroy, %{}, return_records?: true)
  end

  describe "authorization" do
    test "policy success results in successes" do
      assert %Ash.BulkResult{records: [_, _], errors: []} =
               Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_destroy(
                 :destroy_with_policy,
                 %{authorize?: true},
                 strategy: [:atomic_batches],
                 authorize?: true,
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )
    end

    test "policy success results in successes with query" do
      Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
        return_records?: true
      )

      assert %Ash.BulkResult{errors: []} =
               Post
               |> Ash.Query.filter(title: [in: ["title1", "title2"]])
               |> Ash.bulk_destroy(
                 :destroy_with_policy,
                 %{authorize?: true},
                 strategy: :stream,
                 authorize?: true,
                 return_errors?: true
               )
    end

    test "policy failure results in failures" do
      assert %Ash.BulkResult{errors: [%Ash.Error.Forbidden{}], records: []} =
               Ash.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_destroy(
                 :destroy_with_policy,
                 %{authorize?: false},
                 authorize?: true,
                 strategy: :atomic,
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
                 return_records?: true
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Ash.bulk_destroy(
                 :destroy_with_policy,
                 %{authorize?: false},
                 authorize?: true,
                 strategy: :stream,
                 resource: Post,
                 stop_on_error?: false,
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
          Post.destroy_with_policy!(post.id, %{authorize?: false})
        end
      end)
    end
  end

  test "respects filter with atomics" do
    assert %Ash.BulkResult{records: [%{title: "foo"}]} =
             Ash.bulk_create!([%{title: "foo"}, %{title: "bar"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy_with_filter, %{},
               resource: Post,
               strategy: :atomic_batches,
               return_records?: true,
               return_errors?: true
             )

    assert [%{title: "bar"}] = Ash.read!(Post)
  end

  test "respects filter with stream" do
    assert %Ash.BulkResult{records: [%{title: "foo"}]} =
             Ash.bulk_create!([%{title: "foo"}, %{title: "bar"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_destroy!(:destroy_with_filter, %{},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               return_errors?: true
             )

    assert [%{title: "bar"}] = Ash.read!(Post)
  end

  test "validates the passed-in action" do
    bulk_result =
      [%{title: "title1"}, %{title: "title2"}]
      |> Ash.bulk_create!(Post, :create, return_records?: true)
      |> Map.get(:records)
      |> Ash.bulk_destroy(:this_is_not_an_actual_destroy_action, %{}, return_errors?: true)

    assert bulk_result.status == :error
    assert [] != bulk_result.errors

    assert [_, _] = Ash.read!(Post)
  end

  test "skipping authorization checks is honoured" do
    posts =
      Ash.bulk_create!([%{title: "delete me"}], Post, :create, return_records?: true)
      |> Map.get(:records)

    result =
      %Ash.BulkResult{} = Ash.bulk_destroy(posts, :forbidden_destroy, %{}, authorize?: false)

    assert result.status == :success
    assert result.error_count == 0

    assert [] = Ash.read!(Post)
  end

  describe "load" do
    test "allows loading has_many relationship" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      assert %Ash.BulkResult{records: [author]} =
               Author
               |> Ash.Changeset.for_create(:create, %{name: "Author"})
               |> Ash.Changeset.manage_relationship(:posts, [post2, post1],
                 type: :append_and_remove
               )
               |> Ash.create!()
               |> List.wrap()
               |> Ash.bulk_destroy!(:destroy, %{},
                 resource: Author,
                 return_records?: true,
                 load: [posts: load_query]
               )

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading many_to_many relationship" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      assert %Ash.BulkResult{records: [post]} =
               Post
               |> Ash.Changeset.for_create(:create, %{title: "Title"})
               |> Ash.Changeset.manage_relationship(
                 :related_posts,
                 [related_post2, related_post1],
                 type: :append_and_remove
               )
               |> Ash.create!()
               |> List.wrap()
               |> Ash.bulk_destroy!(:destroy, %{},
                 resource: Post,
                 return_records?: true,
                 load: [related_posts: load_query]
               )

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end
  end

  describe "nested bulk operations" do
    setup do
      posts =
        Ash.bulk_create!(
          [%{title: "setup1"}, %{title: "setup2"}, %{title: "setup3"}],
          Post,
          :create,
          return_stream?: true,
          return_records?: true,
          authorize?: false
        )
        |> Enum.map(fn {:ok, result} -> result end)

      {:ok, %{posts: posts}}
    end

    test "supports bulk_create in after_action callbacks", %{posts: posts} do
      post_to_destroy = List.first(posts)

      assert %Ash.BulkResult{} =
               Ash.bulk_destroy!([post_to_destroy], :destroy_with_nested_bulk_create, %{},
                 notify?: true,
                 return_records?: false,
                 authorize?: false,
                 strategy: [:stream]
               )
    end

    test "supports bulk_update in after_action callbacks", %{posts: posts} do
      post_to_destroy = List.first(posts)

      assert %Ash.BulkResult{} =
               Ash.bulk_destroy!([post_to_destroy], :destroy_with_nested_bulk_update, %{},
                 notify?: true,
                 return_records?: false,
                 authorize?: false,
                 strategy: [:stream]
               )
    end

    test "supports bulk_destroy in after_action callbacks", %{posts: posts} do
      [post_to_destroy | _] = posts

      assert %Ash.BulkResult{} =
               Ash.bulk_destroy!([post_to_destroy], :destroy_with_nested_bulk_destroy, %{},
                 notify?: true,
                 return_records?: false,
                 authorize?: false,
                 strategy: [:stream]
               )
    end
  end

  describe "notifications contain loaded data" do
    setup do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
        |> Ash.create!()

      {:ok, %{author: author}}
    end

    defp flush_notifications do
      receive do
        {:notification, _} -> flush_notifications()
      after
        0 -> :ok
      end
    end

    defp create_posts_with_author(author, count \\ 5) do
      posts =
        for i <- 1..count do
          Post
          |> Ash.Changeset.for_create(:create, %{title: "Post #{i}"})
          |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
          |> Ash.create!()
        end

      flush_notifications()
      posts
    end

    test "strategy [:stream], transaction: :batch, notify?: true", %{author: author} do
      posts = create_posts_with_author(author)

      assert %Ash.BulkResult{} =
               Ash.bulk_destroy!(posts, :destroy, %{},
                 strategy: [:stream],
                 transaction: :batch,
                 batch_size: 2,
                 notify?: true,
                 return_records?: true,
                 authorize?: false,
                 load: [:author]
               )

      for _ <- 1..5 do
        assert_received {:notification, notification}
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    test "strategy [:stream], transaction: :batch, return_notifications?: true", %{author: author} do
      posts = create_posts_with_author(author)

      result =
        Ash.bulk_destroy!(posts, :destroy, %{},
          strategy: [:stream],
          transaction: :batch,
          batch_size: 2,
          return_notifications?: true,
          return_records?: true,
          authorize?: false,
          load: [:author]
        )

      assert length(result.notifications) == 5

      for notification <- result.notifications do
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    test "strategy [:atomic], transaction: :batch, notify?: true", %{author: author} do
      posts = create_posts_with_author(author)
      post_ids = Enum.map(posts, & &1.id)

      assert %Ash.BulkResult{} =
               Post
               |> Ash.Query.filter(id in ^post_ids)
               |> Ash.bulk_destroy!(:destroy, %{},
                 strategy: [:atomic],
                 transaction: :batch,
                 batch_size: 2,
                 notify?: true,
                 return_records?: true,
                 authorize?: false,
                 load: [:author]
               )

      for _ <- 1..5 do
        assert_received {:notification, notification}
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    test "strategy [:atomic], transaction: :batch, return_notifications?: true", %{author: author} do
      posts = create_posts_with_author(author)
      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy!(:destroy, %{},
          strategy: [:atomic],
          transaction: :batch,
          batch_size: 2,
          return_notifications?: true,
          return_records?: true,
          authorize?: false,
          load: [:author]
        )

      assert length(result.notifications) == 5

      for notification <- result.notifications do
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    test "strategy [:atomic_batches], transaction: :batch, notify?: true", %{author: author} do
      posts = create_posts_with_author(author)

      assert %Ash.BulkResult{} =
               Ash.bulk_destroy!(posts, :destroy, %{},
                 strategy: [:atomic_batches],
                 transaction: :batch,
                 batch_size: 2,
                 notify?: true,
                 return_records?: true,
                 authorize?: false,
                 load: [:author]
               )

      for _ <- 1..5 do
        assert_received {:notification, notification}
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    test "strategy [:atomic_batches], transaction: :batch, return_notifications?: true", %{
      author: author
    } do
      posts = create_posts_with_author(author)

      result =
        Ash.bulk_destroy!(posts, :destroy, %{},
          strategy: [:atomic_batches],
          transaction: :batch,
          batch_size: 2,
          return_notifications?: true,
          return_records?: true,
          authorize?: false,
          load: [:author]
        )

      assert length(result.notifications) == 5

      for notification <- result.notifications do
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    test "return_records?: false with notify?: true still loads notification data", %{
      author: author
    } do
      posts = create_posts_with_author(author)

      assert %Ash.BulkResult{records: nil} =
               Ash.bulk_destroy!(posts, :destroy, %{},
                 strategy: [:stream],
                 batch_size: 2,
                 notify?: true,
                 return_records?: false,
                 authorize?: false,
                 load: [:author]
               )

      for _ <- 1..5 do
        assert_received {:notification, notification}
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    test "return_records?: false with return_notifications?: true still loads notification data",
         %{author: author} do
      posts = create_posts_with_author(author)

      result =
        Ash.bulk_destroy!(posts, :destroy, %{},
          strategy: [:stream],
          batch_size: 2,
          return_notifications?: true,
          return_records?: false,
          authorize?: false,
          load: [:author]
        )

      assert result.records == nil
      assert length(result.notifications) == 5

      for notification <- result.notifications do
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    defp create_posts_for_partial_success_destroy(author) do
      posts =
        for title <- ["ok_1", "ok_2", "ok_3", "fail_4", "ok_5"] do
          Post
          |> Ash.Changeset.for_create(:create, %{title: title})
          |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
          |> Ash.create!()
        end

      flush_notifications()
      posts
    end

    test "partial success with after_action failure and notify?: true", %{author: author} do
      posts = create_posts_for_partial_success_destroy(author)

      result =
        Ash.bulk_destroy(posts, :destroy_with_conditional_after_action_error, %{},
          strategy: [:stream],
          batch_size: 2,
          notify?: true,
          return_errors?: true,
          authorize?: false,
          load: [:author]
        )

      assert result.status == :partial_success
      assert result.error_count == 1

      for _ <- 1..3 do
        assert_received {:notification, notification}
        assert %Author{name: "Test Author"} = notification.data.author
      end

      refute_received {:notification, _}
    end

    test "partial success with after_action failure and return_notifications?: true", %{
      author: author
    } do
      posts = create_posts_for_partial_success_destroy(author)

      result =
        Ash.bulk_destroy(posts, :destroy_with_conditional_after_action_error, %{},
          strategy: [:stream],
          batch_size: 2,
          return_notifications?: true,
          return_errors?: true,
          authorize?: false,
          load: [:author]
        )

      assert result.status == :partial_success
      assert result.error_count == 1
      assert length(result.notifications) == 3

      for notification <- result.notifications do
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end

    test "partial success with strategy [:atomic] and notify?: true", %{author: author} do
      posts = create_posts_for_partial_success_destroy(author)
      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy(:destroy_with_conditional_after_action_error, %{},
          strategy: [:atomic],
          batch_size: 2,
          notify?: true,
          return_errors?: true,
          authorize?: false,
          load: [:author]
        )

      assert result.status == :partial_success
      assert result.error_count == 1

      refute_received {:notification, _}
    end

    test "partial success with strategy [:atomic] and return_notifications?: true", %{
      author: author
    } do
      posts = create_posts_for_partial_success_destroy(author)
      post_ids = Enum.map(posts, & &1.id)

      result =
        Post
        |> Ash.Query.filter(id in ^post_ids)
        |> Ash.bulk_destroy(:destroy_with_conditional_after_action_error, %{},
          strategy: [:atomic],
          batch_size: 2,
          return_notifications?: true,
          return_errors?: true,
          authorize?: false,
          load: [:author]
        )

      assert result.status == :partial_success
      assert result.error_count == 1

      assert result.notifications == []
    end

    test "partial success with strategy [:atomic_batches] and notify?: true", %{author: author} do
      posts = create_posts_for_partial_success_destroy(author)

      result =
        Ash.bulk_destroy(posts, :destroy_with_conditional_after_action_error, %{},
          strategy: [:atomic_batches],
          batch_size: 2,
          notify?: true,
          return_errors?: true,
          authorize?: false,
          load: [:author]
        )

      assert result.status == :partial_success
      assert result.error_count == 1

      for _ <- 1..2 do
        assert_received {:notification, notification}
        assert %Author{name: "Test Author"} = notification.data.author
      end

      refute_received {:notification, _}
    end

    test "partial success with strategy [:atomic_batches] and return_notifications?: true", %{
      author: author
    } do
      posts = create_posts_for_partial_success_destroy(author)

      result =
        Ash.bulk_destroy(posts, :destroy_with_conditional_after_action_error, %{},
          strategy: [:atomic_batches],
          batch_size: 2,
          return_notifications?: true,
          return_errors?: true,
          authorize?: false,
          load: [:author]
        )

      assert result.status == :partial_success
      assert result.error_count == 1

      assert length(result.notifications) == 2

      for notification <- result.notifications do
        assert %Author{name: "Test Author"} = notification.data.author
      end
    end
  end
end
