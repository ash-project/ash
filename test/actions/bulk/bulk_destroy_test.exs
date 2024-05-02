defmodule Ash.Test.Actions.BulkDestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
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

      destroy :soft do
        require_atomic? false
        soft? true
        change set_attribute(:title2, "archived")
      end
    end

    identities do
      identity :unique_title, :title do
        pre_check_with Ash.Test.Actions.BulkUpdateTest.Domain
      end
    end

    policies do
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
                 return_records?: true,
                 return_errors?: true
               )
    end
  end
end
