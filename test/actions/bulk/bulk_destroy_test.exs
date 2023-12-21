defmodule Ash.Test.Actions.BulkDestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule AddAfterToTitle do
    use Ash.Resource.Change

    def change(changeset, _, %{bulk?: true}) do
      changeset
    end

    def after_batch(results, _, _) do
      Stream.map(results, fn {_changeset, result} ->
        {:ok, %{result | title: result.title <> "_after"}}
      end)
    end
  end

  defmodule AddBeforeToTitle do
    use Ash.Resource.Change

    def change(changeset, _, %{bulk?: true}) do
      changeset
    end

    def before_batch(changesets, _, _) do
      changesets
      |> Stream.map(fn changeset ->
        title = Ash.Changeset.get_attribute(changeset, :title)
        Ash.Changeset.force_change_attribute(changeset, :title, "before_" <> title)
      end)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      destroy :destroy_with_change do
        change fn changeset, _ ->
          title = Ash.Changeset.get_attribute(changeset, :title)
          Ash.Changeset.force_change_attribute(changeset, :title, title <> "_stuff")
        end
      end

      destroy :destroy_with_argument do
        argument :a_title, :string do
          allow_nil? false
        end

        change set_attribute(:title2, arg(:a_title))
      end

      destroy :destroy_with_after_action do
        change after_action(fn _changeset, result ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      destroy :destroy_with_after_batch do
        change AddAfterToTitle
        change AddBeforeToTitle
      end

      destroy :destroy_with_after_transaction do
        change after_transaction(fn _changeset, {:ok, result} ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      destroy :destroy_with_policy do
        argument :authorize?, :boolean, allow_nil?: false

        change set_context(%{authorize?: arg(:authorize?)})
      end
    end

    identities do
      identity :unique_title, :title do
        pre_check_with Ash.Test.Actions.BulkUpdateTest.Api
      end
    end

    policies do
      policy action(:destroy_with_policy) do
        authorize_if context_equals(:authorize?, true)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false
      attribute :title2, :string
      attribute :title3, :string

      timestamps()
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Post
    end
  end

  test "returns destroyed records" do
    assert %Ash.BulkResult{records: [%{}, %{}]} =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Api.bulk_destroy!(:destroy, %{},
               resource: Post,
               return_records?: true,
               return_errors?: true
             )

    assert [] = Api.read!(Post)
  end

  test "runs changes" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff"},
               %{title: "title2_stuff"}
             ]
           } =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Api.bulk_destroy!(:destroy_with_change, %{},
               resource: Post,
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Api.read!(Post)
  end

  test "accepts arguments" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "updated value"},
               %{title: "title2", title2: "updated value"}
             ]
           } =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Api.bulk_destroy!(:destroy_with_argument, %{a_title: "updated value"},
               resource: Post,
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Api.read!(Post)
  end

  test "runs after batch hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "before_title1_after"},
               %{title: "before_title2_after"}
             ]
           } =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Api.bulk_destroy!(:destroy_with_after_batch, %{},
               resource: Post,
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Api.read!(Post)
  end

  test "will return error count" do
    assert %Ash.BulkResult{
             error_count: 2
           } =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Api.bulk_destroy(:destroy, %{title2: "what"},
               resource: Post,
               return_records?: true
             )

    assert [_, _] = Api.read!(Post)
  end

  test "will return errors on request" do
    assert %Ash.BulkResult{
             error_count: 1,
             errors: [%Ash.Changeset{}]
           } =
             Api.bulk_create!([%{title: "title1"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Api.bulk_destroy(:destroy, %{title: %{invalid: :value}},
               resource: Post,
               return_errors?: true
             )

    assert [_] = Api.read!(Post)
  end

  test "runs after action hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff"},
               %{title: "title2_stuff"}
             ]
           } =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Api.bulk_destroy!(:destroy_with_after_action, %{},
               resource: Post,
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Api.read!(Post)
  end

  test "runs after transaction hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff"},
               %{title: "title2_stuff"}
             ]
           } =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Api.bulk_destroy!(:destroy_with_after_transaction, %{},
               resource: Post,
               return_records?: true,
               return_errors?: true
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)

    assert [] = Api.read!(Post)
  end

  describe "authorization" do
    test "policy success results in successes" do
      assert %Ash.BulkResult{records: [_, _], errors: []} =
               Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Api.bulk_destroy(
                 :destroy_with_policy,
                 %{authorize?: true},
                 authorize?: true,
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )
    end

    test "policy failure results in failures" do
      assert %Ash.BulkResult{errors: [_, _], records: []} =
               Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Api.bulk_destroy(
                 :destroy_with_policy,
                 %{authorize?: false},
                 authorize?: true,
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )
    end
  end
end
