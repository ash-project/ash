defmodule Ash.Test.Actions.BulkCreateTest do
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

      create :create_with_change do
        change fn changeset, _ ->
          title = Ash.Changeset.get_attribute(changeset, :title)
          Ash.Changeset.force_change_attribute(changeset, :title, title <> "_stuff")
        end
      end

      create :create_with_argument do
        argument :a_title, :string do
          allow_nil? false
        end

        change set_attribute(:title, arg(:a_title))
      end

      create :create_with_after_action do
        change after_action(fn _changeset, result ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      create :create_with_after_batch do
        change AddAfterToTitle
        change AddBeforeToTitle
      end

      create :create_with_after_transaction do
        change after_transaction(fn _changeset, {:ok, result} ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      create :create_with_policy do
        argument :authorize?, :boolean, allow_nil?: false

        change set_context(%{authorize?: arg(:authorize?)})
      end
    end

    identities do
      identity :unique_title, :title do
        pre_check_with Ash.Test.Actions.BulkCreateTest
      end
    end

    policies do
      policy action(:create_with_policy) do
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

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Post
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "returns created records" do
    assert %Ash.BulkResult{records: [%{title: "title1"}, %{title: "title2"}]} =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_records?: true,
               sorted?: true
             )
  end

  test "runs changes" do
    assert %Ash.BulkResult{records: [%{title: "title1_stuff"}, %{title: "title2_stuff"}]} =
             Api.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create_with_change,
               return_records?: true,
               sorted?: true
             )
  end

  test "accepts arguments" do
    assert %Ash.BulkResult{records: [%{title: "title1"}, %{title: "title2"}]} =
             Api.bulk_create!(
               [%{a_title: "title1"}, %{a_title: "title2"}],
               Post,
               :create_with_argument,
               return_records?: true,
               sorted?: true
             )
  end

  test "runs after batch hooks" do
    assert %Ash.BulkResult{
             records: [%{title: "before_title1_after"}, %{title: "before_title2_after"}]
           } =
             Api.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_after_batch,
               return_records?: true,
               sorted?: true
             )
  end

  test "will return error count" do
    assert %Ash.BulkResult{records: [%{title: "title1_stuff"}], error_count: 1, errors: []} =
             Api.bulk_create!(
               [%{title: "title1"}, %{title: %{foo: :bar}}],
               Post,
               :create_with_change,
               return_records?: true,
               sorted?: true
             )
  end

  test "will return errors on request" do
    assert %Ash.BulkResult{
             records: [%{title: "title1_stuff"}],
             error_count: 1,
             errors: [%Ash.Changeset{}]
           } =
             Api.bulk_create!(
               [%{title: "title1"}, %{title: %{foo: :bar}}],
               Post,
               :create_with_change,
               return_records?: true,
               return_errors?: true,
               sorted?: true
             )
  end

  test "can upsert" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "changes", title3: "wont"},
               %{title: "title2", title2: "changes", title3: "wont"}
             ]
           } =
             Api.bulk_create!(
               [
                 %{title: "title1", title2: "changes", title3: "wont"},
                 %{title: "title2", title2: "changes", title3: "wont"}
               ],
               Post,
               :create,
               return_records?: true,
               sorted?: true
             )

    assert %Ash.BulkResult{
             records: [
               %{title: "title1", title2: "did_change", title3: "wont"},
               %{title: "title2", title2: "did_change", title3: "wont"}
             ]
           } =
             Api.bulk_create!(
               [
                 %{title: "title1", title2: "did_change", title3: "oh no"},
                 %{title: "title2", title2: "did_change", title3: "what happened"}
               ],
               Post,
               :create,
               return_records?: true,
               upsert?: true,
               upsert_identity: :unique_title,
               upsert_fields: [:title2],
               sorted?: true
             )
  end

  test "runs after action hooks" do
    assert %Ash.BulkResult{records: [%{title: "title1_stuff"}, %{title: "title2_stuff"}]} =
             Api.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_after_action,
               return_records?: true,
               sorted?: true
             )
  end

  test "runs after transaction hooks" do
    assert %Ash.BulkResult{records: [%{title: "title1_stuff"}, %{title: "title2_stuff"}]} =
             Api.bulk_create!(
               [%{title: "title1"}, %{title: "title2"}],
               Post,
               :create_with_after_transaction,
               return_records?: true,
               sorted?: true
             )
  end

  describe "authorization" do
    test "policy success results in successes" do
      assert %Ash.BulkResult{records: [%{title: "title1"}, %{title: "title2"}]} =
               Api.bulk_create!(
                 [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}],
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 return_records?: true,
                 sorted?: true
               )
    end

    test "policy failure results in failures" do
      assert %Ash.BulkResult{errors: [_, _]} =
               Api.bulk_create!(
                 [%{title: "title1", authorize?: false}, %{title: "title2", authorize?: false}],
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 return_records?: true,
                 return_errors?: true,
                 sorted?: true
               )
    end
  end

  describe "streaming" do
    test "by default nothing is returned in the stream" do
      assert [] =
               [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}]
               |> Api.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 return_stream?: true
               )
               |> Enum.to_list()
    end

    test "by returning notifications, you get the notifications in the stream" do
      assert [{:notification, _}, {:notification, _}] =
               [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}]
               |> Api.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 return_stream?: true,
                 notify?: true,
                 return_notifications?: true
               )
               |> Enum.to_list()
    end

    test "by returning records, you get the records in the stream" do
      assert [{:ok, %{title: "title1"}}, {:ok, %{title: "title2"}}] =
               [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}]
               |> Api.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
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
      assert [
               {:notification, _},
               {:notification, _},
               {:ok, %{title: "title1"}},
               {:ok, %{title: "title2"}}
             ] =
               [%{title: "title1", authorize?: true}, %{title: "title2", authorize?: true}]
               |> Api.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 notify?: true,
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
      assert [
               {:error, %Ash.Changeset{}},
               {:notification, _},
               {:ok, %{title: "title1"}}
             ] =
               [
                 %{title: "title1", authorize?: true},
                 %{title: "title2", authorize?: false}
               ]
               |> Api.bulk_create!(
                 Post,
                 :create_with_policy,
                 authorize?: true,
                 notify?: true,
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
end
