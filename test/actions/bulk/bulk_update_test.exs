defmodule Ash.Test.Actions.BulkUpdateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

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
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]

      update :update_with_change do
        change fn changeset, _ ->
          title = Ash.Changeset.get_attribute(changeset, :title)
          Ash.Changeset.force_change_attribute(changeset, :title, title <> "_stuff")
        end
      end

      update :update_with_argument do
        argument :a_title, :string do
          allow_nil? false
        end

        change set_attribute(:title2, arg(:a_title))
      end

      update :update_with_after_action do
        change after_action(fn _changeset, result, _context ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      update :update_with_after_batch do
        change AddAfterToTitle
        change AddBeforeToTitle
      end

      update :update_with_after_transaction do
        change after_transaction(fn _changeset, {:ok, result}, _context ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      update :update_with_policy do
        argument :authorize?, :boolean, allow_nil?: false

        change set_context(%{authorize?: arg(:authorize?)})
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

    policies do
      policy action(:update_with_policy) do
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
      attribute :hidden_attribute, :string, public?: true

      timestamps()
    end
  end

  test "returns updated records" do
    assert %Ash.BulkResult{records: [%{title2: "updated value"}, %{title2: "updated value"}]} =
             Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Domain.bulk_update!(:update, %{title2: "updated value"},
               resource: Post,
               return_records?: true,
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
             Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Domain.bulk_update!(:update_with_change, %{title2: "updated value"},
               resource: Post,
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
             Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Domain.bulk_update!(:update_with_argument, %{a_title: "updated value"},
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
             Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Domain.bulk_update!(:update_with_after_batch, %{title2: "updated value"},
               resource: Post,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "will return error count" do
    assert %Ash.BulkResult{
             error_count: 2
           } =
             Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Domain.bulk_update(:update, %{title2: %{invalid: :value}},
               resource: Post,
               return_records?: true,
               authorize?: false
             )
  end

  test "will return errors on request" do
    assert %Ash.BulkResult{
             error_count: 1,
             errors: [%Ash.Changeset{}]
           } =
             Domain.bulk_create!([%{title: "title1"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Domain.bulk_update(:update, %{title2: %{invalid: :value}},
               resource: Post,
               return_errors?: true,
               authorize?: false
             )
  end

  test "runs after action hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff", title2: "updated value"},
               %{title: "title2_stuff", title2: "updated value"}
             ]
           } =
             Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Domain.bulk_update!(:update_with_after_action, %{title2: "updated value"},
               resource: Post,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  test "runs after transaction hooks" do
    assert %Ash.BulkResult{
             records: [
               %{title: "title1_stuff", title2: "updated value"},
               %{title: "title2_stuff", title2: "updated value"}
             ]
           } =
             Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
               return_stream?: true,
               return_records?: true,
               authorize?: false
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Domain.bulk_update!(:update_with_after_transaction, %{title2: "updated value"},
               resource: Post,
               return_records?: true,
               return_errors?: true,
               authorize?: false
             )
             |> Map.update!(:records, fn records ->
               Enum.sort_by(records, & &1.title)
             end)
  end

  describe "authorization" do
    test "policy success results in successes" do
      assert %Ash.BulkResult{records: [_, _], errors: []} =
               Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Domain.bulk_update(
                 :update_with_policy,
                 %{title2: "updated value", authorize?: true},
                 authorize?: true,
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
               Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Domain.bulk_update(
                 :update_with_policy,
                 %{title2: "updated value", authorize?: true},
                 authorize?: true,
                 resource: Post,
                 return_records?: true,
                 return_errors?: true,
                 load: [:hidden_calc]
               )
    end

    test "policy failure results in failures" do
      assert %Ash.BulkResult{errors: [_, _], records: []} =
               Domain.bulk_create!([%{title: "title1"}, %{title: "title2"}], Post, :create,
                 return_stream?: true,
                 return_records?: true,
                 authorize?: false
               )
               |> Stream.map(fn {:ok, result} ->
                 result
               end)
               |> Domain.bulk_update(
                 :update_with_policy,
                 %{title2: "updated value", authorize?: false},
                 authorize?: true,
                 resource: Post,
                 return_records?: true,
                 return_errors?: true
               )
    end
  end
end
