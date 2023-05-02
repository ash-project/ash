defmodule Ash.Test.Actions.BulkCreateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

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

      create :create_with_after_action do
        change after_action(fn _changeset, result ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end

      create :create_with_after_transaction do
        change after_transaction(fn _changeset, {:ok, result} ->
                 {:ok, %{result | title: result.title <> "_stuff"}}
               end)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false

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
end
