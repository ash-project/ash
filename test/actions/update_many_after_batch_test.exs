# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.UpdateManyAfterBatchTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule RecordingDataLayer do
    @moduledoc false
    @behaviour Ash.DataLayer
    use Spark.Dsl.Extension, transformers: [], sections: []

    @impl true
    def can?(_, feature)
        when feature in [
               :update_many,
               :update,
               :filter,
               :changeset_filter,
               :boolean_filter,
               :nested_expressions
             ],
        do: true

    def can?(_, {:atomic, :update}), do: true
    def can?(_, {:filter_expr, _}), do: true
    def can?(_, _), do: false

    @impl true
    def resource_to_query(resource, domain), do: %{resource: resource, domain: domain}

    @impl true
    def update_many(_resource, changesets, _opts) do
      changesets = Enum.to_list(changesets)
      send(self(), {:update_many, changesets})

      records =
        Enum.map(changesets, fn changeset ->
          {:ok, record} = Ash.Changeset.apply_attributes(changeset)
          record
        end)

      {:ok, records}
    end
  end

  defmodule ReportAttributes do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context), do: changeset

    @impl true
    def atomic(changeset, _opts, _context), do: {:ok, changeset}

    @impl true
    def batch_change(changesets, _opts, _context), do: changesets

    @impl true
    def after_batch(changesets_and_results, _opts, _context) do
      Enum.map(changesets_and_results, fn {changeset, result} ->
        send(self(), {:after_batch, result.id, changeset.attributes})
        {:ok, result}
      end)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: RecordingDataLayer

    actions do
      defaults [:read]

      update :update do
        primary? true
        accept [:title, :score]
        change ReportAttributes
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :title, :string, public?: true
      attribute :score, :integer, public?: true
    end
  end

  test "after_batch hooks see each row's own changeset" do
    1..3
    |> Enum.map(fn id ->
      {struct(Post, id: id, title: "old", score: 0), %{title: "post #{id}", score: id}}
    end)
    |> Ash.update_many(Post, :update, return_records?: true, return_errors?: true)

    assert_receive {:update_many, changesets}
    assert length(changesets) == 3

    for id <- 1..3 do
      assert_receive {:after_batch, ^id, attributes}
      assert attributes == %{title: "post #{id}", score: id}
    end
  end
end
