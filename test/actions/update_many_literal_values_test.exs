# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.UpdateManyLiteralValuesTest do
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

  defmodule Slug do
    @moduledoc false
    use Ash.Type.NewType, subtype_of: :string

    @impl Ash.Type
    def constraints do
      [lowercase?: [type: :boolean, default: false]]
    end

    @impl Ash.Type
    def apply_constraints(value, constraints) do
      if constraints[:lowercase?] && value != String.downcase(value) do
        {:error, message: "must be lowercase"}
      else
        {:ok, value}
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: RecordingDataLayer

    actions do
      defaults [:read]

      update :update do
        primary? true
        accept [:published_at, :slug, :score]
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :published_at, :utc_datetime_usec, allow_nil?: false, public?: true
      attribute :slug, Slug, public?: true, constraints: [lowercase?: true]
      attribute :score, :integer, public?: true, constraints: [min: 0]
    end
  end

  defp post(id) do
    struct(Post, id: id, published_at: ~U[2026-01-01 00:00:00.000000Z], slug: "old", score: 0)
  end

  test "distinct datetime literals are written in one update_many call" do
    result =
      1..3
      |> Enum.map(
        &{post(&1), %{published_at: DateTime.add(~U[2026-01-01 00:00:00Z], &1, :minute)}}
      )
      |> Ash.update_many(Post, :update, return_records?: true, return_errors?: true)

    assert result.status == :success
    assert_receive {:update_many, changesets}
    refute_receive {:update_many, _}
    assert length(changesets) == 3

    for %{id: id} = record <- result.records do
      assert record.published_at ==
               DateTime.add(~U[2026-01-01 00:00:00.000000Z], id, :minute)
    end
  end

  test "literal values are cast and constrained before they are written" do
    result =
      Ash.update_many(
        [{post(1), %{published_at: "2026-02-03T04:05:06.000007Z", slug: "new", score: "4"}}],
        Post,
        :update,
        return_records?: true,
        return_errors?: true
      )

    assert result.status == :success

    assert [%{published_at: ~U[2026-02-03 04:05:06.000007Z], slug: "new", score: 4}] =
             result.records

    for input <- [%{slug: "New"}, %{score: -1}] do
      result =
        Ash.update_many([{post(1), input}], Post, :update,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :error
    end
  end
end
