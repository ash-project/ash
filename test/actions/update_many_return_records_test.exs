# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.UpdateManyReturnRecordsTest do
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

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: RecordingDataLayer

    actions do
      defaults update: [:title]
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :title, :string, public?: true
    end
  end

  test "update_many works without return_records?" do
    post = struct(Post, id: 1, title: "old")

    result = Ash.update_many([{post, %{title: "new"}}], Post, :update)

    assert result.status == :success
    assert result.records == nil
    assert_receive {:update_many, [_changeset]}
  end
end
