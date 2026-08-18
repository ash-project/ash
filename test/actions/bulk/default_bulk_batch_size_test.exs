# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.DefaultBulkBatchSizeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule CreateDataLayer do
    @moduledoc false
    use Spark.Dsl.Extension, sections: []
    @behaviour Ash.DataLayer

    @impl true
    def can?(_, :create), do: true
    def can?(_, :bulk_create), do: true
    def can?(_, :read), do: true
    def can?(_, _), do: false

    @impl true
    def resource_to_query(_resource, _domain), do: %{}

    @impl true
    def run_query(_query, _resource), do: {:ok, []}

    @impl true
    def create(resource, changeset) do
      {:ok, struct(resource, changeset.attributes)}
    end

    @impl true
    def bulk_create(resource, changesets, _opts) do
      changesets = Enum.to_list(changesets)
      send(self(), {:bulk_batch, :create, length(changesets)})

      records =
        Enum.map(changesets, fn changeset ->
          resource
          |> struct(changeset.attributes)
          |> Ash.Actions.Helpers.Bulk.put_metadata(changeset)
        end)

      {:ok, records}
    end

    @impl true
    def default_bulk_batch_size(_resource, %{name: :create_small}), do: 4
    def default_bulk_batch_size(_resource, _action), do: 2
  end

  defmodule CreatePost do
    @moduledoc false
    use Ash.Resource, data_layer: CreateDataLayer, domain: Domain

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:create, :read]
      create :create_small
    end
  end

  test "uses the data layer's default_bulk_batch_size when no batch_size option is given" do
    Ash.bulk_create!(
      [%{title: "a"}, %{title: "b"}, %{title: "c"}, %{title: "d"}, %{title: "e"}],
      CreatePost,
      :create,
      return_records?: true
    )

    assert_received {:bulk_batch, :create, 2}
    assert_received {:bulk_batch, :create, 2}
    assert_received {:bulk_batch, :create, 1}
  end

  test "an explicit batch_size option overrides default_bulk_batch_size" do
    Ash.bulk_create!(
      [%{title: "a"}, %{title: "b"}, %{title: "c"}, %{title: "d"}, %{title: "e"}],
      CreatePost,
      :create,
      batch_size: 3,
      return_records?: true
    )

    assert_received {:bulk_batch, :create, 3}
    assert_received {:bulk_batch, :create, 2}
  end

  test "default_bulk_batch_size receives the action, so different actions can size differently" do
    Ash.bulk_create!(
      [%{title: "a"}, %{title: "b"}, %{title: "c"}, %{title: "d"}, %{title: "e"}],
      CreatePost,
      :create_small,
      return_records?: true
    )

    assert_received {:bulk_batch, :create, 4}
    assert_received {:bulk_batch, :create, 1}
  end

  test "Ash.DataLayer.default_bulk_batch_size/2 returns the data layer's value when exported" do
    action = Ash.Resource.Info.action(CreatePost, :create)
    assert Ash.DataLayer.default_bulk_batch_size(CreatePost, action) == 2
  end

  defmodule EtsPost do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: true, public?: true
    end

    actions do
      default_accept :*
      defaults [:create, :read]
    end
  end

  test "Ash.DataLayer.default_bulk_batch_size/2 returns nil when the data layer does not export it" do
    action = Ash.Resource.Info.action(EtsPost, :create)
    assert Ash.DataLayer.default_bulk_batch_size(EtsPost, action) == nil
  end
end
