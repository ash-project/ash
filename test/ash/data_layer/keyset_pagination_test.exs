# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Ash.DataLayer.KeysetPaginationTest do
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule KeysetDataLayer do
    use Spark.Dsl.Extension, sections: []
    @behaviour Ash.DataLayer

    @impl true
    def can?(_, :read), do: true
    def can?(_, :create), do: true
    def can?(_, :limit), do: true
    def can?(_, :sort), do: true
    def can?(_, {:sort, _}), do: true
    def can?(_, :filter), do: true
    def can?(_, {:filter_expr, _}), do: true
    def can?(_, :keyset), do: true
    def can?(_, _), do: false

    @impl true
    def data_layer_keyset_by_default?, do: true

    @impl true
    def resource_to_query(_resource, _domain), do: %{context: %{}}

    @impl true
    def run_query(query, resource) do
      cursor = query.context[:data_layer][:keyset_opts][:after]
      reverse = query.context[:reverse]

      {name, keyset_meta} =
        cond do
          cursor == "page1-cursor" -> {"record-1", "page2-cursor"}
          cursor == "page2-cursor" -> {"record-2", nil}
          reverse -> {"record-3", "reversed-cursor"}
          true -> {"record-0", "page1-cursor"}
        end

      record =
        Ash.create!(resource, %{name: name})
        |> Ash.Resource.put_metadata(:keyset, keyset_meta)

      {:ok, [record]}
    end

    @impl true
    def create(_resource, changeset) do
      Ash.Changeset.apply_attributes(changeset)
    end

    @impl true
    def set_context(_resource, query, context) do
      {:ok, Map.put(query, :context, Map.merge(query.context || %{}, context))}
    end

    @impl true
    def limit(query, limit, _resource), do: {:ok, Map.put(query, :limit, limit)}
    @impl true
    def sort(query, sort, _resource), do: {:ok, Map.put(query, :sort, sort)}
    @impl true
    def filter(query, filter, _resource), do: {:ok, Map.put(query, :filter, filter)}
  end

  defmodule NoKeysetDataLayer do
    use Spark.Dsl.Extension, sections: []
    @behaviour Ash.DataLayer

    @impl true
    def can?(_, :create), do: true
    def can?(_, :read), do: true
    def can?(_, :limit), do: true
    def can?(_, :sort), do: true
    def can?(_, {:sort, _}), do: true
    def can?(_, :filter), do: true
    def can?(_, {:filter_expr, _}), do: true
    def can?(_, :keyset), do: false
    def can?(_, _), do: false

    @impl true
    def data_layer_keyset_by_default?, do: false

    @impl true
    def resource_to_query(_resource, _domain), do: %{context: %{}}

    @impl true
    def run_query(_query, resource) do
      {:ok, [Ash.create!(resource, %{name: "record-0"})]}
    end

    @impl true
    def create(_resource, changeset) do
      Ash.Changeset.apply_attributes(changeset)
    end

    @impl true
    def set_context(_resource, query, context) do
      {:ok, Map.put(query, :context, Map.merge(query.context || %{}, context))}
    end

    @impl true
    def limit(query, limit, _resource), do: {:ok, Map.put(query, :limit, limit)}
    @impl true
    def sort(query, sort, _resource), do: {:ok, Map.put(query, :sort, sort)}
    @impl true
    def filter(query, filter, _resource), do: {:ok, Map.put(query, :filter, filter)}
  end

  defmodule ExplicitKeysetDataLayer do
    use Spark.Dsl.Extension, sections: []
    @behaviour Ash.DataLayer

    @impl true
    def can?(_, :read), do: true
    def can?(_, :create), do: true
    def can?(_, :limit), do: true
    def can?(_, :sort), do: true
    def can?(_, {:sort, _}), do: true
    def can?(_, :filter), do: true
    def can?(_, {:filter_expr, _}), do: true
    def can?(_, :keyset), do: true
    def can?(_, _), do: false

    @impl true
    def data_layer_keyset_by_default?, do: false

    @impl true
    def resource_to_query(_resource, _domain), do: %{context: %{}}

    @impl true
    def run_query(_query, resource) do
      {:ok,
       [
         Ash.create!(resource, %{name: "record-from-dl"})
         |> Ash.Resource.put_metadata(:keyset, "some-cursor")
       ]}
    end

    @impl true
    def create(_resource, changeset) do
      Ash.Changeset.apply_attributes(changeset)
    end

    @impl true
    def set_context(_resource, query, context) do
      {:ok, Map.put(query, :context, Map.merge(query.context || %{}, context))}
    end

    @impl true
    def limit(query, limit, _resource), do: {:ok, Map.put(query, :limit, limit)}
    @impl true
    def sort(query, sort, _resource), do: {:ok, Map.put(query, :sort, sort)}
    @impl true
    def filter(query, filter, _resource), do: {:ok, Map.put(query, :filter, filter)}
  end

  defmodule KeysetResource do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.Test.Ash.DataLayer.KeysetPaginationTest.KeysetDataLayer

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults create: :*

      read :read do
        pagination keyset?: true, via_data_layer?: :data_layer_default
      end
    end
  end

  defmodule ExplicitKeysetResource do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.Test.Ash.DataLayer.KeysetPaginationTest.ExplicitKeysetDataLayer

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults create: :*

      read :read do
        pagination keyset?: true, via_data_layer?: true
      end
    end
  end

  defmodule NoKeysetResource do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.Test.Ash.DataLayer.KeysetPaginationTest.NoKeysetDataLayer

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults create: :*

      read :read do
        pagination keyset?: true, via_data_layer?: false
      end
    end
  end

  describe "use_data_layer_keyset? logic" do
    test "data layer declares :keyset capability and via_data_layer?: :data_layer_default => uses data layer" do
      assert Ash.DataLayer.data_layer_can?(KeysetResource, :keyset)
    end

    test "data layer declares :keyset as false => falls back to Ash keyset pagination" do
      refute Ash.DataLayer.data_layer_can?(NoKeysetResource, :keyset)
      refute Ash.Resource.Info.data_layer(NoKeysetResource).data_layer_keyset_by_default?()
    end
  end

  describe "keyset pagination via data layer (data_layer_keyset_by_default? true)" do
    test "does not add +1 to limit" do
      page =
        KeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.sort(name: :asc)
        |> Ash.Query.page(limit: 1)
        |> Ash.read!()

      assert length(page.results) == 1
    end

    test "more? is true when the last record has non-nil keyset metadata" do
      page =
        KeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.page(limit: 1)
        |> Ash.read!()

      assert page.more?
      assert hd(page.results).__metadata__[:keyset] == "page1-cursor"
    end

    test "more? is false when the last record has nil keyset metadata" do
      page =
        KeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.page(after: "page2-cursor", limit: 1)
        |> Ash.read!()

      refute page.more?
      assert hd(page.results).__metadata__[:keyset] == nil
    end

    test "page forwards correctly using data layer cursor" do
      page1 =
        KeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.page(limit: 1)
        |> Ash.read!()

      assert hd(page1.results).name == "record-0"
      assert hd(page1.results).__metadata__[:keyset] == "page1-cursor"

      page2 =
        KeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.page(after: hd(page1.results).__metadata__[:keyset], limit: 1)
        |> Ash.read!()

      assert hd(page2.results).name == "record-1"
      assert page2.more?
    end

    test "keyset metadata is set by data layer, not overwritten by Ash" do
      page =
        KeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.page(limit: 1)
        |> Ash.read!()

      refute is_nil(hd(page.results).__metadata__[:keyset])
      assert hd(page.results).__metadata__[:keyset] == "page1-cursor"
    end

    test "custom context is passed through to data layer" do
      page =
        KeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.set_context(%{reverse: true})
        |> Ash.Query.page(limit: 1)
        |> Ash.read!()

      assert hd(page.results).name == "record-3"
      assert hd(page.results).__metadata__[:keyset] == "reversed-cursor"
    end
  end

  describe "keyset pagination via data layer (explicit via_data_layer?: true)" do
    test "respects via_data_layer?: true even when data_layer_keyset_by_default? returns false" do
      page =
        ExplicitKeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.page(limit: 1)
        |> Ash.read!()

      assert hd(page.results).name == "record-from-dl"
      assert hd(page.results).__metadata__[:keyset] == "some-cursor"
    end
  end

  describe "keyset pagination not via data layer (via_data_layer?: false)" do
    test "falls back to Ash managed keyset pagination" do
      data_layer = Ash.Resource.Info.data_layer(NoKeysetResource)
      refute data_layer.can?(NoKeysetResource, :keyset)
    end

    test "data layer does not need to implement cursor logic" do
      page =
        NoKeysetResource
        |> Ash.Query.new()
        |> Ash.Query.for_read(:read)
        |> Ash.Query.page(limit: 1)
        |> Ash.read!()

      assert length(page.results) == 1
    end
  end
end
