# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.UpdateManyMultitenancyTest do
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
               :nested_expressions,
               :multitenancy,
               :composite_primary_key
             ],
        do: true

    def can?(_, {:atomic, :update}), do: true
    def can?(_, {:filter_expr, _}), do: true
    def can?(_, _), do: false

    @impl true
    def resource_to_query(resource, domain), do: %{resource: resource, domain: domain}

    @impl true
    def update_many(_resource, changesets, opts) do
      changesets = Enum.to_list(changesets)
      send(self(), {:update_many, changesets, opts})

      records =
        Enum.map(changesets, fn changeset ->
          {:ok, record} = Ash.Changeset.apply_attributes(changeset)
          record
        end)

      {:ok, records}
    end
  end

  defmodule Invoice do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: RecordingDataLayer

    multitenancy do
      strategy :attribute
      attribute :tenant_id
    end

    actions do
      defaults update: [:amount]
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :tenant_id, :string, allow_nil?: false, public?: true
      attribute :amount, :integer, allow_nil?: false, public?: true
    end
  end

  test "an atomic update_many without a tenant is rejected on a multitenant resource" do
    victim = struct(Invoice, id: 9, tenant_id: "victim", amount: 500)

    result =
      Ash.update_many(
        [{victim, %{amount: 0}}],
        Invoice,
        :update,
        strategy: [:atomic],
        return_records?: true,
        return_errors?: true
      )

    assert result.status == :error

    assert result.errors
           |> List.wrap()
           |> Enum.flat_map(fn
             %{errors: nested} -> List.wrap(nested)
             other -> [other]
           end)
           |> Enum.any?(&match?(%Ash.Error.Invalid.TenantRequired{}, &1))

    refute_receive {:update_many, _, _}
  end

  test "an atomic update_many with a tenant scopes the write with a tenant filter" do
    victim = struct(Invoice, id: 9, tenant_id: "acme", amount: 500)

    result =
      Ash.update_many(
        [{victim, %{amount: 0}}],
        Invoice,
        :update,
        tenant: "acme",
        strategy: [:atomic],
        return_records?: true,
        return_errors?: true
      )

    assert result.status == :success

    assert_receive {:update_many, [changeset], opts}
    assert opts.tenant == "acme"
    assert changeset.to_tenant == "acme"
    refute is_nil(changeset.filter)
  end
end
