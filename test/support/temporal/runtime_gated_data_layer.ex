# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.RuntimeGatedDataLayer do
  @moduledoc """
  A data layer whose temporal support depends on the running store, and stores nothing.

  A layer that gates temporal on something it only learns at runtime — a server version,
  a storage feature, an edition — has to answer `can?(:temporal)` before it knows, because
  `Ash.Resource.Verifiers.ValidateTemporal` asks at compile time and fails the resource if
  the answer is false. So it grants temporal while compiling and may withdraw it later.

  That is the only way to reach the branches where a resource is declared temporal and its
  data layer declines — `Ash.Query.add_as_of/2` on a read, `refuse_unserved_temporal/1` on
  a write — so `supported?/1` makes the withdrawal explicit for a test. It defaults to
  granting, which is what compilation needs.
  """
  use Spark.Dsl.Extension, transformers: [], sections: []

  @behaviour Ash.DataLayer

  @doc "Grants or withdraws temporal support for subsequently built queries."
  def supported?(value), do: Application.put_env(:ash, :test_runtime_gated_temporal?, value)

  @doc false
  @impl true
  def can?(_, :temporal), do: Application.get_env(:ash, :test_runtime_gated_temporal?, true)
  def can?(_, :read), do: true
  def can?(_, type) when type in [:create, :update, :destroy], do: true
  def can?(_, _), do: false

  @doc false
  @impl true
  def resource_to_query(resource, domain) do
    %Ash.DataLayer.Simple.Query{resource: resource, domain: domain}
  end

  @doc false
  @impl true
  def set_as_of(_resource, query, _as_of), do: {:ok, query}

  @doc false
  @impl true
  def run_query(_query, _resource), do: {:ok, []}

  @doc false
  @impl true
  def create(_resource, changeset), do: {:ok, changeset.data}
end
