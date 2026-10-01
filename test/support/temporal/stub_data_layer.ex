# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.StubDataLayer do
  @moduledoc """
  A data layer that says it supports temporal resources, and stores nothing.
  """
  use Spark.Dsl.Extension, transformers: [], sections: []

  @behaviour Ash.DataLayer

  @doc false
  @impl true
  def can?(_, :temporal), do: true
  def can?(_, type) when type in [:create, :update, :destroy], do: true
  def can?(_, _), do: false

  @doc false
  @impl true
  def resource_to_query(resource, domain) do
    %Ash.DataLayer.Simple.Query{resource: resource, domain: domain}
  end
end
