# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.DataLayer.Mnesia.Info do
  @moduledoc "Introspection helpers for Ash.DataLayer.Mnesia"

  alias Spark.Dsl.Extension

  @doc "The mnesia table for a resource"
  def table(resource) do
    Extension.get_opt(resource, [:mnesia], :table, resource, true)
  end
end
