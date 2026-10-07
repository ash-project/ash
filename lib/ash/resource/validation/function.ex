# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.Function do
  @moduledoc false

  use Ash.Resource.Validation

  # Wraps an arbitrary function, whose temporal safety cannot be known.
  @impl true
  def temporal_safe?(_opts), do: false

  import Ash.Gettext

  @impl true
  def validate(changeset_query_or_input, [{:fun, {m, f, a}}], context) do
    apply(m, f, [changeset_query_or_input, context | a])
  end

  @impl true
  def validate(changeset_query_or_input, [{:fun, fun}], context) do
    fun.(changeset_query_or_input, context)
  end

  @impl true
  def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]

  @impl true
  def describe(opts) do
    [
      message: error_message("must pass function %{function}"),
      vars: [function: opts[:fun]]
    ]
  end
end
