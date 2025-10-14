# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.Selecting do
  @moduledoc "This check is true when the field is being selected and false when it is not."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "selecting #{opts[:attribute]}"
  end

  @impl true
  def match?(_actor, %{changeset: %Ash.Changeset{} = changeset}, opts) do
    Ash.Changeset.selecting?(changeset, opts[:attribute])
  end

  def match?(_actor, %{query: %Ash.Query{} = query}, opts) do
    Ash.Query.selecting?(query, opts[:attribute])
  end

  def match?(_, _, _), do: false
end
