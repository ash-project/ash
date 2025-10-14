# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.ChangingRelationships do
  @moduledoc "This check is true when the specified relationship is changing"
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    case opts[:relationships] do
      [relationship] ->
        "changing relationship: #{relationship}"

      relationships ->
        "changing any of #{Enum.join(relationships, ",")} relationships"
    end
  end

  @impl true
  def match?(_actor, %{changeset: %Ash.Changeset{} = changeset}, opts) do
    Enum.any?(opts[:relationships], fn relationship ->
      Ash.Changeset.changing_relationship?(changeset, relationship)
    end)
  end

  def match?(_, _, _), do: false
end
