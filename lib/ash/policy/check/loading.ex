defmodule Ash.Policy.Check.Loading do
  @moduledoc "This check is true when the field or relationship, or path to field, is being loaded and false when it is not."
  use Ash.Policy.SimpleCheck
  require Logger

  @impl true
  def describe(opts) do
    "loading #{opts[:field]}"
  end

  @impl true
  def requires_original_data?(_, _), do: false

  @impl true
  def match?(_actor, %{query: %Ash.Query{} = query}, opts) do
    Logger.warning(
      "`loading/1` check is deprecated! Use field policies to secure field access instead."
    )

    Ash.Query.loading?(query, opts[:field])
  end

  def match?(_actor, %{changeset: %Ash.Changeset{} = query}, opts) do
    Logger.warning(
      "`loading/1` check is deprecated! Use field policies to secure field access instead."
    )

    Ash.Changeset.loading?(query, opts[:field])
  end

  def match?(_, _, _) do
    Logger.warning(
      "`loading/1` check is deprecated! Use field policies to secure field access instead."
    )

    false
  end
end
