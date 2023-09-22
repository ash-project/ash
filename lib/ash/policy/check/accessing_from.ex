defmodule Ash.Policy.Check.AccessingFrom do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    "accessing from #{inspect(options[:source])}.#{options[:relationship]}"
  end

  @impl true
  def match?(_actor, %{changeset: %Ash.Changeset{} = changeset}, options) do
    accessing_from = changeset.context[:accessing_from]

    if accessing_from do
      accessing_from.source == options[:source] &&
        accessing_from.name == options[:relationship]
    else
      false
    end
  end

  def match?(_actor, %{query: %Ash.Query{} = query}, options) do
    accessing_from = query.context[:accessing_from]

    if accessing_from do
      accessing_from.source == options[:source] &&
        accessing_from.name == options[:relationship]
    else
      false
    end
  end

  def match?(_, _, _) do
    raise "Field Policy"
  end
end
