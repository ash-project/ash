defmodule Ash.Type.Struct do
  @moduledoc """
  Represents a struct.

  This cannot be loaded from a database, it can only be used to cast input.

  Use the `instance_of` constraint to specify that it must be an instance of a specific struct.
  """
  use Ash.Type

  @constraints [
    instance_of: [
      type: :atom,
      doc: "The module the struct should be an instance of"
    ]
  ]

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(%struct{} = value, constraints) do
    case constraints[:instance_of] do
      nil ->
        {:ok, value}

      ^struct ->
        {:ok, value}

      _ ->
        :error
    end
  end

  def cast_input(_, _), do: :error

  @impl Ash.Type
  def load(record, load, _constraints, %{domain: domain} = context) do
    opts = Ash.Context.to_opts(context, domain: domain)

    Ash.load(record, load, opts)
  end

  @impl Ash.Type
  def merge_load(left, right, constraints, context) do
    instance_of = constraints[:instance_of]
    left = Ash.Query.load(instance_of, left)
    right = Ash.Query.load(instance_of, right)

    if left.valid? do
      {:ok, Ash.Query.merge_query_load(left, right, context)}
    else
      {:error, Ash.Error.to_ash_error(left.errors)}
    end
  end

  @impl Ash.Type
  def get_rewrites(merged_load, calculation, path, constraints) do
    instance_of = constraints[:instance_of]

    if instance_of && Ash.Resource.Info.resource?(instance_of) do
      merged_load = Ash.Query.load(instance_of, merged_load)
      Ash.Actions.Read.Calculations.get_all_rewrites(merged_load, calculation, path)
    else
      []
    end
  end

  @impl Ash.Type
  def rewrite(value, rewrites, _constraints) do
    Ash.Actions.Read.Calculations.rewrite(rewrites, value)
  end

  @impl Ash.Type
  def can_load?(constraints) do
    instance_of = constraints[:instance_of]

    instance_of && Ash.Resource.Info.resource?(instance_of)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}
  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(_, _), do: :error
end
