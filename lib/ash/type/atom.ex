defmodule Ash.Type.Atom do
  @constraints [
    one_of: [
      type: :any,
      doc: "Allows constraining the value of an atom to a pre-defined list"
    ]
  ]
  @moduledoc """
  Stores an atom as a string in the database

  A builtin type that can be referenced via `:atom`

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :string

  @impl true
  def constraints, do: @constraints

  @impl true
  def matches_type?(v, _) do
    is_atom(v)
  end

  @impl true
  def generator(constraints) do
    # We don't want to create tons of atoms.
    one_of = constraints[:one_of] || [:example, :atom, :value]
    StreamData.member_of(one_of)
  end

  def apply_constraints(nil, _), do: {:ok, nil}

  def apply_constraints(value, constraints) do
    errors =
      Enum.reduce(constraints, [], fn
        {:one_of, atom_list}, errors ->
          if Enum.member?(atom_list, value) do
            errors
          else
            [
              [
                message: "atom must be one of %{atom_list}, got: %{value}",
                atom_list: Enum.join(atom_list, ", "),
                value: value
              ]
              | errors
            ]
          end
      end)

    case errors do
      [] -> {:ok, value}
      errors -> {:error, errors}
    end
  end

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  @impl true
  def cast_input(value, _) when is_atom(value) do
    {:ok, value}
  end

  def cast_input("", _), do: {:ok, nil}

  def cast_input(value, _) when is_binary(value) do
    {:ok, String.to_existing_atom(value)}
  rescue
    ArgumentError ->
      :error
  end

  def cast_input(_value, _), do: :error

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) when is_atom(value) do
    {:ok, value}
  end

  def cast_stored(value, _) when is_binary(value) do
    {:ok, String.to_existing_atom(value)}
  rescue
    ArgumentError ->
      :error
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) when is_atom(value) do
    {:ok, to_string(value)}
  end

  def dump_to_native(_, _), do: :error
end

import Ash.Type.Comparable

defcomparable left :: BitString, right :: Atom do
  Comp.compare(left, to_string(right))
end
