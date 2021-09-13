defmodule Ash.Type.Interval do
  @intervals ~w(year month week day hour minute second millisecond microsecond)a
  @string_intervals Enum.map(@intervals, &to_string/1)
  @string_intervals_to_interval Enum.map(@intervals, &{&1, to_string(&1)})
  @moduledoc """
  An interval of time, primarily meant to be used in expression functions

  Valid intervals are (as strings or atoms): #{inspect(@intervals)}
  """
  use Ash.Type

  @impl true
  def storage_type, do: :string

  @impl true
  def cast_input(value, _) when value in @intervals do
    {:ok, value}
  end

  def cast_input(value, _) when value in @string_intervals do
    {:ok, @string_intervals_to_interval[value]}
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) when value in @intervals do
    {:ok, value}
  end

  def cast_stored(value, _) when value in @string_intervals do
    {:ok, @string_intervals_to_interval[value]}
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) when is_atom(value) do
    {:ok, to_string(value)}
  end

  def dump_to_native(_, _), do: :error
end
