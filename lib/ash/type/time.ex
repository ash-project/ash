# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Time do
  @constraints [
    precision: [
      type: {:one_of, [:microsecond, :second]},
      default: :second
    ]
  ]
  @moduledoc """
  Represents a time in the database, with a 'second' precision

  A builtin type that can be referenced via `:time`

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def init(constraints) do
    {precision, constraints} = Keyword.pop(constraints, :precision)
    precision = precision || :second
    {:ok, [{:precision, precision} | constraints]}
  end

  @impl true
  @spec storage_type(nonempty_maybe_improper_list()) :: any()
  def storage_type([{:precision, :microsecond} | _]) do
    :time_usec
  end

  def storage_type(_constraints) do
    :time
  end

  @impl true
  def generator(_constraints) do
    # Waiting on blessed date/datetime generators in stream data
    # https://github.com/whatyouhide/stream_data/pull/161/files
    StreamData.constant(Time.utc_now())
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(
        %Time{microsecond: {_, _} = microseconds} = time,
        [{:precision, :second} | _] = constraints
      )
      when microseconds != {0, 0} do
    cast_input(%{time | microsecond: {0, 0}}, constraints)
  end

  def cast_input(
        %Time{microsecond: {0, 0}} = time,
        [{:precision, :microsecond} | _] = constraints
      ) do
    cast_input(%{time | microsecond: {0, 6}}, constraints)
  end

  def cast_input(
        %Time{microsecond: nil} = time,
        [{:precision, :microsecond} | _] = constraints
      ) do
    cast_input(%{time | microsecond: {0, 6}}, constraints)
  end

  def cast_input(value, constraints) do
    Ecto.Type.cast(storage_type(constraints), value)
  end

  @impl true
  def matches_type?(%Time{}, _), do: true
  def matches_type?(_, _), do: false

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_binary(value) do
    cast_input(value, constraints)
  end

  def cast_stored(value, constraints) do
    Ecto.Type.load(storage_type(constraints), value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, constraints) do
    Ecto.Type.dump(storage_type(constraints), value)
  end
end

import Ash.Type.Comparable

defcomparable left :: Time, right :: Time do
  Time.compare(left, right)
end
