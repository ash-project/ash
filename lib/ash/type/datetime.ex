defmodule Ash.Type.DateTime do
  @moduledoc """
  Represents a datetime, with configurable precision and timezone.
  """

  @beginning_of_day Time.new!(0, 0, 0)

  @constraints [
    precision: [
      type: {:one_of, [:microsecond, :second]},
      default: :second
    ],
    timezone: [
      type: {:one_of, [:utc]},
      default: :utc
    ]
  ]

  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  @impl true
  def matches_type?(%DateTime{}, _), do: true
  def matches_type?(_, _), do: false

  @impl true
  def init(constraints) do
    {precision, constraints} = Keyword.pop(constraints, :precision)
    precision = precision || :second
    {:ok, [{:precision, precision} | constraints]}
  end

  @impl true
  @spec storage_type(nonempty_maybe_improper_list()) :: any()
  def storage_type([{:precision, :microsecond} | _]) do
    :utc_datetime_usec
  end

  def storage_type(_constraints) do
    :utc_datetime
  end

  @impl true
  def generator(_constraints) do
    # Waiting on blessed date/datetime generators in stream data
    # https://github.com/whatyouhide/stream_data/pull/161/files
    StreamData.constant(DateTime.utc_now())
  end

  @impl true
  def cast_input(%Date{} = date, constraints) do
    case DateTime.new(date, @beginning_of_day) do
      {:ok, value} ->
        cast_input(value, constraints)

      _ ->
        {:error, "Date could not be converted to datetime"}
    end
  end

  def cast_input(
        %DateTime{microsecond: {_, _} = microseconds} = datetime,
        [{:precision, :second} | _] = constraints
      )
      when microseconds != {0, 0} do
    cast_input(%{datetime | microsecond: {0, 0}}, constraints)
  end

  def cast_input(
        %DateTime{microsecond: {0, 0}} = datetime,
        [{:precision, :microsecond} | _] = constraints
      ) do
    cast_input(%{datetime | microsecond: {0, 6}}, constraints)
  end

  def cast_input(
        %DateTime{microsecond: nil} = datetime,
        [{:precision, :microsecond} | _] = constraints
      ) do
    cast_input(%{datetime | microsecond: {0, 6}}, constraints)
  end

  def cast_input(value, constraints) do
    case Ecto.Type.cast(storage_type(constraints), value) do
      :error ->
        case Ash.Type.cast_input(:date, value, []) do
          {:ok, date} ->
            cast_input(date, constraints)

          _ ->
            {:error, "Could not cast input to datetime"}
        end

      {:ok, value} ->
        {:ok, value}
    end
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

defcomparable left :: DateTime, right :: DateTime do
  DateTime.compare(left, right)
end
