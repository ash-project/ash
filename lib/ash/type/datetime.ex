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
  def storage_type(constraints) do
    case constraints[:precision] || :microsecond do
      :microsecond ->
        :utc_datetime_usec

      :second ->
        :utc_datetime
    end
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

  def cast_input(value, constraints) do
    Ecto.Type.cast(storage_type(constraints), value)
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
