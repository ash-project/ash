defmodule Ash.Type.UtcDatetime do
  @moduledoc """
  Represents a utc datetime

  A builtin type that can be referenced via `:utc_datetime`
  """
  use Ash.Type

  @beginning_of_day Time.new!(0, 0, 0)

  @impl true
  def storage_type(_), do: :utc_datetime

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

  def cast_input(value, _) do
    Ecto.Type.cast(:utc_datetime, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_binary(value) do
    cast_input(value, constraints)
  end

  def cast_stored(value, _) do
    Ecto.Type.load(:utc_datetime, value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:utc_datetime, value)
  end
end
