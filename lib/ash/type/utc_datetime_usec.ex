defmodule Ash.Type.UtcDatetimeUsec do
  @moduledoc """
  Represents a utc datetime with microsecond precision.

  A builtin type that can be referenced via `:utc_datetime_usec`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :utc_datetime_usec

  @impl true
  def generator(_constraints) do
    # Waiting on blessed date/datetime generators in stream data
    # https://github.com/whatyouhide/stream_data/pull/161/files
    StreamData.constant(DateTime.utc_now())
  end

  @impl true
  def cast_input(value, _) do
    Ecto.Type.cast(:utc_datetime_usec, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_binary(value) do
    cast_input(value, constraints)
  end

  def cast_stored(value, _) do
    case Ecto.Type.load(:utc_datetime_usec, value) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        if is_binary(value) do
          case DateTime.from_iso8601(value) do
            {:ok, value, _offset} ->
              {:ok, value}

            _ ->
              :error
          end
        else
          :error
        end
    end
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:utc_datetime_usec, value)
  end
end
