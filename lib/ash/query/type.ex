defmodule Ash.Query.Type do
  @moduledoc false

  def try_cast(list, {:array, type}) do
    if Enumerable.impl_for(list) do
      list
      |> Enum.reduce_while({:ok, []}, fn value, {:ok, list} ->
        case try_cast(value, type) do
          {:ok, casted} -> {:cont, {:ok, [casted | list]}}
          :error -> {:halt, :error}
        end
      end)
      |> case do
        :error -> :error
        {:ok, val} -> {:ok, Enum.reverse(val)}
      end
    else
      :error
    end
  end

  def try_cast(%Ash.CiString{} = str, :string), do: {:ok, str}
  def try_cast(value, :number) when is_number(value), do: {:ok, value}
  def try_cast(value, :any), do: {:ok, value}

  def try_cast(value, :number) do
    case Ash.Type.cast_input(:decimal, value) do
      {:ok, value} ->
        {:ok, value}

      _ ->
        case Ash.Type.cast_input(:integer, value) do
          {:ok, value} ->
            {:ok, value}

          _ ->
            :error
        end
    end
  end

  def try_cast(value, type) do
    case Ash.Type.cast_input(type, value) do
      {:ok, value} ->
        {:ok, value}

      _ ->
        :error
    end
  end
end
