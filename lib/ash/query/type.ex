defmodule Ash.Query.Type do
  @moduledoc false

  def try_cast(value, type, constraints \\ [])

  def try_cast(value, nil, _constraints), do: {:ok, value}

  def try_cast(list, {:array, type}, constraints) do
    if Enumerable.impl_for(list) do
      list
      |> Enum.reduce_while({:ok, []}, fn value, {:ok, list} ->
        case try_cast(value, type, constraints[:items] || []) do
          {:ok, casted} -> {:cont, {:ok, [casted | list]}}
          :error -> {:halt, :error}
        end
      end)
      |> case do
        :error -> :error
        {:ok, val} -> {:ok, Enum.reverse(val)}
      end
    else
      if is_nil(list) do
        {:ok, list}
      else
        :error
      end
    end
  end

  def try_cast(%Ash.CiString{} = str, :string, _constraints), do: {:ok, str}
  def try_cast(value, :number, _constraints) when is_number(value), do: {:ok, value}
  def try_cast(value, :any, _constraints), do: {:ok, value}

  def try_cast(value, :number, constraints) do
    case Ash.Type.cast_input(:decimal, value, constraints) do
      {:ok, value} ->
        {:ok, value}

      _ ->
        case Ash.Type.cast_input(:integer, value, constraints) do
          {:ok, value} ->
            {:ok, value}

          _ ->
            :error
        end
    end
  end

  def try_cast(value, type, constraints) do
    if Ash.Expr.expr?(value) do
      :error
    else
      case Ash.Type.cast_input(type, value, constraints) do
        {:ok, value} ->
          {:ok, value}

        _ ->
          :error
      end
    end
  end
end
