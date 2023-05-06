defmodule Ash.Calculation.Function do
  @moduledoc false
  use Ash.Calculation

  def calculate(results, [fun: fun], context) do
    Enum.reduce_while(results, {:ok, []}, fn result, {:ok, acc} ->
      case apply_fun(fun, result, context) do
        {:ok, result} -> {:cont, {:ok, [result | acc]}}
        {:error, error} -> {:halt, {:error, error}}
        result -> {:cont, {:ok, [result | acc]}}
      end
    end)
    |> case do
      {:ok, results} -> {:ok, Enum.reverse(results)}
      error -> error
    end
  end

  defp apply_fun({m, f, a}, result, context) do
    apply(m, f, [result, context | a])
  end

  defp apply_fun(fun, result, context) do
    fun.(result, context)
  end
end
