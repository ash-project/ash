defmodule Ash.DataLayer.Ets.FilterCompiler do
  @moduledoc """
  Attempts to convert an `Ash.DataLayer.Ets.Query` into a match specification
  guard function.
  """

  # Here we assume that the actual record is stored in the `$2` match variable.
  @field_assumption :"$2"

  require Logger

  @doc """
  Attempts to convert the filter into a "guard function" tuple.any()

  Returns `:error` for unsupported filters.
  """
  @spec compile(Ash.Filter.t()) :: {:ok, tuple} | :error
  def compile(filter) when is_struct(filter, Ash.Query), do: compile(filter.filter)

  def compile(filter) when is_struct(filter, Ash.Filter.Simple.Not) do
    with {:ok, guard} <- compile(filter.predicate) do
      {:ok, {:not, guard}}
    end
  end

  def compile(filter) when is_struct(filter, Ash.Filter.Simple) do
    filter.predicates
    |> Enum.reduce_while(:error, fn
      predicate, :error ->
        case compile(predicate) do
          {:ok, guard} -> {:cont, {:ok, guard}}
          :error -> {:halt, :error}
        end

      predicate, {:ok, parent} ->
        case compile(predicate) do
          {:ok, guard} -> {:cont, {:ok, {:and, guard, parent}}}
          :error -> {:halt, :error}
        end
    end)
  end

  def compile(filter) when is_struct(filter, Ash.Filter), do: compile(filter.expression)

  def compile(filter) when is_struct(filter, Ash.Query.Operator.Eq) do
    with {:ok, lhs} <- compile(filter.left),
         {:ok, rhs} <- compile(filter.right) do
      {:ok, {:==, lhs, rhs}}
    end
  end

  def compile(filter) when is_struct(filter, Ash.Query.Operator.NotEq) do
    with {:ok, lhs} <- compile(filter.left),
         {:ok, rhs} <- compile(filter.right) do
      {:ok, {:"/=", lhs, rhs}}
    end
  end

  def compile(filter) when is_struct(filter, Ash.Query.Ref) when filter.relationship_path == [],
    do: {:ok, {:map_get, filter.attribute.name, @field_assumption}}

  def compile(filter) when is_binary(filter), do: {:ok, {:const, filter}}

  def compile(filter) do
    Logger.warning("Unsupported filter #{inspect(filter, structs: false)}")
    :error
  end
end
