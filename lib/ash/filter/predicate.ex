defmodule Ash.Filter.Predicate do
  @moduledoc """
  Represents a predicate which can be simplified and/or compared with other predicates

  Simplification and comparison will need more documentation, but ultimately it
  is the logic that allows us to have a flexible and powerful authorization
  system.
  """

  @type predicate :: struct

  @type comparison ::
          :unknown
          | :right_includes_left
          | :left_includes_right
          | :mutually_inclusive
          | :mutually_exclusive

  @doc "Compare two predicates. If possible, use `c:bulk_compare/1` instead"
  @callback compare(predicate(), predicate()) :: comparison()

  @doc """
  As long as at least one predicate of the type defined in your module,
  (and this callback is implemented), it will be called with all of the
  other predicates present in a filter. The return value is relatively
  complex, but it should be a list of boolean statements. E.g.
  `{op, left, right}` and `{:not, predicate}` (nested as deep as necessary).

  The best way to do it is to find lists of predicates that are mutually
  exclusive or mutually inclusive, and pass those lists into
  `Ash.SatSolver.mutually_exclusive/1` and `Ash.SatSolver.mutually_inclusive/1`
  """
  @callback bulk_compare([predicate()]) :: term

  @doc """
  Simplify to a more primitive statement.

  For example, `x in [1, 2]` simplifies to `x == 1 or x == 2`.
  Simplifying to filter expressions that already have comparisons
  lets you avoid writing that logic for a given predicate.
  """
  @callback simplify(predicate()) :: term

  @optional_callbacks compare: 2, bulk_compare: 1, simplify: 1

  @doc """
  Checks with each predicate module to see if it has a comparison
  with
  """
  def compare(same, same), do: :mutually_inclusive

  def compare(left, right) do
    if :erlang.function_exported(right.__struct__, :compare, 2) do
      if left.__struct__ == right.__struct__ do
        with :unknown <- left.__struct__.compare(left, right) do
          right.__struct__.compare(left, right)
        end
      else
        with :unknown <- left.__struct__.compare(left, right),
             :unknown <- right.__struct__.compare(right, left),
             :unknown <- right.__struct__.compare(left, right) do
          left.__struct__.compare(right, left)
        end
      end
    else
      if :erlang.function_exported(left.__struct__, :compare, 2) do
        left.__struct__.compare(left, right)
      else
        :unknown
      end
    end
  end
end
