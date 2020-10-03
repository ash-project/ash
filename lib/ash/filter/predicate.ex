defmodule Ash.Filter.Predicate do
  @moduledoc """
  Represents a filter predicate

  The `embedded` flag is set to true for predicates that are present in the `base_filter`.
  Datalayers may optionally use this information.
  """

  defstruct [:resource, :attribute, :relationship_path, :predicate, :value, embedded: false]

  @type predicate :: struct

  @type comparison ::
          :unknown
          | :right_excludes_left
          | :left_excludes_right
          | :right_includes_left
          | :left_includes_right
          | :mutually_inclusive
          # A simplification value for the right term
          | {:simplify, term}
          | {:simplify, term, term}

  @type t :: %__MODULE__{
          attribute: Ash.attribute(),
          relationship_path: list(atom),
          predicate: predicate
        }

  @callback compare(predicate(), predicate()) :: comparison()

  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Filter.Predicate

      @impl true
      def compare(_, _), do: :unknown

      defoverridable compare: 2
    end
  end

  def match?(predicate, value, type) do
    predicate.__struct__.match?(predicate, value, type)
  end

  def compare(same, same), do: :mutually_inclusive

  def compare(left, right) do
    if left.__struct__ == right.__struct__ do
      with {:right_to_left, :unknown} <- {:right_to_left, left.__struct__.compare(left, right)},
           {:left_to_right, :unknown} <- {:left_to_right, right.__struct__.compare(left, right)} do
        :mutually_exclusive
      else
        {:right_to_left, {:simplify, left, _}} -> {:simplify, left}
        {:left_to_right, {:simplify, _, right}} -> {:simplify, right}
        {_, other} -> other
      end
    else
      with {:right_to_left, :unknown} <- {:right_to_left, left.__struct__.compare(left, right)},
           {:right_to_left, :unknown} <- {:right_to_left, right.__struct__.compare(left, right)},
           {:left_to_right, :unknown} <- {:left_to_right, right.__struct__.compare(left, right)},
           {:left_to_right, :unknown} <- {:left_to_right, left.__struct__.compare(left, right)} do
        :mutually_exclusive
      else
        {:right_to_left, {:simplify, left, _}} -> {:simplify, left}
        {:left_to_right, {:simplify, _, right}} -> {:simplify, right}
        {_, other} -> other
      end
    end
  end
end
