defmodule Ash.Filter.Expression do
  @moduledoc "Represents a boolean expression"

  defstruct [:op, :left, :right]

  def new(_, nil, nil), do: nil
  def new(:and, false, _), do: false
  def new(:and, _, false), do: false
  def new(:or, true, _), do: true
  def new(:or, _, true), do: true
  def new(_, nil, right), do: right
  def new(_, left, nil), do: left

  def new(op, left, right) do
    # TODO: more optimization passes!
    # Remove predicates that are on both sides of an `and`
    # if a predicate is on both sides of an `or`, lift it to an `and`
    if left == right do
      left
    else
      %__MODULE__{op: op, left: left, right: right}
    end
  end
end

defimpl Inspect, for: Ash.Filter.Expression do
  import Inspect.Algebra

  def inspect(
        %{left: left, right: right, op: op},
        opts
      ) do
    container_type = container_type(opts)

    opts = put_container_type(opts, op)

    if container_type && op != container_type do
      concat(["(", to_doc(left, opts), " ", to_string(op), " ", to_doc(right, opts), ")"])
    else
      concat([to_doc(left, opts), " ", to_string(op), " ", to_doc(right, opts)])
    end
  end

  # custom options not available before Elixir 1.9
  defp container_type(%{custom_options: options}), do: options[:container_type]
  defp container_type(_), do: nil

  defp put_container_type(opts, container_type) do
    custom_options = apply(Map, :get, [opts, :custom_options])

    apply(Map, :put, [
      opts,
      :custom_options,
      Keyword.put(custom_options, :container_type, container_type)
    ])

    # above version required to avoid dialyzer warnings on lack of custom_options in pre-1.9 elixir
    # %{opts | custom_options: Keyword.put(opts.custom_options, :container_type, container_type)}
  end
end
