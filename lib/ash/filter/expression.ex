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

  def new(op, %__MODULE__{op: left_op} = left, %__MODULE__{op: op} = right) when left_op != op do
    [left, right] = Enum.sort([left, right])
    optimize(%__MODULE__{op: op, left: right, right: left})
  end

  def new(op, left, right) do
    [left, right] = Enum.sort([left, right])
    optimize(%__MODULE__{op: op, left: left, right: right})
  end

  defp optimize(%__MODULE__{op: op, left: left, right: right}) do
    Ash.Filter.reduce(right, left, fn expression, other_expr ->
      case do_optimize(expression, other_expr) do
        nil ->
          right

        true ->
          if op == :or do
            true
          else
            right
          end

        false ->
          if op == :and do
            false
          else
            right
          end

        new_left ->
          Ash.Filter.reduce()
      end
    end)
  end

  defp do_optimize(expr, %__MODULE__{op: :or, left: left, right: right}) do
    if has_expression?(left, expr) && has_expression?(right, expr) do
      nil
    else
      expr
    end
  end

  defp has_expression?(expr, candidate) do
    false
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
  defp container_type(%{custom_options: %{container_type: container_type}}), do: container_type
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
