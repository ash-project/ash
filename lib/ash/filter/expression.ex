defmodule Ash.Filter.Expression do
  defstruct [:op, :left, :right]

  def new(_, nil, nil), do: nil
  def new(_, nil, right), do: right
  def new(_, left, nil), do: left

  def new(op, %__MODULE__{op: left_op} = left, %__MODULE__{op: op} = right) when left_op != op do
    %__MODULE__{op: op, left: right, right: left}
  end

  def new(op, left, right) do
    %__MODULE__{op: op, left: left, right: right}
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

  defp container_type(opts) do
    opts.custom_options[:container_type]
  end

  defp put_container_type(opts, container_type) do
    %{opts | custom_options: Keyword.put(opts.custom_options, :container_type, container_type)}
  end
end
