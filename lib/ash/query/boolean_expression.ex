defmodule Ash.Query.BooleanExpression do
  @moduledoc "Represents a boolean expression"
  @dialyzer {:nowarn_function, optimized_new: 4}

  alias Ash.Query.Operator.{Eq, In, NotEq}
  alias Ash.Query.Ref

  defstruct [:op, :left, :right]

  def new(_, nil, nil), do: nil
  def new(_, left, nil), do: left
  def new(_, nil, right), do: right

  def new(op, left, right) do
    %__MODULE__{op: op, left: left, right: right}
  end

  def optimized_new(op, left, right, current_op \\ :and)
  def optimized_new(_, nil, nil, _), do: nil
  def optimized_new(:and, false, _, _), do: false
  def optimized_new(:and, _, false, _), do: false
  def optimized_new(:or, true, _, _), do: true
  def optimized_new(:or, _, true, _), do: true
  def optimized_new(_, nil, right, _), do: right
  def optimized_new(_, left, nil, _), do: left

  def optimized_new(
        op,
        %__MODULE__{op: op} = left_expr,
        %__MODULE__{
          op: op,
          left: left,
          right: right
        },
        op
      ) do
    optimized_new(op, optimized_new(op, left_expr, left, op), right, op)
  end

  def optimized_new(op, %__MODULE__{} = left, %__MODULE__{} = right, _) do
    do_new(op, left, right)
  end

  def optimized_new(op, left, %__MODULE__{} = right, current_op) do
    optimized_new(op, right, left, current_op)
  end

  def optimized_new(op, %In{} = left, %NotEq{} = right, current_op) do
    optimized_new(op, right, left, current_op)
  end

  def optimized_new(op, %In{} = left, %Eq{} = right, current_op) do
    optimized_new(op, right, left, current_op)
  end

  def optimized_new(op, %Eq{right: %Ref{}} = left, right, _) do
    do_new(op, left, right)
  end

  def optimized_new(op, left, %Eq{right: %Ref{}} = right, _) do
    do_new(op, left, right)
  end

  def optimized_new(op, %NotEq{right: %Ref{}} = left, right, _) do
    do_new(op, left, right)
  end

  def optimized_new(op, left, %NotEq{right: %Ref{}} = right, _) do
    do_new(op, left, right)
  end

  def optimized_new(
        :or,
        %Eq{left: left, right: value},
        %In{left: left, right: %{__struct__: MapSet} = mapset} = right,
        _
      ) do
    %{right | right: MapSet.put(mapset, value)}
  end

  def optimized_new(
        :or,
        %NotEq{left: left, right: value},
        %In{left: left, right: %{__struct__: MapSet} = mapset} = right,
        _
      ) do
    without = MapSet.delete(mapset, value)

    case MapSet.size(without) do
      0 -> false
      1 -> %Eq{left: left, right: Enum.at(without, 0)}
      _ -> %{right | right: without}
    end
  end

  def optimized_new(
        :and,
        %Eq{left: left, right: value} = left_expr,
        %In{left: left, right: %{__struct__: MapSet} = mapset},
        _
      ) do
    if MapSet.member?(mapset, value) do
      left_expr
    else
      false
    end
  end

  def optimized_new(
        :and,
        %NotEq{left: left, right: value},
        %In{left: left, right: %{__struct__: MapSet} = mapset} = right_expr,
        _
      ) do
    if MapSet.member?(mapset, value) do
      false
    else
      right_expr
    end
  end

  def optimized_new(
        op,
        %NotEq{} = left,
        %Eq{} = right,
        current_op
      ) do
    optimized_new(op, right, left, current_op)
  end

  def optimized_new(
        :or,
        %Eq{left: left, right: left_value},
        %Eq{left: left, right: right_value},
        _
      ) do
    %In{left: left, right: MapSet.new([left_value, right_value])}
  end

  def optimized_new(
        :or,
        %NotEq{left: left, right: left_value},
        %Eq{left: left, right: right_value} = right_expr,
        _
      )
      when left_value != right_value do
    right_expr
  end

  def optimized_new(
        :or,
        %NotEq{left: left, right: left_value},
        %Eq{left: left, right: right_value},
        _
      )
      when left_value == right_value do
    true
  end

  def optimized_new(
        :and,
        %Eq{left: left, right: left_value} = left_expr,
        %Eq{left: left, right: right_value},
        _
      ) do
    if left_value == right_value do
      left_expr
    else
      false
    end
  end

  def optimized_new(
        :and,
        %NotEq{left: left, right: left_value} = left_expr,
        %NotEq{left: left, right: right_value},
        _
      )
      when left_value == right_value do
    left_expr
  end

  def optimized_new(
        :or,
        %In{left: left, right: left_values},
        %In{left: left, right: right_values} = right,
        _
      ) do
    %{right | right: MapSet.union(left_values, right_values)}
  end

  def optimized_new(
        :and,
        %In{left: left, right: left_values},
        %In{left: left, right: right_values} = right,
        _
      ) do
    intersection = MapSet.intersection(left_values, right_values)

    case MapSet.size(intersection) do
      0 -> false
      1 -> %Eq{left: left, right: Enum.at(intersection, 0)}
      _ -> %{right | right: intersection}
    end
  end

  def optimized_new(
        op,
        %__MODULE__{op: op, left: left, right: right} = left_expr,
        right_expr,
        op
      ) do
    case right_expr do
      %In{} = in_op ->
        with {:left, nil} <- {:left, Ash.Filter.find(left, &simplify?(&1, in_op))},
             {:right, nil} <- {:right, Ash.Filter.find(right, &simplify?(&1, in_op))} do
          do_new(:or, left_expr, in_op)
        else
          {:left, _} ->
            %{left_expr | left: optimized_new(:or, left, in_op)}

          {:right, _} ->
            %{left_expr | right: optimized_new(:or, right, in_op)}
        end

      %Eq{} = eq_op ->
        with {:left, nil} <- {:left, Ash.Filter.find(left, &simplify?(&1, eq_op))},
             {:right, nil} <- {:right, Ash.Filter.find(right, &simplify?(&1, eq_op))} do
          do_new(:or, left_expr, eq_op)
        else
          {:left, _} ->
            %{left_expr | left: optimized_new(:or, left, eq_op)}

          {:right, _} ->
            %{left_expr | right: optimized_new(:or, right, eq_op)}
        end

      _ ->
        do_new(op, left_expr, right_expr)
    end
  end

  def optimized_new(op, left, right, _) do
    # TODO: more optimization passes!
    # Remove predicates that are on both sides of an `and`
    # if a predicate is on both sides of an `or`, lift it to an `and`
    do_new(op, left, right)
  end

  defp simplify?(%NotEq{} = left, %In{} = right), do: simplify?(right, left)
  defp simplify?(%NotEq{} = left, %Eq{} = right), do: simplify?(right, left)
  defp simplify?(%Eq{} = left, %In{} = right), do: simplify?(right, left)

  defp simplify?(%Eq{right: %Ref{}}, _), do: false
  defp simplify?(_, %Eq{right: %Ref{}}), do: false
  defp simplify?(%Eq{left: left}, %Eq{left: left}), do: true

  defp simplify?(%NotEq{right: %Ref{}}, _), do: false
  defp simplify?(_, %NotEq{right: %Ref{}}), do: false
  defp simplify?(%NotEq{left: left}, %NotEq{left: left}), do: true

  defp simplify?(
         %Eq{left: left},
         %In{left: left, right: %MapSet{}}
       ),
       do: true

  defp simplify?(
         %NotEq{left: left},
         %In{left: left, right: %MapSet{}}
       ),
       do: true

  defp simplify?(
         %Eq{left: left},
         %NotEq{left: left, right: %MapSet{}}
       ),
       do: true

  defp simplify?(_, _), do: false

  defp do_new(op, left, right) do
    if left == right do
      left
    else
      %__MODULE__{
        op: op,
        left: left,
        right: right
      }
    end
  end
end

defimpl Inspect, for: Ash.Query.BooleanExpression do
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
