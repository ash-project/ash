defmodule Ash.Query.BooleanExpression do
  @moduledoc "Represents a boolean expression"
  @dialyzer {:nowarn_function, optimized_new: 4}

  defstruct [:op, :left, :right]

  alias Ash.Query.Operator.{Eq, In, NotEq}
  alias Ash.Query.Ref

  def new(_, nil, nil), do: nil
  def new(_, left, nil), do: left
  def new(_, nil, right), do: right

  def new(op, left, right) do
    %__MODULE__{op: op, left: left, right: right}
  end

  # In many cases we could actually just return `true/false` directly because we know
  # statements are contradictions. However, that would likely confuse users. For example:
  # `Ash.Query.filter(Resource, x == 1 and x in [2, 3])` becomes `#Ash.Query<filter: false>`
  # We may want to go down this route some day, but for now we simply use this to combine
  # statements where possible, which helps with authorization logic that leverages the query
  # For example, `x in [1, 2] or x == 3` becomes `x in [1, 2, 3]`, and `x in [1, 2, 3] and x != 1`
  # becomes `x in [2, 3]`
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
        %Eq{left: left, right: value} = left_op,
        %In{left: left, right: %{__struct__: MapSet} = mapset} = right,
        _
      ) do
    if can_optimize?(value) do
      %{right | right: MapSet.put(mapset, value)}
    else
      do_new(:or, left_op, right)
    end
  end

  def optimized_new(
        :or,
        %NotEq{left: left, right: value} = left_op,
        %In{left: left, right: %{__struct__: MapSet} = mapset} = right,
        _
      ) do
    if can_optimize?(value) do
      without = MapSet.delete(mapset, value)

      case MapSet.size(without) do
        0 ->
          do_new(:or, left_op, right)

        1 ->
          %Eq{left: left, right: Enum.at(without, 0)}

        _ ->
          %{right | right: without}
      end
    else
      do_new(:or, left_op, right)
    end
  end

  def optimized_new(
        :and,
        %Eq{left: left, right: value} = left_expr,
        %In{left: left, right: %{__struct__: MapSet} = mapset} = right,
        _
      ) do
    if can_optimize?(value) do
      if MapSet.member?(mapset, value) do
        left_expr
      else
        do_new(:and, left_expr, right)
      end
    else
      do_new(:and, left_expr, right)
    end
  end

  def optimized_new(
        :and,
        %NotEq{left: left, right: value} = left_op,
        %In{left: left, right: %{__struct__: MapSet} = mapset} = right_expr,
        _
      ) do
    if can_optimize?(value) do
      %{right_expr | right: MapSet.delete(mapset, value)}
    else
      do_new(:and, left_op, right_expr)
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
        %Eq{left: left, right: left_value} = left_expr,
        %Eq{left: left, right: right_value} = right_expr,
        _
      ) do
    if can_optimize?(left_value) && can_optimize?(right_value) do
      %In{left: left, right: MapSet.new([left_value, right_value])}
    else
      do_new(:or, left_expr, right_expr)
    end
  end

  def optimized_new(
        :and,
        %Eq{left: left, right: left_value} = left_expr,
        %Eq{left: left, right: right_value} = right_expr,
        _
      ) do
    if can_optimize?(left_value) && can_optimize?(right_value) && left_value == right_value do
      left_expr
    else
      do_new(:and, left_expr, right_expr)
    end
  end

  def optimized_new(
        :and,
        %NotEq{left: left, right: left_value} = left_expr,
        %NotEq{left: left, right: right_value} = right_expr,
        _
      ) do
    if can_optimize?(left_value) && can_optimize?(right_value) && left_value == right_value do
      left_value
    else
      do_new(:and, left_expr, right_expr)
    end
  end

  def optimized_new(
        :or,
        %In{left: left, right: %{__struct__: MapSet} = left_values},
        %In{left: left, right: %{__struct__: MapSet} = right_values} = right_expr,
        _
      ) do
    %{right_expr | right: MapSet.union(left_values, right_values)}
  end

  def optimized_new(
        :and,
        %In{left: left, right: left_values} = left_expr,
        %In{left: left, right: right_values} = right_expr,
        _
      ) do
    if can_optimize?(left_values) && can_optimize?(right_values) do
      intersection = MapSet.intersection(left_values, right_values)

      case MapSet.size(intersection) do
        0 -> do_new(:and, left_expr, right_expr)
        1 -> %Eq{left: left, right: Enum.at(intersection, 0)}
        _ -> %{right_expr | right: intersection}
      end
    else
      do_new(:and, left_expr, right_expr)
    end
  end

  def optimized_new(
        op,
        %__MODULE__{left: left, right: right} = left_expr,
        right_expr,
        op
      ) do
    case right_expr do
      %In{} = in_op ->
        with {:left, nil} <- {:left, Ash.Filter.find(left, &simplify?(&1, in_op))},
             {:right, nil} <- {:right, Ash.Filter.find(right, &simplify?(&1, in_op))} do
          do_new(op, left_expr, in_op)
        else
          {:left, _} ->
            %{left_expr | left: optimized_new(op, left, in_op)}

          {:right, _} ->
            %{left_expr | right: optimized_new(op, right, in_op)}
        end

      %Eq{} = eq_op ->
        with {:left, nil} <- {:left, Ash.Filter.find(left, &simplify?(&1, eq_op))},
             {:right, nil} <- {:right, Ash.Filter.find(right, &simplify?(&1, eq_op))} do
          do_new(op, left_expr, eq_op)
        else
          {:left, _} ->
            %{left_expr | left: optimized_new(op, left, eq_op)}

          {:right, _} ->
            %{left_expr | right: optimized_new(op, right, eq_op)}
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

  defp simplify?(%Eq{left: left, right: left_right}, %Eq{left: left, right: right_right}) do
    can_optimize?(left_right) && can_optimize?(right_right)
  end

  defp simplify?(%NotEq{right: %Ref{}}, _), do: false
  defp simplify?(_, %NotEq{right: %Ref{}}), do: false

  defp simplify?(%NotEq{left: left, right: left_right}, %NotEq{left: left, right: right_right}) do
    can_optimize?(left_right) && can_optimize?(right_right)
  end

  defp simplify?(
         %Eq{left: left, right: left_right},
         %In{left: left, right: %MapSet{}}
       ) do
    can_optimize?(left_right)
  end

  defp simplify?(
         %NotEq{left: left, right: left_right},
         %In{left: left, right: %MapSet{}}
       ) do
    can_optimize?(left_right)
  end

  defp simplify?(
         %Eq{left: left, right: left_right},
         %NotEq{left: left, right: %MapSet{}}
       ) do
    can_optimize?(left_right)
  end

  defp simplify?(_, _), do: false

  defp can_optimize?(value) when is_list(value) do
    Enum.all?(value, &can_optimize?/1)
  end

  defp can_optimize?(map) when is_map(map) and not is_struct(map) do
    Enum.all?(map, fn {key, val} ->
      can_optimize?(key) && can_optimize?(val)
    end)
  end

  defp can_optimize?(%{__struct__: MapSet} = mapset) do
    Enum.all?(mapset, &can_optimize?/1)
  end

  defp can_optimize?(value) when is_integer(value) or is_binary(value) do
    true
  end

  defp can_optimize?(_), do: false

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
