defmodule Ash.Query.Function.If do
  @moduledoc """
  If predicate is truthy, then the second argument is returned, otherwise the third.
  """
  use Ash.Query.Function, name: :if, no_inspect?: true
  import Ash.Expr, only: [expr?: 1]

  def args, do: [[:boolean, :any], [:boolean, :any, :any]]

  def evaluate_nil_inputs?, do: true

  def new([condition, block]) do
    args =
      if Keyword.keyword?(block) && Keyword.has_key?(block, :do) do
        if Keyword.has_key?(block, :else) do
          [condition, block[:do], block[:else]]
        else
          [condition, block[:do], nil]
        end
      else
        [condition, block, nil]
      end

    new(args)
  end

  def new([true, block, _else_block]), do: {:ok, block}
  def new([false, _block, else_block]), do: {:ok, else_block}
  def new([nil, _block, else_block]), do: {:ok, else_block}

  def new([condition, block, else_block]) do
    super([condition, block, else_block])
  end

  def can_return_nil?(%{arguments: [_ | rest]}) do
    Enum.any?(rest, &Ash.Expr.can_return_nil?/1)
  end

  def evaluate(%{arguments: [true, when_true, _]}),
    do: {:known, when_true}

  def evaluate(%{arguments: [false, _, when_false]}),
    do: {:known, when_false}

  def evaluate(%{arguments: [nil, _, when_false]}),
    do: {:known, when_false}

  def evaluate(%{arguments: [_, when_true, _]}), do: {:known, when_true}

  def partial_evaluate(%{arguments: [false, _, when_false]}),
    do: {:ok, when_false}

  def partial_evaluate(%{arguments: [nil, _, when_false]}),
    do: {:ok, when_false}

  def partial_evaluate(%{arguments: [condition, when_true, _]} = fun) do
    if expr?(condition) do
      {:ok, fun}
    else
      {:ok, when_true}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{arguments: [condition, when_true, when_false]}, opts) do
      with {:ok, required_expr} <- is_thing_is_nil(condition),
           true <- required_expr == when_false,
           true <- is_required_error(when_true) do
        concat(["required!(", to_doc(required_expr, opts), ")"])
      else
        _ ->
          {conds, final} = extract_cases(when_false)

          case conds do
            [] ->
              if is_nil(final) do
                concat([
                  nest(
                    concat([
                      group(concat(["if ", to_doc(condition, opts), " do"])),
                      line(),
                      to_doc(when_true, opts)
                    ]),
                    2
                  ),
                  line(),
                  "end"
                ])
              else
                concat([
                  nest(
                    concat([
                      group(concat(["if ", to_doc(condition, opts), " do"])),
                      line(),
                      to_doc(when_true, opts)
                    ]),
                    2
                  ),
                  line(),
                  "else",
                  nest(
                    concat([
                      line(),
                      to_doc(when_false, opts)
                    ]),
                    2
                  ),
                  line(),
                  "end"
                ])
              end

            conds ->
              conds = [{condition, when_true} | conds ++ [{true, when_true}]]

              concat(
                [
                  "cond do"
                ] ++
                  Enum.flat_map(conds, fn {cond, when_true} ->
                    [
                      nest(concat([line(), to_doc(cond, opts), " ->"]), 2),
                      nest(concat([line(), to_doc(when_true, opts)]), 4)
                    ]
                  end) ++
                  [
                    line(),
                    "end"
                  ]
              )
          end
      end
    end

    defp is_thing_is_nil(%{name: :is_nil, args: [thing]}) do
      {:ok, thing}
    end

    defp is_thing_is_nil(%{name: :is_nil, arguments: [thing]}) do
      {:ok, thing}
    end

    defp is_thing_is_nil(%{operator: :is_nil, left: thing, right: true}) do
      {:ok, thing}
    end

    defp is_thing_is_nil(_) do
      :error
    end

    defp is_required_error(%{name: :error, args: [Ash.Error.Changes.Required | _]}), do: true
    defp is_required_error(%{name: :error, arguments: [Ash.Error.Changes.Required | _]}), do: true

    defp is_required_error(_), do: false

    defp extract_cases(other, list_acc \\ [])

    defp extract_cases(%{name: :if, arguments: [condition, when_true, when_false]}, list_acc) do
      extract_cases(when_false, [{condition, when_true} | list_acc])
    end

    defp extract_cases(%{name: :if, args: [condition, blocks]}, list_acc) do
      {:ok, if_func} = Ash.Query.Function.If.new([condition, blocks])
      extract_cases(if_func, list_acc)
    end

    defp extract_cases(other, list_acc) do
      {Enum.reverse(list_acc), other}
    end
  end
end
