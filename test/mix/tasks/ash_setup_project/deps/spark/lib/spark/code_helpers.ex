# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.CodeHelpers do
  @moduledoc """
  Helpers for meta programming around code and code snippets
  """

  @doc """
  Given a section of Elixir AST, generate a hash of the code to help with
  generating unique names.
  """
  @spec code_identifier(Macro.t()) :: binary
  def code_identifier(code) do
    code
    |> strip_meta()
    |> :erlang.term_to_iovec()
    |> :erlang.md5()
    |> Base.encode16()
  end

  @doc false
  @spec strip_meta(Macro.t()) :: Macro.t()
  def strip_meta(code) do
    Macro.prewalk(code, fn
      {foo, _, bar} when is_atom(foo) and is_atom(bar) ->
        {foo, [], nil}

      {foo, _, bar} ->
        {foo, [], bar}

      other ->
        other
    end)
  end

  @doc """
  Copy of `Macro.prewalk/2` w/ a branch accumulator
  """
  def prewalk(ast, fun) when is_function(fun, 1) do
    elem(prewalk(ast, nil, nil, fn x, nil, nil -> {fun.(x), nil} end), 0)
  end

  @doc """
  Copy of `Macro.prewalk/3` w/ a branch accumulator
  """
  def prewalk(ast, acc, branch_acc, fun) when is_function(fun, 3) do
    traverse(ast, acc, branch_acc, fun, fn x, a -> {x, a} end)
  end

  @doc """
  A copy of the corresponding `Macro.traverse` function that has a separate accumulator that only goes *down* each branch, only for `pre`
  """
  def traverse(ast, acc, branch_acc, pre, post)
      when is_function(pre, 3) and is_function(post, 2) do
    {ast, acc, branch_acc} = pre.(ast, acc, branch_acc)
    do_traverse(ast, acc, branch_acc, pre, post)
  end

  defp do_traverse({form, meta, args}, acc, branch_acc, pre, post) when is_atom(form) do
    {args, acc} = do_traverse_args(args, acc, branch_acc, pre, post)
    post.({form, meta, args}, acc)
  end

  defp do_traverse({form, meta, args}, acc, branch_acc, pre, post) do
    {form, acc, branch_acc} = pre.(form, acc, branch_acc)
    {form, acc} = do_traverse(form, acc, branch_acc, pre, post)
    {args, acc} = do_traverse_args(args, acc, branch_acc, pre, post)
    post.({form, meta, args}, acc)
  end

  defp do_traverse({left, right}, acc, branch_acc, pre, post) do
    {left, acc, left_branch_acc} = pre.(left, acc, branch_acc)
    {left, acc} = do_traverse(left, acc, left_branch_acc, pre, post)
    {right, acc, right_branch_acc} = pre.(right, acc, branch_acc)
    {right, acc} = do_traverse(right, acc, right_branch_acc, pre, post)
    post.({left, right}, acc)
  end

  defp do_traverse(list, acc, branch_acc, pre, post) when is_list(list) do
    {list, acc} = do_traverse_args(list, acc, branch_acc, pre, post)
    post.(list, acc)
  end

  defp do_traverse(x, acc, _branch_acc, _pre, post) do
    post.(x, acc)
  end

  defp do_traverse_args(args, acc, _branch_acc, _pre, _post) when is_atom(args) do
    {args, acc}
  end

  defp do_traverse_args(args, acc, branch_acc, pre, post) when is_list(args) do
    :lists.mapfoldl(
      fn x, acc ->
        {x, acc, branch_acc} = pre.(x, acc, branch_acc)
        do_traverse(x, acc, branch_acc, pre, post)
      end,
      acc,
      args
    )
  end

  @doc """
  Lift anonymous and captured functions.

  Acts as an AST transformer to allow these kinds of functions to be added in
  the AST:

  In the case of captured functions, it ensures they are all captured remote
  functions (ie calls with both the module and function name present) - this
  often requires the definition of a new public function on the target module.

  In the case of anonymous functions, it converts them into a new public
  function on the module and returns a (remote) function capture much like that
  of above.
  """
  @spec lift_functions(Macro.t(), atom, Macro.Env.t()) :: Macro.t()
  # (Don't) lift functions of the `&Module.function/arity` format
  # This is a temporary hack to support keyword lists needed by Ash
  def lift_functions(value, key, caller) when is_list(value) do
    if Keyword.keyword?(value) do
      Enum.reduce(value, {[], nil}, fn {k, v}, {keyword, funs} ->
        {v, functions} = lift_functions(v, key, caller)

        funs =
          if functions do
            quote do
              unquote(funs)
              unquote(functions)
            end
          else
            funs
          end

        {[{k, v} | keyword], funs}
      end)
      |> then(fn {keyword, funs} ->
        {Enum.reverse(keyword), funs}
      end)
    else
      {value, nil}
    end
  end

  # This is a temporary hack to support function calls needed by Ash
  def lift_functions({function, meta, args}, key, caller) when is_atom(meta) and is_list(args) do
    if Keyword.keyword?(args) do
      Enum.reduce(args, {[], nil}, fn {k, v}, {keyword, funs} ->
        {v, functions} = lift_functions(v, key, caller)

        funs =
          if functions do
            quote do
              unquote(funs)
              unquote(functions)
            end
          else
            funs
          end

        {[{k, v} | keyword], funs}
      end)
      |> then(fn {keyword, funs} ->
        {{function, meta, Enum.reverse(keyword)}, funs}
      end)
    else
      Enum.reduce(args, {[], nil}, fn v, {list, funs} ->
        {v, functions} = lift_functions(v, key, caller)

        funs =
          if functions do
            quote do
              unquote(funs)
              unquote(functions)
            end
          else
            funs
          end

        {[v | list], funs}
      end)
      |> then(fn {list, funs} ->
        {{function, meta, Enum.reverse(list)}, funs}
      end)
    end
  end

  def lift_functions(value, key, caller) when is_list(value) do
    if Keyword.keyword?(value) do
      Enum.reduce(value, {[], nil}, fn {k, v}, {keyword, funs} ->
        {v, functions} = lift_functions(v, key, caller)

        funs =
          if functions do
            quote do
              unquote(funs)
              unquote(functions)
            end
          else
            funs
          end

        {[{k, v} | keyword], funs}
      end)
      |> then(fn {keyword, funs} ->
        {Enum.reverse(keyword), funs}
      end)
    else
      {value, nil}
    end
  end

  def lift_functions({:&, _, [{:/, _, [{{:., _, _}, _, _}, _]}]} = value, _key, _caller),
    do: {value, nil}

  # Lift functions of the `&function/arity` variety
  def lift_functions({:&, context1, [{:/, context2, [{_, _, _}, arity]}]} = value, key, caller)
      when is_integer(arity) do
    fn_args = Macro.generate_unique_arguments(arity, caller.module)
    fn_name = generate_unique_function_name(value, key)
    function = generate_captured_function_caller(fn_name, arity, caller, context1, context2)

    value = Spark.Dsl.Extension.expand_alias_no_require(value, caller)

    {function,
     quote generated: true do
       unless Module.defines?(__MODULE__, {unquote(fn_name), unquote(Enum.count(fn_args))}, :def) do
         @doc false
         def unquote(fn_name)(unquote_splicing(fn_args)) do
           unquote(value).(unquote_splicing(fn_args))
         end
       end
     end}
  end

  def lift_functions({:&, _, [{name, _, fn_args}]} = value, key, caller)
      when is_atom(name) and name != :& do
    fn_args = generate_captured_arguments(fn_args, caller)
    fn_name = generate_unique_function_name(value, key)
    function = generate_captured_function_caller(fn_name, fn_args, caller)

    value = Spark.Dsl.Extension.expand_alias_no_require(value, caller)

    {function,
     quote generated: true do
       unless Module.defines?(__MODULE__, {unquote(fn_name), unquote(Enum.count(fn_args))}, :def) do
         @doc false
         def unquote(fn_name)(unquote_splicing(fn_args)) do
           unquote(value).(unquote_splicing(fn_args))
         end
       end
     end}
  end

  # Lift functions of the `&Module.function(&1, args)` variety
  def lift_functions(
        {:&, _, [{{:., _, [{:__aliases__, _, _aliases}, name]}, _, fn_args}]} = value,
        key,
        caller
      )
      when is_atom(name) do
    fn_args = generate_captured_arguments(fn_args, caller)
    fn_name = generate_unique_function_name(value, key)
    function = generate_captured_function_caller(fn_name, fn_args, caller)

    value = Spark.Dsl.Extension.expand_alias_no_require(value, caller)

    {function,
     quote generated: true do
       unless Module.defines?(__MODULE__, {unquote(fn_name), unquote(Enum.count(fn_args))}, :def) do
         @doc false
         def unquote(fn_name)(unquote_splicing(fn_args)) do
           unquote(value).(unquote_splicing(fn_args))
         end
       end
     end}
  end

  # Lift functions of the `&(&1 + &2)` variety
  def lift_functions({:&, _, [body]} = value, key, caller) do
    fn_args = generate_captured_arguments(body, caller)
    fn_name = generate_unique_function_name(value, key)
    function = generate_captured_function_caller(fn_name, fn_args, caller)
    value = Spark.Dsl.Extension.expand_alias_no_require(value, caller)

    {function,
     quote generated: true do
       unless Module.defines?(__MODULE__, {unquote(fn_name), unquote(Enum.count(fn_args))}, :def) do
         @doc false
         def unquote(fn_name)(unquote_splicing(fn_args)) do
           unquote(value).(unquote_splicing(fn_args))
         end
       end
     end}
  end

  # Lift anonymous functions with one or more clauses.
  def lift_functions(
        {:fn, _, [{:->, _, [fn_args, _body]} | _] = clauses} = quoted_fn,
        key,
        caller
      )
      when is_list(fn_args) do
    fn_name = generate_unique_function_name(quoted_fn, key)

    arity =
      case fn_args do
        [{:when, _, args_with_clause}] ->
          Enum.count(args_with_clause) - 1

        other ->
          Enum.count(other)
      end

    function = generate_captured_function_caller(fn_name, arity, caller)

    function_defs =
      for clause <- clauses do
        case clause do
          {:->, _, [[{:when, _, args_with_clause}], body]} ->
            args = :lists.droplast(args_with_clause)
            clause = List.last(args_with_clause)
            body = Spark.Dsl.Extension.expand_alias_no_require(body, caller)

            quote do
              def unquote(fn_name)(unquote_splicing(args)) when unquote(clause) do
                unquote(body)
              end
            end

          {:->, _, [args, body]} ->
            body = Spark.Dsl.Extension.expand_alias_no_require(body, caller)

            quote do
              def unquote(fn_name)(unquote_splicing(args)) do
                unquote(body)
              end
            end
        end
      end

    {function,
     quote generated: true do
       unless Module.defines?(__MODULE__, {unquote(fn_name), unquote(arity)}, :def) do
         @doc false
         unquote_splicing(function_defs)
       end
     end}
  end

  # Ignore all other values.
  def lift_functions(value, _key, _caller), do: {value, nil}

  # sobelow_skip ["DOS.BinToAtom"]
  defp generate_unique_function_name(value, key) do
    fn_name = Spark.CodeHelpers.code_identifier(value)

    :"#{key}_#{Spark.Dsl.Extension.monotonic_number({key, fn_name})}_generated_#{fn_name}"
  end

  # Counts up all the arguments and generates new unique arguments for them.
  # Works around the caveat that each usage of a unique `&n` variable must only
  # be counted once.
  defp generate_captured_arguments(args, caller) do
    Macro.prewalk(args, [], fn
      {:&, _, [v]} = ast, acc when is_integer(v) ->
        {ast, [v | acc]}

      ast, acc ->
        {ast, acc}
    end)
    |> elem(1)
    |> Enum.uniq()
    |> Enum.count()
    |> Macro.generate_unique_arguments(caller.module)
  end

  # Generates the code for calling the target function as a function capture.
  defp generate_captured_function_caller(
         fn_name,
         arity,
         caller,
         context1 \\ [],
         context2 \\ [context: Elixir, imports: [{2, Kernel}]]
       )

  defp generate_captured_function_caller(fn_name, arity, caller, context1, context2)
       when is_integer(arity) do
    {:&, context1,
     [
       {:/, context2,
        [
          {{:., [], [{:__aliases__, [alias: false], [caller.module]}, fn_name]},
           [no_parens: true], []},
          arity
        ]}
     ]}
  end

  defp generate_captured_function_caller(fn_name, fn_args, caller, context1, context2),
    do:
      generate_captured_function_caller(
        fn_name,
        Enum.count(fn_args),
        caller,
        context1,
        context2
      )
end
