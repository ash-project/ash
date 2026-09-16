# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function do
  @moduledoc """
  A function is a predicate with an arguments list.

  For more information on being a predicate, see `Ash.Filter.Predicate`. Most of the complexities
  are there. A function must meet both behaviours.
  """

  import Ash.Expr, only: [expr?: 1]

  @type arg :: any
  @doc """
  The number and types of arguments supported.
  """
  @callback args() :: [arg] | :var_args

  @doc """
  The return type for each corresponding set of args.
  """
  @callback returns() ::
              [Ash.Type.t() | {Ash.Type.t(), constraints :: Keyword.t()}]
              | Ash.Type.t()
              | {Ash.Type.t(), constraints :: Keyword.t()}
              | :unknown

  @doc "The name of the function"
  @callback name() :: atom
  @doc "Instantiate a new function with the provided arguments"
  @callback new(list(term)) :: {:ok, term} | {:error, String.t() | Exception.t()}
  @doc "Evaluate a function when all arguments are known valid values"
  @callback evaluate(func :: map) :: :unknown | {:known, term} | {:error, term}
  @doc "Evaluate a function when some or no arguments are known valid values"
  @callback partial_evaluate(func) :: {:ok, func} | {:error, term} when func: map
  @doc "Whether or not the function can be evaluated eagerly. For example, `now()` cannot be."
  @callback eager_evaluate?() :: boolean()
  @doc "Whether or not the function is a predicate (takes a reference as the first argument, a value as the second, and returns a boolean)"
  @callback predicate?() :: boolean()
  @doc "Whether or not the function should be usable when parsing input."
  @callback private?() :: boolean
  @doc "Whether or not the function return nil."
  @callback can_return_nil?(func :: map) :: boolean()

  @doc """
  If `true`, will be allowed to evaluate `nil` inputs.

  If `false` (the default), any `nil` inputs will cause a `nil` return.
  """
  @callback evaluate_nil_inputs?() :: boolean()

  @optional_callbacks partial_evaluate: 1

  @doc "Evaluate the operator with provided inputs"
  def evaluate(%mod{arguments: arguments} = func) do
    if Enum.any?(arguments, &is_nil/1) && !mod.evaluate_nil_inputs?() do
      {:known, nil}
    else
      mod.evaluate(func)
    end
  end

  def new(mod, args) do
    args = List.wrap(args)

    case mod.args() do
      :var_args ->
        # Varargs is special, and should only be used in rare circumstances (like this one)
        # no type casting or help can be provided for these functions.
        mod.new(args)

      mod_args ->
        configured_args = overload_signatures(mod) ++ List.wrap(mod_args)
        allowed_arg_counts = Enum.map(configured_args, &Enum.count/1)
        given_arg_count = Enum.count(args)

        if given_arg_count in allowed_arg_counts do
          signatures =
            Enum.filter(configured_args, fn args ->
              Enum.count(args) == given_arg_count
            end)

          # Prefer a signature the values already fit over one they must be
          # coerced into: a `NaiveDateTime` coerces to `:datetime` by assuming
          # UTC, but `[:naive_datetime, ...]` is the signature meant for it.
          (Enum.find_value(signatures, &try_cast_arguments(&1, args, exact?: true)) ||
             Enum.find_value(signatures, &try_cast_arguments(&1, args)))
          |> case do
            nil ->
              {:error, cast_error(mod, configured_args, args)}

            casted ->
              case mod.new(casted) do
                {:ok, function} ->
                  if Enum.any?(casted, &expr?/1) do
                    if function_exported?(mod, :partial_evaluate, 1) && match?(%^mod{}, function) do
                      mod.partial_evaluate(function)
                    else
                      {:ok, function}
                    end
                  else
                    case function do
                      %^mod{__predicate__?: _} ->
                        if mod.eager_evaluate?() do
                          if mod.evaluate_nil_inputs?() ||
                               Enum.all?(function.arguments, &(not is_nil(&1))) do
                            case mod.evaluate(function) do
                              {:known, result} ->
                                {:ok, result}

                              :unknown ->
                                {:ok, function}

                              {:error, error} ->
                                {:error, error}
                            end
                          else
                            {:ok, nil}
                          end
                        else
                          {:ok, function}
                        end

                      _ ->
                        {:ok, function}
                    end
                  end

                other ->
                  other
              end
          end
        else
          did_you_mean =
            Enum.map_join(allowed_arg_counts, "\n", fn arg_count ->
              " . * #{mod.name()}/#{arg_count}"
            end)

          {:error,
           """
             No such function #{mod.name()}/#{given_arg_count}. Did you mean one of:

             #{did_you_mean}
           """}
        end
    end
  end

  @doc """
  Casts `args` to a single declared signature, returning the cast list or `nil`.

  With `exact?: true`, a value only fits a declared type if casting leaves it
  unchanged, so signatures are matched on the type the value already has.
  """
  def try_cast_arguments(configured_args, args, opts \\ []) do
    exact? = Keyword.get(opts, :exact?, false)

    args
    |> Enum.zip(configured_args)
    |> Enum.reduce_while({:ok, []}, fn
      {nil, _}, {:ok, args} ->
        {:cont, {:ok, [nil | args]}}

      {arg, :any}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {arg, :same}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      # `{:array, :same}`, `{:range, :any}`: the parameter can't be checked here,
      # but the constructor can when the argument is a typed expression.
      {arg, {_parameterized, vague} = declared}, {:ok, args} when vague in [:any, :same] ->
        if expr?(arg) and not compatible_expr_type?(arg, declared) do
          {:halt, :error}
        else
          {:cont, {:ok, [arg | args]}}
        end

      {%{__predicate__?: _} = arg, _}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {arg, {type, constraints}}, {:ok, args} when type != :array ->
        if expr?(arg) do
          if compatible_expr_type?(arg, type) do
            {:cont, {:ok, [arg | args]}}
          else
            {:halt, :error}
          end
        else
          case Ash.Query.Type.try_cast(arg, type, constraints) do
            {:ok, value} when not exact? or value == arg -> {:cont, {:ok, [value | args]}}
            _ -> {:halt, :error}
          end
        end

      {arg, type}, {:ok, args} ->
        if expr?(arg) do
          if compatible_expr_type?(arg, type) do
            {:cont, {:ok, [arg | args]}}
          else
            {:halt, :error}
          end
        else
          case Ash.Query.Type.try_cast(arg, type, []) do
            {:ok, value} when not exact? or value == arg -> {:cont, {:ok, [value | args]}}
            _ -> {:halt, :error}
          end
        end
    end)
    |> case do
      {:ok, args} ->
        Enum.reverse(args)

      _ ->
        nil
    end
  end

  @doc """
  Whether an expression's type is compatible with a declared argument type.

  Vague declarations (`:any`, `:same` and their array forms) accept anything.
  A concrete declaration matches when the expression's type, as resolved by
  `Ash.Expr.determine_type/1`, is that type or acts as it: a NewType acts as
  its `subtype_of`, and any type may name another via `c:Ash.Type.acts_as/1`
  (an `:atom` acts as a `:string`, an embedded resource acts as a `:map`, and
  so on), recursively. Functions should therefore declare the most general
  type they accept: `datetime_add/3` declares `:datetime`, so any datetime
  NewType such as `:utc_datetime_usec` is accepted. An expression whose type
  cannot be determined is considered compatible, as there is no evidence
  against it.
  """
  @spec compatible_expr_type?(term, term) :: boolean
  def compatible_expr_type?(_expr, vague) when vague in [:any, :same], do: true
  def compatible_expr_type?(_expr, {:array, vague}) when vague in [:any, :same], do: true

  # `{:range, :same}`: only the constructor can be checked here.
  def compatible_expr_type?(expr, {parameterized, vague}) when vague in [:any, :same],
    do: compatible_expr_type?(expr, parameterized)

  def compatible_expr_type?(expr, declared) do
    declared =
      case declared do
        {:array, {type, _constraints}} -> {:array, Ash.Type.get_type(type)}
        {type, _constraints} when type != :array -> Ash.Type.get_type(type)
        type -> Ash.Type.get_type(type)
      end

    case Ash.Expr.determine_type(expr) do
      {:ok, {actual, constraints}} ->
        if known_type?(actual) && known_type?(declared) do
          declared in acts_as_types(actual, constraints)
        else
          true
        end

      :error ->
        true
    end
  end

  # Every type the given type may stand in for, itself included, following
  # `Ash.Type.NewType.subtype_of/0` and `c:Ash.Type.acts_as/1` until they run out.
  defp acts_as_types(type, constraints, acc \\ [])

  defp acts_as_types({:array, type}, constraints, acc) do
    type
    |> acts_as_types(constraints[:items] || [], [])
    |> Enum.map(&{:array, &1})
    |> Enum.concat(acc)
    |> Enum.uniq()
  end

  defp acts_as_types(type, constraints, acc) do
    type = Ash.Type.get_type(type)

    if type in acc do
      acc
    else
      acc = [type | acc]

      acc =
        if Ash.Type.NewType.new_type?(type) do
          acts_as_types(type.subtype_of(), Ash.Type.NewType.constraints(type, constraints), acc)
        else
          acc
        end

      case Ash.Type.acts_as(type, constraints) do
        nil -> acc
        other -> acts_as_types(other, [], acc)
      end
    end
  end

  defp known_type?({:array, type}), do: known_type?(type)
  defp known_type?(type), do: is_atom(type) && Ash.Type.ash_type?(type)

  # Types may register overloads for a function under its name, the same way
  # they do for operators (see `Ash.Type.operator_overloads/0`). Those are
  # signatures too, and take priority over the function's own declarations.
  defp overload_signatures(mod) do
    mod.name()
    |> Ash.Query.Operator.operator_overloads()
    |> Kernel.||(%{})
    |> Map.keys()
    |> Enum.filter(&is_list/1)
  end

  defp cast_error(mod, configured_args, args) do
    given_arg_count = Enum.count(args)

    # If a typed input reference is what disqualified every signature, say so:
    # that is far more useful than a generic "could not cast".
    args
    |> Enum.with_index()
    |> Enum.find_value(fn
      {%Ash.Query.Ref{attribute: %{type: ref_type}} = ref, index}
      when not is_nil(ref_type) ->
        accepted_types =
          configured_args
          |> Enum.filter(&(Enum.count(&1) == given_arg_count))
          |> Enum.map(&Enum.at(&1, index))

        if Enum.any?(accepted_types, &compatible_expr_type?(ref, &1)) do
          nil
        else
          accepted =
            accepted_types
            |> Enum.map(fn
              {type, _constraints} when type != :array -> type
              type -> type
            end)
            |> Enum.uniq()
            |> Enum.map_join(", ", &inspect/1)

          "`#{mod.name()}` cannot be applied to `#{ref_name(ref)}`: #{ordinal(index + 1)} " <>
            "argument must be one of #{accepted}, but `#{ref_name(ref)}` is of type " <>
            describe_type(ref)
        end

      _ ->
        nil
    end)
    |> case do
      nil -> "Could not cast function arguments for #{mod.name()}/#{given_arg_count}"
      message -> message
    end
  end

  defp ref_name(%Ash.Query.Ref{attribute: %{name: name}, relationship_path: []}), do: name

  defp ref_name(%Ash.Query.Ref{attribute: %{name: name}, relationship_path: path}),
    do: Enum.join(path ++ [name], ".")

  defp ref_name(%Ash.Query.Ref{attribute: attribute}), do: inspect(attribute)

  defp describe_type(%Ash.Query.Ref{attribute: %{type: type}}) do
    type = Ash.Type.get_type(type)

    Ash.Type.short_names()
    |> Enum.find_value(type, fn {short_name, module} ->
      if module == type, do: short_name
    end)
    |> inspect()
  end

  # Copied from https://github.com/andrewhao/ordinal/blob/master/lib/ordinal.ex
  @doc """
  Attaches the appropriate suffix to refer to an ordinal number, e.g 1 -> "1st"
  """
  def ordinal(num) do
    cond do
      Enum.any?([11, 12, 13], &(&1 == Integer.mod(num, 100))) ->
        "#{num}th"

      Integer.mod(num, 10) == 1 ->
        "#{num}st"

      Integer.mod(num, 10) == 2 ->
        "#{num}nd"

      Integer.mod(num, 10) == 3 ->
        "#{num}rd"

      true ->
        "#{num}th"
    end
  end

  defmacro __using__(opts) do
    quote do
      @before_compile Ash.Query.Function
      @behaviour Ash.Query.Function
      if unquote(opts[:predicate?] || false) do
        @behaviour Ash.Filter.Predicate
      end

      alias Ash.Query.Ref

      defstruct [
        :arguments,
        name: unquote(opts[:name]),
        embedded?: false,
        __function__?: true,
        __predicate__?: unquote(opts[:predicate?] || false),
        extra: %{}
      ]

      @impl Ash.Query.Function
      def predicate?, do: unquote(opts[:predicate?] || false)

      @impl Ash.Query.Function
      def name, do: unquote(opts[:name])

      @impl Ash.Query.Function
      def new(args), do: {:ok, struct(__MODULE__, arguments: args)}

      @impl Ash.Query.Function
      def evaluate(_), do: :unknown

      @impl Ash.Query.Function
      def eager_evaluate?, do: unquote(Keyword.get(opts, :eager_evaluate?, true))

      @impl Ash.Query.Function
      def evaluate_nil_inputs?, do: false

      @impl Ash.Query.Function
      def private?, do: false

      @impl Ash.Query.Function
      def can_return_nil?(_), do: true

      @impl Ash.Query.Function
      def returns do
        :unknown
      end

      defoverridable new: 1,
                     evaluate: 1,
                     private?: 0,
                     evaluate_nil_inputs?: 0,
                     can_return_nil?: 1,
                     returns: 0

      if !unquote(opts[:no_inspect?]) do
        defimpl Inspect do
          import Inspect.Algebra

          def inspect(%{arguments: args, name: name}, opts) do
            concat(
              to_string(name),
              container_doc("(", args, ")", opts, &to_doc/2, separator: ",")
            )
          end
        end
      end
    end
  end

  defmacro __before_compile__(_) do
    quote generated: true do
      if Module.defines?(__MODULE__, {:partial_evaluate, 1}, :def) do
        def has_partial_evaluate?, do: true
      else
        def has_partial_evaluate?, do: false
      end
    end
  end
end
