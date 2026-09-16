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
        configured_args = List.wrap(mod_args)
        allowed_arg_counts = Enum.map(configured_args, &Enum.count/1)
        given_arg_count = Enum.count(args)

        if given_arg_count in allowed_arg_counts do
          mod_args
          |> Enum.filter(fn args ->
            Enum.count(args) == given_arg_count
          end)
          |> Enum.find_value(&try_cast_arguments(&1, args, check_refs?: mod.predicate?()))
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
  Casts `args` against one declared signature, returning the cast list or `nil`
  when the signature does not fit.

  Literal values are cast with `Ash.Query.Type.try_cast/3`. Expressions are never
  cast; with `check_refs?: true` a `%Ash.Query.Ref{}` whose attribute type is
  incompatible with a concrete declared type makes the signature not fit, so a
  predicate such as `contains/2` cannot be applied to an `:integer` attribute.
  """
  def try_cast_arguments(configured_args, args, opts \\ []) do
    check_refs? = Keyword.get(opts, :check_refs?, false)

    args
    |> Enum.zip(configured_args)
    |> Enum.reduce_while({:ok, []}, fn
      {nil, _}, {:ok, args} ->
        {:cont, {:ok, [nil | args]}}

      {arg, :any}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {arg, :same}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {arg, {:array, vague}}, {:ok, args} when vague in [:any, :same] ->
        {:cont, {:ok, [arg | args]}}

      {%{__predicate__?: _} = arg, _}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {arg, {type, constraints}}, {:ok, args} when type != :array ->
        cond do
          !expr?(arg) ->
            case Ash.Query.Type.try_cast(arg, type, constraints) do
              {:ok, value} -> {:cont, {:ok, [value | args]}}
              :error -> {:halt, :error}
            end

          check_refs? and ref_type_conflicts?(arg, type) ->
            {:halt, :error}

          true ->
            {:cont, {:ok, [arg | args]}}
        end

      {arg, type}, {:ok, args} ->
        cond do
          !expr?(arg) ->
            case Ash.Query.Type.try_cast(arg, type, []) do
              {:ok, value} -> {:cont, {:ok, [value | args]}}
              :error -> {:halt, :error}
            end

          check_refs? and ref_type_conflicts?(arg, type) ->
            {:halt, :error}

          true ->
            {:cont, {:ok, [arg | args]}}
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
  Returns true when `ref` points at an attribute whose type cannot satisfy the
  declared argument `type`.

  Only a reference with a determinable Ash type is ever rejected. A type is
  compatible with the declaration when it is the same type, a `Ash.Type.NewType`
  of it, or stored the same way (so an `:atom` attribute still satisfies a
  `:string` argument). Anything undeterminable is left alone, as it is today.
  """
  def ref_type_conflicts?(%Ash.Query.Ref{} = ref, declared_type) do
    with {:ok, {ref_type, ref_constraints}} <- Ash.Expr.determine_type(ref),
         declared_type when is_atom(declared_type) or is_tuple(declared_type) <-
           Ash.Type.get_type(declared_type),
         true <- Ash.Type.ash_type?(declared_type) do
      not compatible_types?(ref_type, ref_constraints, declared_type)
    else
      _ ->
        false
    end
  end

  def ref_type_conflicts?(_other, _declared_type), do: false

  defp compatible_types?(ref_type, ref_constraints, declared_type) do
    ref_base = Ash.Type.NewType.subtype_of(ref_type)
    declared_base = Ash.Type.NewType.subtype_of(declared_type)

    ref_base == declared_base or
      same_storage_type?(ref_type, ref_constraints, declared_type)
  end

  defp same_storage_type?(ref_type, ref_constraints, declared_type) do
    Ash.Type.storage_type(ref_type, ref_constraints) ==
      Ash.Type.storage_type(declared_type, [])
  rescue
    _ -> true
  end

  defp cast_error(mod, configured_args, args) do
    given_arg_count = Enum.count(args)

    ref_types =
      args
      |> Enum.flat_map(fn
        %Ash.Query.Ref{} = ref ->
          case Ash.Expr.determine_type(ref) do
            {:ok, {type, _}} -> ["#{inspect(ref)} is of type #{inspect(type)}"]
            :error -> []
          end

        _ ->
          []
      end)

    message =
      "Could not cast function arguments for #{mod.name()}/#{given_arg_count}. " <>
        "Accepted argument types: #{inspect(configured_args)}"

    message =
      if ref_types == [] do
        message
      else
        message <> ". " <> Enum.join(ref_types, ", ")
      end

    Ash.Error.Query.InvalidFilterValue.exception(
      value: %Ash.Query.Call{name: mod.name(), args: args},
      message: message
    )
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
