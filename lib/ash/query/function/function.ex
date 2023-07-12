defmodule Ash.Query.Function do
  @moduledoc """
  A function is a predicate with an arguments list.

  For more information on being a predicate, see `Ash.Filter.Predicate`. Most of the complexities
  are there. A function must meet both behaviours.
  """

  import Ash.Filter.TemplateHelpers, only: [expr?: 1]

  @type arg :: any
  @doc """
  The number and types of arguments supported.
  """
  @callback args() :: [arg]
  @callback new(list(term)) :: {:ok, term} | {:error, String.t() | Exception.t()}
  @callback evaluate(func :: map) :: :unknown | {:known, term}
  @callback partial_evaluate(func) :: func when func: map
  @callback private?() :: boolean

  @optional_callbacks partial_evaluate: 1

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
          |> Enum.find_value(&try_cast_arguments(&1, args))
          |> case do
            nil ->
              {:error, "Could not cast function arguments for #{mod.name()}/#{given_arg_count}"}

            casted ->
              case mod.new(casted) do
                {:ok, function} ->
                  if Enum.any?(casted, &expr?/1) do
                    if function_exported?(mod, :partial_evaluate, 1) && match?(%^mod{}, function) do
                      {:ok, mod.partial_evaluate(function)}
                    else
                      {:ok, function}
                    end
                  else
                    case function do
                      %^mod{__predicate__?: _} ->
                        if mod.eager_evaluate?() do
                          case mod.evaluate(function) do
                            {:known, result} ->
                              {:ok, result}

                            :unknown ->
                              {:ok, function}

                            {:error, error} ->
                              {:error, error}
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

  def try_cast_arguments(configured_args, args) do
    args
    |> Enum.zip(configured_args)
    |> Enum.reduce_while({:ok, []}, fn
      {nil, _}, {:ok, args} ->
        {:cont, {:ok, [nil | args]}}

      {arg, :any}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {%{__predicate__?: _} = arg, _}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {arg, {type, constraints}}, {:ok, args} when type != :array ->
        if expr?(arg) do
          {:cont, {:ok, [arg | args]}}
        else
          case Ash.Query.Type.try_cast(arg, type, constraints) do
            {:ok, value} -> {:cont, {:ok, [value | args]}}
            :error -> {:halt, :error}
          end
        end

      {arg, type}, {:ok, args} ->
        if expr?(arg) do
          {:cont, {:ok, [arg | args]}}
        else
          case Ash.Query.Type.try_cast(arg, type, []) do
            {:ok, value} -> {:cont, {:ok, [value | args]}}
            :error -> {:halt, :error}
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
      @behaviour Ash.Filter.Predicate

      alias Ash.Query.Ref

      defstruct [
        :arguments,
        name: unquote(opts[:name]),
        embedded?: false,
        __function__?: true,
        __predicate__?: unquote(opts[:predicate?] || false)
      ]

      def name, do: unquote(opts[:name])

      def new(args), do: {:ok, struct(__MODULE__, arguments: args)}

      def evaluate(_), do: :unknown

      def predicate?, do: unquote(opts[:predicate?] || false)

      def eager_evaluate?, do: unquote(Keyword.get(opts, :eager_evaluate?, true))

      def private?, do: false

      defoverridable new: 1, evaluate: 1, private?: 0

      unless unquote(opts[:no_inspect?]) do
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
end
