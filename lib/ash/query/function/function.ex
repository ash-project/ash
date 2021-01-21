defmodule Ash.Query.Function do
  @moduledoc """
  A function is a predicate with an arguments list.

  For more information on being a predicate, see `Ash.Filter.Predicate`. Most of the complexities
  are there. A function must meet both behaviours.
  """

  @type arg :: any
  @doc """
  The number and types of arguments supported.
  """
  @callback args() :: [arg]
  @callback new(list(term)) :: {:ok, term}
  @callback evaluate(func :: map) :: :unknown | {:known, term}

  alias Ash.Query.{BooleanExpression, Call, Not, Ref}

  def new(mod, args) do
    args = List.wrap(args)
    all_args = args

    case mod.args() do
      :var_args ->
        mod.new(args)

      mod_args ->
        configured_args = List.wrap(mod_args)
        configured_arg_count = Enum.count(configured_args)
        given_arg_count = Enum.count(args)

        if configured_arg_count == given_arg_count do
          args
          |> Enum.zip(configured_args)
          |> Enum.with_index()
          |> Enum.reduce_while({:ok, []}, fn
            {{arg, :any}, _}, {:ok, args} ->
              {:cont, {:ok, [arg | args]}}

            {{%struct{} = arg, _}, _}, {:ok, args}
            when struct in [BooleanExpression, Call, Not, Ref] ->
              {:cont, {:ok, [arg | args]}}

            {{%{__predicate__?: _} = arg, _}, _}, {:ok, args} ->
              {:cont, {:ok, [arg | args]}}

            {{arg, type}, i}, {:ok, args} ->
              case Ash.Type.cast_input(type, arg) do
                {:ok, value} ->
                  {:cont, {:ok, [value | args]}}

                _ ->
                  {:halt,
                   {:error,
                    Ash.Error.Query.InvalidExpression.exception(
                      expression: struct(mod, arguments: all_args),
                      message: "#{ordinal(i + 1)} argument to #{mod.name()} is invalid"
                    )}}
              end
          end)
          |> case do
            {:ok, args} ->
              mod.new(Enum.reverse(args))

            {:error, error} ->
              {:error, error}
          end
        else
          {:error,
           "function #{mod.name()} takes #{configured_arg_count} arguments, provided #{
             given_arg_count
           }"}
        end
    end
  end

  # Copied from https://github.com/andrewhao/ordinal/blob/master/lib/ordinal.ex
  @doc """
  Attaches the appropiate suffix to refer to an ordinal number, e.g 1 -> "1st"
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

      defoverridable new: 1, evaluate: 1

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
