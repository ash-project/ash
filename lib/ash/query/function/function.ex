defmodule Ash.Query.Function do
  @moduledoc """
  A function is a predicate with an arguments list.

  For more information on being a predicate, see `Ash.Filter.Predicate`. Most of the complexities
  are there. A function must meet both behaviours.
  """

  alias Ash.Query.{BooleanExpression, Call, Not, Ref}

  @type arg :: any
  @doc """
  The number and types of arguments supported.
  """
  @callback args() :: [arg]
  @callback new(list(term)) :: {:ok, term}
  @callback evaluate(func :: map) :: :unknown | {:known, term}

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
              mod.new(casted)
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

  defp try_cast_arguments(configured_args, args) do
    args
    |> Enum.zip(configured_args)
    |> Enum.reduce_while({:ok, []}, fn
      {arg, :any}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {%struct{} = arg, _}, {:ok, args}
      when struct in [BooleanExpression, Call, Not, Ref] ->
        {:cont, {:ok, [arg | args]}}

      {%{__predicate__?: _} = arg, _}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}

      {arg, type}, {:ok, args} ->
        case Ash.Query.Type.try_cast(arg, type) do
          {:ok, value} -> {:cont, {:ok, [value | args]}}
          :error -> {:halt, :error}
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

      def predicate?, do: unqutoe(opts[:predicate?] || false)

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
