defmodule Ash.Query.Function do
  @moduledoc """
  A function is a predicate with an arguments list.

  For more information on being a predicate, see `Ash.Filter.Predicate`. Most of the complexities
  are there. A function must meet both behaviours.
  """

  @callback new(list(term)) :: {:ok, term}

  @type arg :: :ref | :term | {:options, Keyword.t()}
  @doc """
  The number and types of arguments supported.

  Currently supports three values: `:ref`, `:term`, and `{:options, schema}`.

  * `:ref` - a column/relationship path reference. Will be an instance of `Ash.Filter.Ref`
  * `:term` - any value. No type validation is currently supported except for what is listed here, so it must be done in the `c:new/1` function
  * `{:options, keys}` - Only the last arg may be options, and `keys` is a list of atoms for which options are accepted
  """
  @callback args() :: [arg]

  @doc """
  Return true or false if the left and right match the operator.

  Any references are resolved before being passed in.

  If this is not defined, it will be assumed that data does not match.
  """
  @callback match?(term) :: boolean

  def new(mod, args, ref) do
    args = List.wrap(args)
    configured_args = List.wrap(mod.args())
    configured_arg_count = Enum.count(configured_args)
    given_arg_count = Enum.count(args)

    if configured_arg_count == given_arg_count do
      args
      |> Enum.zip(configured_args)
      |> Enum.with_index()
      |> Enum.reduce_while({:ok, []}, fn
        {{arg, :ref}, i}, {:ok, args} when is_atom(arg) ->
          case Ash.Resource.attribute(ref.resource, arg) do
            nil ->
              {:halt,
               {:error, "invalid reference in #{ordinal(i + 1)} argument to #{mod.name()}"}}

            attribute ->
              {:cont, {:ok, [%{ref | attribute: attribute} | args]}}
          end

        {{arg, :term}, _}, {:ok, args} ->
          {:cont, {:ok, [arg | args]}}

        {{arg, {:options, keys}}, i}, {:ok, args} ->
          case to_keys(arg, keys) do
            {:ok, opts} ->
              {:cont, {:ok, [opts | args]}}

            {:error, message} when is_binary(message) ->
              {:halt,
               {:error, "#{ordinal(i + 1)} argument to #{mod.name()} is invalid: #{message}"}}

            {:error, exception} ->
              {:halt,
               {:error,
                "#{ordinal(i + 1)} argument to #{mod.name()} is invalid: #{
                  Exception.message(exception)
                }"}}
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

  defp to_keys(nil, _), do: {:ok, nil}

  defp to_keys(opts, keys) do
    if is_map(opts) || Keyword.keyword?(opts) do
      string_keys = Enum.map(keys, &to_string/1)

      Enum.reduce_while(opts, {:ok, []}, fn
        {key, value}, {:ok, opts} when is_binary(key) ->
          if key in string_keys do
            {:cont, {:ok, [{String.to_existing_atom(key), value} | opts]}}
          else
            {:halt, {:error, "No such option #{key}"}}
          end

        {key, value}, {:ok, opts} when is_atom(key) ->
          if key in keys do
            {:cont, {:ok, [{key, value} | opts]}}
          else
            {:halt, {:error, "No such option #{key}"}}
          end

        {key, _}, _ ->
          {:halt, {:error, "No such option #{key}"}}
      end)
    else
      {:error, "Invalid options #{inspect(opts)}"}
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

      alias Ash.Filter.Ref

      defstruct [
        :arguments,
        name: unquote(opts[:name]),
        embedded?: false,
        __function__?: true,
        __predicate__?: true
      ]

      def name, do: unquote(opts[:name])

      defimpl Inspect do
        import Inspect.Algebra

        def inspect(%{arguments: args, name: name}, opts) do
          concat(
            to_string(name),
            container_doc("(", args, ")", opts, &to_doc(&1, &2), separator: ", ")
          )
        end
      end
    end
  end
end
