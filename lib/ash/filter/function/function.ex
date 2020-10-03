defmodule Ash.Filter.Function do
  @callback new(list(term)) :: {:ok, term}

  def new(mod, args, ref) do
    args = List.wrap(args)
    configured_args = mod.args()
    configured_arg_count = Enum.count(configured_args)
    given_arg_count = Enum.count(args)

    args =
      if configured_arg_count < given_arg_count do
        raise "function #{mod.name()} takes #{configured_arg_count} arguments, provided #{
                given_arg_count
              }"
      else
        args ++
          :lists.duplicate(
            max(0, given_arg_count - configured_arg_count),
            nil
          )
      end

    args
    |> Enum.zip(configured_args)
    |> Enum.reduce_while({:ok, []}, fn
      {arg, :ref}, {:ok, args} when is_atom(arg) ->
        case Ash.Resource.attribute(ref.resource, arg) do
          nil ->
            {:halt, {:error, "invalid reference"}}

          attribute ->
            {:cont, {:ok, [%{ref | attribute: attribute} | args]}}
        end

      {arg, :any}, {:ok, args} ->
        {:cont, {:ok, [arg | args]}}
    end)
    |> case do
      {:ok, args} ->
        mod.new(Enum.reverse(args))

      {:error, error} ->
        {:error, error}
    end
  end

  defmacro __using__(opts) do
    quote do
      use Ash.Filter.Predicate

      alias Ash.Filter.Ref

      defstruct [
        :args,
        name: unquote(opts[:name]),
        embedded: false,
        __function__?: true,
        __predicate__?: true
      ]

      def name, do: unquote(opts[:name])

      defimpl Inspect do
        import Inspect.Algebra

        def inspect(%{args: args, name: name}, opts) do
          concat([
            container_doc(name <> "(", args, ")", opts, &inspect/1, separator: ", ")
          ])
        end
      end
    end
  end
end
