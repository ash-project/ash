defmodule Ash.Filter.Predicate do
  @moduledoc "Represents a filter predicate"

  defstruct [:attribute, :relationship_path, :predicate]

  alias Ash.Filter
  alias Ash.Filter.{Expression, Not}

  @type predicate :: struct

  @type comparison ::
          :unknown
          | :right_excludes_left
          | :left_excludes_right
          | :right_includes_left
          | :left_includes_right
          | :mutually_inclusive
          # A simplification value for the right term
          | {:simplify, term}
          | {:simplify, term, term}

  @type t :: %__MODULE__{
          attribute: Ash.attribute(),
          relationship_path: list(atom),
          predicate: predicate
        }

  @callback new(Ash.resource(), Ash.attribute(), term) :: {:ok, struct} | {:error, term}
  @callback compare(predicate(), predicate()) :: comparison()

  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Filter.Predicate

      @spec compare(Ash.Filter.Predicate.predicate(), Ash.Filter.Predicate.predicate()) ::
              Ash.Filter.Predicate.comparison() | :unknown
      def compare(_, _), do: :unknown

      defoverridable compare: 2
    end
  end

  @spec compare(predicate(), predicate()) :: comparison
  def compare(%__MODULE__{predicate: left} = pred, right) do
    case compare(left, right) do
      {:simplify, simplification} ->
        simplification =
          Filter.map(simplification, fn
            %struct{} = expr when struct in [__MODULE__, Not, Expression] ->
              expr

            other ->
              wrap_in_predicate(pred, other)
          end)

        {:simplify, simplification}

      other ->
        other
    end
  end

  def compare(left, %__MODULE__{predicate: right}), do: compare(left, right)

  def compare(left, right) do
    if left.__struct__ == right.__struct__ do
      with {:right_to_left, :unknown} <- {:right_to_left, left.__struct__.compare(left, right)},
           {:left_to_right, :unknown} <- {:left_to_right, right.__struct__.compare(left, right)} do
        :mutually_exclusive
      else
        {:right_to_left, {:simplify, left, _}} -> {:simplify, left}
        {:left_to_right, {:simplify, _, right}} -> {:simplify, right}
        {_, other} -> other
      end
    else
      with {:right_to_left, :unknown} <- {:right_to_left, left.__struct__.compare(left, right)},
           {:right_to_left, :unknown} <- {:right_to_left, right.__struct__.compare(left, right)},
           {:left_to_right, :unknown} <- {:left_to_right, right.__struct__.compare(left, right)},
           {:left_to_right, :unknown} <- {:left_to_right, left.__struct__.compare(left, right)} do
        :mutually_exclusive
      else
        {:right_to_left, {:simplify, left, _}} -> {:simplify, left}
        {:left_to_right, {:simplify, _, right}} -> {:simplify, right}
        {_, other} -> other
      end
    end
  end

  defp wrap_in_predicate(predicate, %struct{} = other) do
    if Ash.implements_behaviour?(struct, Ash.Filter.Predicate) do
      %{predicate | predicate: other}
    else
      other
    end
  end

  def new(resource, attribute, predicate, value, relationship_path) do
    case predicate.new(resource, attribute, value) do
      {:ok, predicate} ->
        if Ash.data_layer_can?(resource, {:filter_predicate, predicate}) do
          {:ok,
           %__MODULE__{
             attribute: attribute,
             predicate: predicate,
             relationship_path: relationship_path
           }}
        else
          {:error, "Data layer does not support filtering with #{inspect(predicate)}"}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  def add_inspect_path(inspect_opts, field) do
    case inspect_opts.custom_options[:relationship_path] do
      empty when empty in [nil, []] -> to_string(field)
      path -> Enum.join(path, ".") <> "." <> to_string(field)
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(
          %{relationship_path: relationship_path, predicate: predicate},
          opts
        ) do
      opts = %{
        opts
        | syntax_colors: [
            atom: :yellow,
            binary: :green,
            boolean: :pink,
            list: :cyan,
            map: :magenta,
            number: :red,
            regex: :violet,
            tuple: :white
          ],
          custom_options: Keyword.put(opts.custom_options, :relationship_path, relationship_path)
      }

      to_doc(predicate, opts)
    end
  end
end
