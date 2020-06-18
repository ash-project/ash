defmodule Ash.Filter.Predicate do
  defstruct [:attribute, :relationship_path, :predicate]

  alias Ash.Filter.Predicate.Comparison

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
  @callback compare(predicate(), predicate()) :: Comparison.comparison()

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
  def compare(%__MODULE__{predicate: left}, right), do: compare(left, right)
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

    # right_compared_to_left =
    #   case left.__struct__.compare(right) do
    #     :unkown ->
    #       right.__struct__.compare(left)
    #   end
    # left_compared_to_right = right.__struct__.compare(left)

    # # This is not very performant, and will result in many unnecassary cycles
    # # simplifying values. This will work in the short term though. It can be
    # # optimized later
    # case {right_compared_to_left, left_compared_to_right} do
    #   {:unknown, :unknown} -> :unknown
    #   {:exclusive, :unknown} -> :right_excludes_left
    #   {:unknown, :exclusive} -> :left_excludes_right
    #   {:inclusive, :unknown} -> :right_includes_left
    #   {:unknown, :inclusive} -> :left_includes_right
    #   {:inclusive, :inclusive} -> :mutually_inclusive
    #   {:exclusive, :exclusive} -> :mutually_exclusive
    #   {_, :exclusive} -> :left_excludes_right
    #   {:exclusive, _} -> :right_excludes_left
    #   {_, :inclusive} -> :left_includes_right
    #   {:inclusive, _} -> :right_includes_left
    #   {{:simplify, new_left, _}, _} -> {:simplify, new_left}
    #   {_, {:simplify, _, new_left}} -> {:simplify, new_left}
    #   {_, _} -> :unknown
    # end
  end

  def new(resource, attribute, predicate, value, relationship_path) do
    case predicate.new(resource, attribute, value) do
      {:ok, predicate} ->
        {:ok,
         %__MODULE__{
           attribute: attribute,
           predicate: predicate,
           relationship_path: relationship_path
         }}

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
            list: :orange,
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
