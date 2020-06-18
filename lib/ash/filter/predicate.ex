defmodule Ash.Filter.Predicate do
  defstruct [:attribute, :relationship_path, :predicate]

  alias Ash.Filter.Predicate.Comparison

  @type predicate :: struct

  @type comparison ::
          :exclusive
          | :inclusive
          | :equal
          | {:simplify, Predicate.predicate()}
          | {:simplify, Predicate.predicate(), Predicate.predicate()}

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

  @spec compare(predicate(), predicate()) :: comparison()
  def compare(left, right) do
    with :unknown <- left.__struct__.compare(right),
         :unknown <- right.__struct__.compare(left) do
      :exclusive
    end
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
