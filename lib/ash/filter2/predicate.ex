defmodule Ash.Filter2.Predicate do
  defstruct [:resource, :attribute, :relationship_path, :predicate]

  alias Ash.Filter2.Predicate.Comparison

  @type predicate :: struct

  @type comparison ::
          :exclusive
          | :inclusive
          | :equal
          | {:simplify, Predicate.predicate()}
          | {:simplify, Predicate.predicate(), Predicate.predicate()}

  @type t :: %__MODULE__{
          resource: Ash.resource(),
          attribute: Ash.attribute(),
          relationship_path: list(atom),
          predicate: predicate
        }

  @callback new(Ash.type(), term) :: {:ok, struct} | {:error, term}
  @callback inspect(String.t(), predicate(), Inspect.Opts.t()) :: Inspect.Algebra.t()
  @callback compare(predicate(), predicate()) :: Comparison.comparison()

  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Filter2.Predicate

      @spec compare(Ash.Filter2.Predicate.predicate(), Ash.Filter2.Predicate.predicate()) ::
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
    case predicate.new(attribute.type, value) do
      {:ok, predicate} ->
        {:ok,
         %__MODULE__{
           resource: resource,
           attribute: attribute,
           predicate: predicate,
           relationship_path: relationship_path
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end

defimpl Inspect, for: Ash.Filter2.Predicate do
  def inspect(
        %{attribute: attribute, relationship_path: relationship_path, predicate: predicate},
        opts
      ) do
    field =
      case relationship_path do
        [] -> to_string(attribute.name)
        path -> Enum.join(path, ".") <> "." <> to_string(attribute.name)
      end

    predicate.__struct__.inspect(field, predicate, opts)
  end
end
