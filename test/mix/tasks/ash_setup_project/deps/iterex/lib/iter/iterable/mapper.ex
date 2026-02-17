defmodule Iter.Iterable.Mapper do
  defstruct iterable: nil, mapper: nil

  @moduledoc """
  An iterable which applies a mapper function to all it's elements and returns their new values.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          mapper: (Iterable.element() -> Iterable.element())
        }

  @doc """
  Create a new map iterable.
  """
  @spec new(Iterable.t(), (Iterable.element() -> Iterable.element())) :: Iterable.t()
  def new(iterable, mapper) when is_function(mapper, 1),
    do: %__MODULE__{iterable: iterable, mapper: mapper}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(mapper) do
      case Iterable.next(mapper.iterable) do
        {:ok, element, iterable} -> {:ok, mapper.mapper.(element), %{mapper | iterable: iterable}}
        :done -> :done
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
