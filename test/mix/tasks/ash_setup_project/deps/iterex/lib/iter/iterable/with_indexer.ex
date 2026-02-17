defmodule Iter.Iterable.WithIndexer do
  defstruct iterable: nil, count: 0

  @moduledoc """
  Creates an iterator which emits the current iteration count as well as the next value.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          count: 0
        }

  @doc """
  Create a new filter iterable.
  """
  @spec new(Iterable.t()) :: Iterable.t()
  def new(iterable), do: %__MODULE__{iterable: iterable}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(enumerator) do
      with {:ok, element, iterable} <- Iterable.next(enumerator.iterable) do
        {:ok, {element, enumerator.count},
         %{enumerator | iterable: iterable, count: enumerator.count + 1}}
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
