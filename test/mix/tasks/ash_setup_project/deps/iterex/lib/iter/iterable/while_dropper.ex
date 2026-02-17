defmodule Iter.Iterable.WhileDropper do
  defstruct iterable: nil, predicate: nil

  @moduledoc """
  An iterable that drops elements until `predicate` returns a truthy value.
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{iterable: Iterable.t(), predicate: (Iterable.element() -> boolean)}

  @doc """
  Creates an iterable that drops elements until `predicate` returns a truthy
  value.
  """
  @spec new(Iterable.t(), (Iterable.element() -> boolean)) :: t
  def new(iterable, predicate) when is_function(predicate, 1),
    do: %__MODULE__{iterable: iterable, predicate: predicate}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(drop_while) do
      with {:ok, element, iterable} <- Iterable.next(drop_while.iterable) do
        if drop_while.predicate.(element) do
          next(%{drop_while | iterable: iterable})
        else
          {:ok, element, iterable}
        end
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
