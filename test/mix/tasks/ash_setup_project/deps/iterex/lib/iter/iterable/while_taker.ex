defmodule Iter.Iterable.WhileTaker do
  defstruct iterable: nil, predicate: nil

  @moduledoc """
  An iterable which emits elements until `predicate` returns `false`.
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{iterable: Iterable.t(), predicate: (Iterable.element() -> boolean)}

  @doc """
  Creates an iterable which emits elements until `predicate` returns `false`.
  """
  @spec new(Iterable.t(), (Iterable.element() -> boolean)) :: t
  def new(iterable, predicate) when is_function(predicate, 1),
    do: %__MODULE__{iterable: iterable, predicate: predicate}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(take_while) do
      with {:ok, element, iterable} <- Iterable.next(take_while.iterable) do
        if take_while.predicate.(element) == true do
          {:ok, element, %{take_while | iterable: iterable}}
        else
          :done
        end
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
