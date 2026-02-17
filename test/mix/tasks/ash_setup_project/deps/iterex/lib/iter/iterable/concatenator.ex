defmodule Iter.Iterable.Concatenator do
  defstruct current_iterable: nil, outer_iterable: nil

  @moduledoc """
  An iterable which can concatenate a number of iterables.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{outer_iterable: Iterable.t(), current_iterable: Iterable.t()}

  @doc """
  Create a new concatenator out of an iterable of iterables.
  """
  @spec new(Iterable.t()) :: Iterable.t()
  def new(iterable), do: %__MODULE__{outer_iterable: iterable}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(concat) when is_nil(concat.current_iterable) do
      case Iterable.next(concat.outer_iterable) do
        {:ok, element, iterable} ->
          next(%{concat | current_iterable: element, outer_iterable: iterable})

        :done ->
          :done
      end
    end

    def next(concat) do
      case Iterable.next(concat.current_iterable) do
        {:ok, element, iterable} -> {:ok, element, %{concat | current_iterable: iterable}}
        :done -> next(%{concat | current_iterable: nil})
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
