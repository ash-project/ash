defmodule Iter.Iterable.Zipper do
  defstruct iterables: nil, zipper: nil

  @moduledoc """
  An iterable which returns the elements of two iterables as tuple pairs.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterables: [Iterable.t()],
          zipper: ([Iterable.element()] -> any)
        }

  @doc """
  Create a new zip out of two iterables.
  """
  @spec new(Iterable.t(), ([Iterable.element()] -> any)) :: Iterable.t()
  def new(iterables, zipper) when is_function(zipper, 1),
    do: %__MODULE__{iterables: Iterable.to_list(iterables), zipper: zipper}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(zipper) do
      with {:ok, element, iterables} <- get_next(zipper.iterables, [], [], zipper.zipper) do
        {:ok, element, %{zipper | iterables: iterables}}
      end
    end

    defp get_next([], iterables, elements, zipper) do
      elements =
        elements
        |> :lists.reverse()
        |> then(zipper)

      iterables =
        iterables
        |> :lists.reverse()

      {:ok, elements, iterables}
    end

    defp get_next([head | tail], iterables, elements, zipper) do
      case Iterable.next(head) do
        {:ok, element, head} -> get_next(tail, [head | iterables], [element | elements], zipper)
        :done -> :done
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
