defmodule Iter.Iterable.Deduper do
  defstruct iterable: nil, last_result: nil, fun: nil

  @moduledoc """
  An iterable that only emits elements if they are different from the previous element.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          last_result: nil | Iterable.element(),
          fun: (Iterable.element() -> any)
        }

  @doc """
  Creates an iterable that only emits elements if they are different from the previous element.
  """
  @spec new(Iterable.t(), (Iterable.element() -> any)) :: Iterable.t()
  def new(iterable, fun \\ &Function.identity/1), do: %__MODULE__{iterable: iterable, fun: fun}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(deduper) do
      with {:ok, element, iterable} <- Iterable.next(deduper.iterable) do
        case deduper.fun.(element) do
          result when result == deduper.last_result ->
            next(%{deduper | iterable: iterable})

          result ->
            {:ok, element, %{deduper | iterable: iterable, last_result: result}}
        end
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
