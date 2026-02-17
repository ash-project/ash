defmodule Iter.Iterable.EveryMapper do
  defstruct iterable: nil, every: nil, count: 0, mapper: nil

  @moduledoc """
  An iterable which maps every `nth` element in the iterable.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          every: non_neg_integer,
          count: non_neg_integer,
          mapper: (Iterable.element() -> any)
        }

  @doc """
  Creates a new iterable which maps every `nth` element in the iterable.
  """
  @spec new(Iterable.t(), non_neg_integer, (Iterable.element() -> any)) :: t
  def new(iterable, 0, _mapper), do: iterable
  def new(iterable, 1, mapper), do: Iterable.Mapper.new(iterable, mapper)

  def new(iterable, nth, mapper) when is_integer(nth) and nth > 1,
    do: %__MODULE__{iterable: iterable, every: nth, mapper: mapper}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(map_every) when map_every.count == 0 do
      with {:ok, element, iterable} <- Iterable.next(map_every.iterable) do
        {:ok, map_every.mapper.(element),
         %{map_every | iterable: iterable, count: map_every.every - 1}}
      end
    end

    def next(map_every) do
      with {:ok, element, iterable} <- Iterable.next(map_every.iterable) do
        {:ok, element, %{map_every | iterable: iterable, count: map_every.count - 1}}
      end
    end
  end

  defimpl IntoIterable do
    def into_iterable(self), do: self
  end
end
