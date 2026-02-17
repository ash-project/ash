defmodule Iter.Iterable.FlatMapper do
  defstruct iterable: nil, mapper: nil

  @moduledoc """
  An iterable which works like `map/2` but flattens nested iterables.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type mapper :: (Iterable.t() -> Iterable.t() | Iterable.element())
  @type t :: %__MODULE__{iterable: Iterable.t(), mapper: mapper}

  @doc """
  Creates an iterable which works like `map/2` but flattens nested iterables.
  """
  @spec new(Iterable.t(), mapper) :: t
  def new(iterable, mapper) when is_function(mapper, 1),
    do: %__MODULE__{iterable: iterable, mapper: mapper}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(flat_map) do
      with {:ok, element, iterable} <- Iterable.next(flat_map.iterable) do
        maybe_wrap(flat_map.mapper.(element), %{flat_map | iterable: iterable})
      end
    end

    defp maybe_wrap(element, tail) do
      head = IntoIterable.into_iterable(element)
      concat = Iterable.Concatenator.new([head, tail])
      Iterable.next(concat)
    rescue
      Protocol.UndefinedError -> {:ok, element, tail}
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
