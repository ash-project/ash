defmodule Iter.Iterable.TailDropper do
  defstruct iterable: nil, how_many: nil, buffer: nil

  @moduledoc """
  An iterable which drops the last `how_many` elements.
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          how_many: non_neg_integer(),
          buffer: nil | Iterable.t()
        }

  @doc """
  Creates an iterable which drops the last `how_many` elements.
  """
  @spec new(Iterable.t(), pos_integer()) :: t
  def new(iterable, how_many) when is_integer(how_many) and how_many > 0,
    do: %__MODULE__{iterable: iterable, how_many: how_many}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(drop) when is_nil(drop.buffer) do
      with {:ok, drop} <- fill_buffer(drop) do
        next(drop)
      end
    end

    def next(drop) do
      with {:ok, to_append, iterable} <- Iterable.next(drop.iterable),
           {:ok, to_emit, buffer} <- Iterable.next(drop.buffer) do
        {:ok, to_emit, %{drop | iterable: iterable, buffer: Iterable.append(buffer, to_append)}}
      end
    end

    # Ensure that at least `how_many` elements exist.
    defp fill_buffer(drop), do: fill_buffer(%{drop | buffer: Iterable.Empty.new()}, 0)

    defp fill_buffer(drop, buffer_size) when buffer_size == drop.how_many,
      do: {:ok, drop}

    defp fill_buffer(drop, buffer_size) do
      with {:ok, element, iterable} <- Iterable.next(drop.iterable) do
        fill_buffer(
          %{drop | iterable: iterable, buffer: Iterable.append(drop.buffer, element)},
          buffer_size + 1
        )
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
