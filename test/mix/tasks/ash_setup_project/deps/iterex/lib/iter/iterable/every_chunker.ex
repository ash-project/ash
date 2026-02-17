defmodule Iter.Iterable.EveryChunker do
  defstruct iterable: nil, count: nil, step: nil, leftover: nil

  @moduledoc """
  An iterable that chunks into `count` size elements, where each new chunk
  starts `step` elements into the enumerable.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          count: pos_integer,
          step: pos_integer,
          leftover: Iterable.t() | :discard
        }

  @doc """
  Creates an iterable that chunks into `count` size elements, where each new
  chunk starts `step` elements into the enumerable.
  """
  @spec new(Iterable.t(), pos_integer, pos_integer, Iterable.t() | :discard) :: t
  def new(iterable, count, step, leftover)
      when is_integer(count) and count > 0 and is_integer(step) and step > 0,
      do: %__MODULE__{
        iterable: iterable,
        count: count,
        step: step,
        leftover: leftover
      }

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(chunk_every) do
      how_many = chunk_every.count

      case Iterable.peek(chunk_every.iterable, how_many) do
        {:ok, elements, ^how_many, iterable} ->
          {:ok, elements, %{chunk_every | iterable: Iterable.drop(iterable, chunk_every.step)}}

        {:ok, _elements, _how_many, _iterable} when chunk_every.leftover == :discard ->
          :done

        {:ok, _elements, 0, _iterable} ->
          :done

        {:ok, _elements, _how_many, iterable} ->
          elements =
            [iterable, chunk_every.leftover]
            |> Iterable.concat()
            |> Iterable.take_head(chunk_every.count)
            |> Iterable.to_list()

          {:ok, elements, Iterable.Empty.new()}

        :done ->
          :done
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
