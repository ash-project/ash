defmodule Iter.Iterable.ByChunker do
  defstruct iterable: nil, chunker: nil, state: :none, buffer: []

  @moduledoc """
  An iterable that chunks elements by subsequent return values of `fun`.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          chunker: (Iterable.element() -> any),
          state: {:ok, any} | :none,
          buffer: []
        }

  @doc """
  Creates an iterable that chunks elements by subsequent return values of `fun`.
  """
  @spec new(Iterable.t(), (Iterable.element() -> any)) :: t
  def new(iterable, chunker) when is_function(chunker, 1),
    do: %__MODULE__{iterable: iterable, chunker: chunker}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(chunk_by) when chunk_by.buffer == [] do
      with {:ok, element, iterable} <- Iterable.next(chunk_by.iterable) do
        next(%{
          chunk_by
          | iterable: iterable,
            state: {:ok, chunk_by.chunker.(element)},
            buffer: [element]
        })
      end
    end

    def next(chunk_by) do
      case Iterable.next(chunk_by.iterable) do
        {:ok, element, iterable} ->
          chunk_result = chunk_by.chunker.(element)

          if {:ok, chunk_result} == chunk_by.state do
            next(%{chunk_by | iterable: iterable, buffer: [element | chunk_by.buffer]})
          else
            {:ok, :lists.reverse(chunk_by.buffer),
             %{chunk_by | iterable: iterable, state: {:ok, chunk_result}, buffer: [element]}}
          end

        :done ->
          {:ok, :lists.reverse(chunk_by.buffer), Iterable.Empty.new()}
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
