defmodule Iter.Iterable.WhileChunker do
  defstruct iterable: nil, acc: nil, chunk_fun: nil, after_fun: nil

  @moduledoc """
  An iterable that chunks based on a chunk function.
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type acc :: any
  @type chunk :: any
  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          acc: nil,
          chunk_fun:
            (Iterable.element(), acc -> {:cont, chunk, acc} | {:cont, acc} | {:halt, acc}),
          after_fun: (acc -> {:cont, chunk, acc} | {:cont, acc})
        }

  @doc """
  Creates an iterable that chunks based on a chunk function.
  """
  @spec new(
          Iterable.t(),
          any,
          (Iterable.element(), acc -> {:cont, chunk, acc} | {:cont, acc} | {:halt, acc}),
          (acc -> {:cont, chunk, acc} | {:cont, acc})
        ) :: t
  def new(iterable, acc, chunk_fun, after_fun)
      when is_function(chunk_fun, 2) and is_function(after_fun, 1),
      do: %__MODULE__{iterable: iterable, acc: acc, chunk_fun: chunk_fun, after_fun: after_fun}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(chunk_while) do
      case Iterable.next(chunk_while.iterable) do
        {:ok, element, iterable} -> handle_chunk_fun(chunk_while, element, iterable)
        :done -> handle_after_fun(chunk_while, chunk_while.acc)
      end
    end

    defp handle_chunk_fun(chunk_while, element, iterable) do
      case chunk_while.chunk_fun.(element, chunk_while.acc) do
        {:cont, chunk, acc} -> {:ok, chunk, %{chunk_while | acc: acc, iterable: iterable}}
        {:cont, acc} -> next(%{chunk_while | acc: acc, iterable: iterable})
        {:halt, acc} -> handle_after_fun(chunk_while, acc)
      end
    end

    defp handle_after_fun(chunk_while, acc) do
      case chunk_while.after_fun.(acc) do
        {:cont, chunk, _acc} -> {:ok, chunk, Iterable.Empty.new()}
        {:cont, _acc} -> :done
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
