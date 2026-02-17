defmodule Iter.Iterable.Peeker do
  defstruct iterable: nil, elements: [], size: 0

  @moduledoc """
  The result of "peeking" into an iterable.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          elements: [Iterable.element()],
          size: non_neg_integer()
        }

  @doc """
  Create a new "peeker" iterable.
  """
  @spec new(Iterable.t()) :: t
  def new(iterable), do: %__MODULE__{iterable: iterable}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(%{elements: [], iterable: iterable}), do: Iterable.next(iterable)

    def next(%{elements: [element], iterable: iterable}), do: {:ok, element, iterable}

    def next(%{elements: [element | rest], size: size} = peeker),
      do: {:ok, element, %{peeker | elements: rest, size: size - 1}}

    @doc false
    @impl true
    def peek(%{elements: [], iterable: iterable} = peeker) do
      case Iterable.next(iterable) do
        {:ok, element, iterable} ->
          {:ok, element, %{peeker | elements: [element], size: 1, iterable: iterable}}

        :done ->
          :done
      end
    end

    def peek(%{elements: [element | _]} = peeker) do
      {:ok, element, peeker}
    end

    @doc false
    @impl true
    def peek(peeker, 0), do: {:ok, [], 0, peeker}

    def peek(%{elements: elements, size: size} = peeker, how_many) when how_many == size do
      {:ok, elements, size, peeker}
    end

    def peek(%{elements: elements, size: size} = peeker, how_many)
        when how_many < size and how_many > 0 do
      {:ok, :lists.sublist(elements, how_many), how_many, peeker}
    end

    def peek(%{elements: elements, iterable: iterable, size: size} = peeker, how_many) do
      remaining = how_many - size

      with {:ok, got, extra_elements, iterable} <- do_peek(iterable, remaining, [], 0) do
        elements = elements ++ extra_elements
        size = got + size
        {:ok, elements, size, %{peeker | elements: elements, iterable: iterable, size: size}}
      end
    end

    defp do_peek(iterable, 0, result, so_far), do: {:ok, so_far, :lists.reverse(result), iterable}

    defp do_peek(iterable, remaining, result, so_far) do
      case Iterable.next(iterable) do
        {:ok, element, iterable} ->
          do_peek(iterable, remaining - 1, [element | result], so_far + 1)

        :done when result == [] ->
          :done

        :done ->
          {:ok, so_far, :lists.reverse(result), Iterable.Empty.new()}
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
