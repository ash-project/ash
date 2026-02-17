defmodule Iter.Iterable.TailTaker do
  defstruct iterable: nil, how_many: nil, buffer: nil

  @moduledoc """
  An iterable which takes `how_many` elements from the end of the iterable.
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          how_many: pos_integer(),
          buffer: nil | [Iterable.element()]
        }

  @doc """
  Creates an iterable which takes `how_many` elements from the end of the iterable.
  """
  @spec new(Iterable.t(), pos_integer()) :: t
  def new(iterable, how_many) when is_integer(how_many) and how_many > 0,
    do: %__MODULE__{iterable: iterable, how_many: how_many}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(take) when is_nil(take.buffer) do
      take
      |> fill_buffer()
      |> next()
    end

    def next(%{buffer: []}), do: :done
    def next(%{buffer: [hd | tail]} = take), do: {:ok, hd, %{take | buffer: tail}}

    defp fill_buffer(take), do: fill_buffer(%{take | buffer: []}, 0)

    defp fill_buffer(take, buffer_size) do
      case Iterable.next(take.iterable) do
        {:ok, element, iterable} when buffer_size == take.how_many ->
          fill_buffer(
            %{take | buffer: [element | :lists.droplast(take.buffer)], iterable: iterable},
            buffer_size
          )

        {:ok, element, iterable} ->
          fill_buffer(
            %{take | buffer: [element | take.buffer], iterable: iterable},
            buffer_size + 1
          )

        :done ->
          %{take | buffer: :lists.reverse(take.buffer), iterable: Iterable.Empty.new()}
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
