defmodule Iter.Iterable.HeadTaker do
  defstruct iterable: nil, how_many: nil

  @moduledoc """
  An iterable which takes the first `how_many` elements.
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{iterable: Iterable.t(), how_many: pos_integer()}

  @doc """
  Creates an iterable which takes the first `how_many` elements.
  """
  @spec new(Iterable.t(), pos_integer()) :: t
  def new(iterable, how_many) when is_integer(how_many) and how_many > 0,
    do: %__MODULE__{iterable: iterable, how_many: how_many}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(take) when take.how_many == 0, do: :done

    def next(take) when take.how_many > 0 do
      with {:ok, element, iterable} <- Iterable.next(take.iterable) do
        {:ok, element, %{take | iterable: iterable, how_many: take.how_many - 1}}
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    @impl true
    def into_iterable(self), do: self
  end
end
