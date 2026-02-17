defmodule Iter.Iterable.HeadDropper do
  defstruct iterable: nil, how_many: nil

  @moduledoc """
  An iterable which drops the first `how_many` elements.
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{iterable: Iterable.t(), how_many: non_neg_integer()}

  @doc """
  Creates an iterable which drops the first `how_many` elements.
  """
  @spec new(Iterable.t(), pos_integer()) :: t
  def new(iterable, how_many) when is_integer(how_many) and how_many > 0,
    do: %__MODULE__{iterable: iterable, how_many: how_many}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(drop) when drop.how_many == 0, do: Iterable.next(drop.iterable)

    def next(drop) do
      with {:ok, _element, iterable} <- Iterable.next(drop.iterable) do
        next(%{drop | iterable: iterable, how_many: drop.how_many - 1})
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
