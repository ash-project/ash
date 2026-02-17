defmodule Iter.Iterable.EveryDropper do
  defstruct iterable: nil, every: nil, count: 0

  @moduledoc """
  An iterable which drops every `nth` element from the iterable.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          every: non_neg_integer,
          count: non_neg_integer
        }

  @doc """
  Creates an iterable which drops every `nth` element from the iterable.
  """
  @spec new(Iterable.t(), non_neg_integer) :: t
  def new(iterable, 0), do: iterable
  def new(_iterable, 1), do: Iterable.Empty.new()

  def new(iterable, every) when is_integer(every) and every > 1,
    do: %__MODULE__{iterable: iterable, every: every}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(drop_every) when drop_every.count == 0 do
      with {:ok, _element, iterable} <- Iterable.next(drop_every.iterable),
           {:ok, element, iterable} <- Iterable.next(iterable) do
        {:ok, element, %{drop_every | iterable: iterable, count: drop_every.every - 2}}
      end
    end

    def next(drop_every) do
      with {:ok, element, iterable} <- Iterable.next(drop_every.iterable) do
        {:ok, element, %{drop_every | iterable: iterable, count: drop_every.count - 1}}
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
