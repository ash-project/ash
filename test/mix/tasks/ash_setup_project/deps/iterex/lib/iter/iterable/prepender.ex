defmodule Iter.Iterable.Prepender do
  defstruct iterable: nil, element: nil

  @moduledoc """
  An iterable which prepends a single element to the end of another iterable.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{iterable: Iterable.t(), element: Iterable.element()}

  @doc """
  Create a new prepender iterable.
  """
  @spec new(Iterable.t(), Iterable.element()) :: t
  def new(iterable, element), do: %__MODULE__{iterable: iterable, element: element}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(prepender), do: {:ok, prepender.element, prepender.iterable}

    @doc false
    @impl true
    def count(prepender), do: Iterable.count(prepender.iterable) + 1

    @doc false
    @impl true
    def empty?(_), do: false
  end

  defimpl IntoIterable do
    def into_iterable(self), do: self
  end
end
