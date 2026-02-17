defmodule Iter.Iterable.Empty do
  defstruct []

  @moduledoc """
  An iterable that's always exhausted
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @opaque t :: %__MODULE__{}

  @doc """
  Creates an iterable that's always exhausted.
  """
  @spec new :: t
  def new, do: %__MODULE__{}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(_), do: :done

    @doc false
    @impl true
    def empty?(_), do: true

    @doc false
    @impl true
    def count(_), do: 0
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
