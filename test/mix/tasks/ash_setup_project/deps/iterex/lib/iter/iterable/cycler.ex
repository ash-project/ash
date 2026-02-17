defmodule Iter.Iterable.Cycler do
  defstruct source: nil, buffer: []

  @moduledoc """
  An iterable which emits elements for ever.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{source: Iterable.t(), buffer: [Iterable.element()]}

  @doc """
  Create an eternal iterable.
  """
  @spec new(Iterable.t()) :: Iterable.t()
  def new(iterable), do: %__MODULE__{source: iterable}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(cycler) do
      case Iterable.next(cycler.source) do
        {:ok, element, iterable} ->
          {:ok, element, %{cycler | source: iterable, buffer: [element | cycler.buffer]}}

        :done when cycler.buffer == [] ->
          :done

        :done ->
          iterator =
            cycler.buffer
            |> :lists.reverse()
            |> IntoIterable.into_iterable()

          next(%{cycler | source: iterator, buffer: []})
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
