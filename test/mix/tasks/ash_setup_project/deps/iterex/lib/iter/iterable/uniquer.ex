defmodule Iter.Iterable.Uniquer do
  defstruct seen: :sets.new(version: 2), iterable: nil

  @moduledoc """
  An iterable that only emits unique elements.
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{seen: :sets.set(Iterable.element()), iterable: Iterable.t()}

  @doc """
  Creates an iterable that only emits unique elements.
  """
  def new(iterable), do: %__MODULE__{iterable: iterable}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(uniq) do
      with {:ok, element, iterable} <- Iterable.next(uniq.iterable) do
        if :sets.is_element(element, uniq.seen) do
          next(%{uniq | iterable: iterable})
        else
          {:ok, element, %{uniq | seen: :sets.add_element(element, uniq.seen)}}
        end
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
