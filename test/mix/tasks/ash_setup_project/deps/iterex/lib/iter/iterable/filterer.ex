defmodule Iter.Iterable.Filterer do
  defstruct iterable: nil, predicate: nil

  @moduledoc """
  An iterable which drops elements for which `predicate` doesn't return a truthy value.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          predicate: (Iterable.element() -> as_boolean(any))
        }

  @doc """
  Create a new filter iterable.
  """
  @spec new(Iterable.t(), (Iterable.element() -> boolean)) :: Iterable.t()
  def new(iterable, predicate) when is_function(predicate, 1),
    do: %__MODULE__{iterable: iterable, predicate: predicate}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(filter) do
      case Iterable.next(filter.iterable) do
        {:ok, element, iterable} ->
          if filter.predicate.(element) do
            {:ok, element, %{filter | iterable: iterable}}
          else
            next(%{filter | iterable: iterable})
          end

        :done ->
          :done
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
