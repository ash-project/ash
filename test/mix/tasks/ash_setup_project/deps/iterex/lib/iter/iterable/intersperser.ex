defmodule Iter.Iterable.Intersperser do
  defstruct iterable: nil, separator: nil, next: :init

  @moduledoc """
  An iterable which places a separator value in between consecutive elements.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          separator: any,
          next: {:ok, Iterable.element()} | :none | :init
        }

  @doc """
  Create a new intersperser iterable out of an iterable and a separator
  """
  @spec new(Iterable.t(), any) :: Iterable.t()
  def new(iterable, separator),
    do: %__MODULE__{iterable: iterable, separator: separator}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(%{next: :init} = intersperser) do
      with {:ok, element, iterable} <- Iterable.next(intersperser.iterable) do
        {:ok, element, %{intersperser | next: :none, iterable: iterable}}
      end
    end

    def next(%{next: {:ok, element}} = intersperser) do
      {:ok, element, %{intersperser | next: :none}}
    end

    def next(intersperser) do
      with {:ok, element, iterable} <- Iterable.next(intersperser.iterable) do
        {:ok, intersperser.separator, %{intersperser | next: {:ok, element}, iterable: iterable}}
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
