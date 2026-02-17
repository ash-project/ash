defmodule Iter.Iterable.Appender do
  defstruct iterable: nil, element: nil

  @moduledoc """
  An iterable which appends a single element to the end of another iterable.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{iterable: Iterable.t(), element: {:ok, Iterable.element()} | :done}

  @doc """
  Create a new appender iterable.
  """
  @spec new(Iterable.t(), Iterable.element()) :: t
  def new(iterable, element), do: %__MODULE__{iterable: iterable, element: {:ok, element}}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(appender) do
      case {Iterable.next(appender.iterable), appender.element} do
        {{:ok, element, iterable}, _} ->
          {:ok, element, %{appender | iterable: iterable}}

        {:done, :done} ->
          :done

        {:done, {:ok, element}} ->
          {:ok, element, %{appender | iterable: Iterable.Empty.new(), element: :done}}
      end
    end
  end

  defimpl IntoIterable do
    def into_iterable(self), do: self
  end
end
