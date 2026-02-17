defmodule Iter.Iterable.Stepper do
  defstruct iterable: nil, step_size: nil

  @moduledoc """
  An iterable which advances it's internal iterable by a specific amount each time.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{
          iterable: Iterable.t(),
          step_size: pos_integer()
        }

  @doc """
  Create a new iterable which wraps another iterable.
  """
  @spec new(Iterable.t(), pos_integer()) :: t
  def new(iterable, step_size) when is_integer(step_size) and step_size > 1,
    do: %__MODULE__{iterable: iterable, step_size: step_size}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(stepper) do
      with {:ok, element, iterable} <- Iterable.next(stepper.iterable) do
        {:ok, element, %{stepper | iterable: Iterable.drop(iterable, stepper.step_size - 1)}}
      end
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
