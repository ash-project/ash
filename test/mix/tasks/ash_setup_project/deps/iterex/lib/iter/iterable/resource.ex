defmodule Iter.Iterable.Resource do
  defstruct acc: nil, start_fun: nil, next_fun: nil, after_fun: nil, buffer: []

  @moduledoc """
  An iterable which provides compatibility with `Stream.resource/3`
  """
  alias Iter.{Impl, IntoIterable, Iterable}

  @type acc :: any
  @type start_fun :: (-> acc)
  @type next_fun :: (acc -> {[Iterable.element()], acc} | {:halt, acc})
  @type after_fun :: (acc -> any)
  @type t :: %__MODULE__{
          acc: acc,
          start_fun: nil | start_fun(),
          next_fun: next_fun(),
          after_fun: after_fun(),
          buffer: [Iterable.element()]
        }

  @doc """
  Create an iterable from functions in a manner compatible with `Stream.resource/3`.
  """
  @spec new(start_fun, next_fun, after_fun) :: t
  def new(start_fun, next_fun, after_fun),
    do: %__MODULE__{start_fun: start_fun, next_fun: next_fun, after_fun: after_fun}

  defimpl Iterable do
    use Impl

    @doc false
    @impl true
    def next(%{buffer: [head | tail]} = resource), do: {:ok, head, %{resource | buffer: tail}}

    def next(resource) when is_nil(resource.start_fun) do
      case resource.next_fun.(resource.acc) do
        {[element], acc} ->
          {:ok, element, %{resource | acc: acc}}

        {[head | tail], acc} ->
          {:ok, head, %{resource | buffer: tail, acc: acc}}

        {[], acc} ->
          next(%{resource | acc: acc})

        {:halt, acc} ->
          resource.after_fun.(acc)
          :done
      end
    end

    def next(resource) do
      acc = resource.start_fun.()
      next(%{resource | acc: acc, start_fun: nil})
    end
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
