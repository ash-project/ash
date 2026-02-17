defmodule Iter.Iterable.Map do
  defstruct iterator: nil, map: nil

  @moduledoc """
  A wrapper around Erlang's `:maps.iterator`.
  """

  alias Iter.{Impl, IntoIterable, Iterable}

  @type t :: %__MODULE__{iterator: :maps.iterator(), map: map}

  @doc """
  Convert a map into a map iterable.
  """
  @spec new(map) :: t
  def new(map) when is_map(map), do: %__MODULE__{iterator: :maps.iterator(map), map: map}

  defimpl Iterable do
    use Impl
    import :erlang, only: [map_get: 2]

    @doc false
    @impl true
    def next(map_iterable) do
      case :maps.next(map_iterable.iterator) do
        :none -> :done
        {key, value, iterator} -> {:ok, {key, value}, %{map_iterable | iterator: iterator}}
      end
    end

    @doc false
    @impl true
    def filter(map_iterable, predicate) when is_function(predicate, 1) do
      new_map = :maps.filter(&filter_predicate(predicate, &1, &2), map_iterable.iterator)

      IntoIterable.into_iterable(new_map)
    end

    defp filter_predicate(predicate, key, value) do
      case predicate.({key, value}) do
        true -> true
        {true, _} -> true
        _ -> false
      end
    end

    @doc false
    @impl true
    def each(map_iterable, fun) when is_function(fun, 1) do
      :maps.foreach(&each_fun(fun, &1, &2), map_iterable.iterator)

      :done
    end

    defp each_fun(fun, key, value), do: fun.({key, value})

    @doc false
    @impl true
    def member?(map, {key, value}) when map_get(key, map.map) == value, do: true
    def member?(_, _), do: false
  end

  defimpl IntoIterable do
    @doc false
    def into_iterable(self), do: self
  end
end
