defimpl Iter.Iterable, for: MapSet do
  @moduledoc false

  ## Note:
  #
  # This implementation knows that the `map` key in `MapSet` contains a version
  # 2 `:sets` record and cheats accordingly.
  # If the internals of either change then I guess the tests will fail.

  use Iter.Impl

  @doc false
  @impl true
  def next(map_set) when map_size(map_set.map) == 0, do: :done

  def next(map_set) do
    with {:ok, element} <- get_next_element(map_set) do
      {:ok, element, %{map_set | map: :sets.del_element(element, map_set.map)}}
    end
  end

  defp get_next_element(map_set) do
    map_set.map
    |> :maps.iterator()
    |> :maps.next()
    |> case do
      :none -> :done
      {key, _, _iterator} -> {:ok, key}
    end
  end

  @doc false
  def count(map_set), do: MapSet.size(map_set)

  @doc false
  def dedup(map_set), do: map_set

  @doc false
  def uniq(map_set), do: map_set

  @doc false
  def member?(map_set, element), do: MapSet.member?(map_set, element)
end
