defimpl Iter.IntoIterable, for: Map do
  @moduledoc false

  @doc false
  def into_iterable(map), do: Iter.Iterable.Map.new(map)
end
