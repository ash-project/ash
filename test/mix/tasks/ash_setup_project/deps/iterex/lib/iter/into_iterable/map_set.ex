defimpl Iter.IntoIterable, for: MapSet do
  @moduledoc false

  @doc false
  def into_iterable(set), do: set
end
