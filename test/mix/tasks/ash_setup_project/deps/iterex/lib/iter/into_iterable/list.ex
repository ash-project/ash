defimpl Iter.IntoIterable, for: List do
  @moduledoc false

  @doc false
  def into_iterable(list), do: list
end
