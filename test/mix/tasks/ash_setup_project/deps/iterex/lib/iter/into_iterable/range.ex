defimpl Iter.IntoIterable, for: Range do
  @moduledoc false

  @doc false
  def into_iterable(range), do: range
end
