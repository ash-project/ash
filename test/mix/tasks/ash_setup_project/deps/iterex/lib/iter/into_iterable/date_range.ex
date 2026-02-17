defimpl Iter.IntoIterable, for: Date.Range do
  @moduledoc false

  @doc false
  def into_iterable(date_range), do: date_range
end
