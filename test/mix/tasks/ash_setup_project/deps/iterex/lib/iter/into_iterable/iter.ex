defimpl Iter.IntoIterable, for: Iter do
  @moduledoc false
  def into_iterable(iter), do: iter.iterable
end
