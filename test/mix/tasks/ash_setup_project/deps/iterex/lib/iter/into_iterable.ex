defprotocol Iter.IntoIterable do
  @moduledoc """
  A protocol for converting a value into an iterable.

  Required by `Iter.from/1` and others.

  By default this protocol is implemented for `List`, `Map`, `MapSet`, `Range`,
  `Date.Range`, `File.Stream` and `IO.Stream` as well as all iterex's internal
  types.

  Allows the data and the iterable for that data to be separate.  It could be as
  simple as a reference to some external data source and a position marker or it
  may be more efficient to read data in batches and iterate it.  Regardless,
  most types simply return themselves from this function.

  It is important that your `into_iterable/1` callback must not actually start
  iterating, it simply returns a data structure suitable to track the iteration
  of the underlying data.
  """

  @doc """
  Convert a value into an iterable.
  """
  @spec into_iterable(t) :: Iter.Iterable.t()
  def into_iterable(value)
end
