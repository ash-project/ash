defmodule Iter do
  @moduledoc """
  Functions for working with iterators.

  An iterator is a struct that wraps any value which implements the
  `Iter.Iterable` protocol. Lists, maps and ranges are all common data types
  which can be used as iterators.

  ## Explicit conversion

  Whilst the `Iter.Iterable` protocol is designed to work with many different
  types, you must explicitly convert your iterables into an iterator using
  `Iter.from/1`.  This allows for easy pattern matching of iterators as well as
  for default implementations of Elixir's `Enum` and `Collectable` protocols.

  Any value passed to `Iter.from/1` must implement the `Iter.IntoIterable`
  protocol.

  ## Lazy by default

  Like Elixir's `Stream`, an `Iter` is lazy by default.  Any function which
  returns an `Iter.t` does so by simply composing iterables on top of each
  other.  No iteration is performed until it is needed, and then only the
  minimum amount needed to return the result.
  """

  defstruct iterable: nil
  alias Iter.{IntoIterable, Iterable}

  @type t :: %__MODULE__{iterable: Iterable.t()}
  @type element :: any
  @type predicate :: (element -> as_boolean(any))
  @type mapper :: (element -> any)
  @type sorter :: (element, element -> as_boolean(any))

  @doc """
  Is the passed value an iterator?
  """
  @spec is_iter(any) :: Macro.output()
  defguard is_iter(iter) when is_struct(iter, __MODULE__)

  @doc """
  Returns `true` if all elements in the iterator are truthy.

  ## Examples

      iex> [1, 2, false]
      ...> |> Iter.from()
      ...> |> Iter.all?()
      false

      iex> [1, 2, nil]
      ...> |> Iter.from()
      ...> |> Iter.all?()
      false

      iex> [1, 2, 3]
      ...> |> Iter.from()
      ...> |> Iter.all?()
      true
  """
  @spec all?(t) :: boolean
  def all?(iter), do: all?(iter, &(&1 not in [nil, false]))

  @doc """
  Returns `true` if `fun.(element)` is truthy for all elements in the iterator.

  Iterates over the iterator and invokes `fun` on each element. If `fun` ever
  returns a falsy value (`false` or `nil`), iteration stops immediately and
  `false` is returned.  Otherwise `true` is returned.

  ## Examples

      iex> [2, 4, 6]
      ...> |> Iter.from()
      ...> |> Iter.all?(&(rem(&1, 2) == 0))
      true

      iex> [2, 3, 4]
      ...> |> Iter.from()
      ...> |> Iter.all?(&(rem(&1, 2) == 0))
      false

      iex> []
      ...> |> Iter.from()
      ...> |> Iter.all?()
      true
  """
  @spec all?(t, predicate) :: boolean
  def all?(iter, predicate) when is_iter(iter) and is_function(predicate, 1),
    do: Iterable.all?(iter.iterable, predicate)

  @doc """
  Returns `true` if at least one element in the iterator is truthy.

  When an element is a truthy value (neither `false` nor `nil`) iteration stops
  immediately and `true` is returned.  In all other cases `false` is returned.

  ## Examples

      iex> [false, false, false]
      ...> |> Iter.from()
      ...> |> Iter.any?()
      false

      iex> [false, true, false]
      ...> |> Iter.from()
      ...> |> Iter.any?()
      true

      iex> []
      ...> |> Iter.from()
      ...> |> Iter.any?()
      false
  """
  @spec any?(t) :: boolean
  def any?(iter), do: any?(iter, &(&1 not in [nil, false]))

  @doc """
  Returns `true` if `fun.(element)` is truthy for at least one element in the
  iterator.

  Consumes the iterator and invokes `fun` on each element.  When an invocation
  of `fun` returns a truthy value (neither `false` nor `nil`) iteration stops
  immediately and `true` is returned.  In all other cases `false` is returned.

  ## Examples

      iex> [2, 4, 6]
      ...> |> Iter.from()
      ...> |> Iter.any?(&(rem(&1, 2) == 1))
      false

      iex> [2, 3, 4]
      ...> |> Iter.from()
      ...> |> Iter.any?(&(rem(&1, 2) == 1))
      true

      iex> []
      ...> |> Iter.from()
      ...> |> Iter.any?(&(rem(&1, 2) == 1))
      false
  """
  @spec any?(t, predicate) :: boolean
  def any?(iter, predicate) when is_iter(iter) and is_function(predicate, 1),
    do: Iterable.any?(iter.iterable, predicate)

  @doc """
  Append a new element to the end of the iterable.

  ## Example

      iex> 1..3
      ...> |> Iter.from()
      ...> |> Iter.append(4)
      ...> |> Iter.to_list()
      [1, 2, 3, 4]
  """
  @spec append(t, element) :: t
  def append(iter, element) when is_iter(iter),
    do: iter.iterable |> Iterable.append(element) |> new()

  @doc """
  Return the element `index` items from the beginning of the iterator.

  Works by advancing the iterator the specified number of elements and then
  returning the element requested and an iterator of the remaining elements.

  ## Return values

  - `{:ok, element, new_iterator}` - the element requested and the iterator of
    the remaining elements.
  - `:done` - the iterator was exhausted before the element was found.

  ## Examples

      iex> 10..20
      ...> |> Iter.from()
      ...> |> Iter.at(5)
      {:ok, 15, Iter.from(16..20)}
  """
  @spec at(t, non_neg_integer) :: {:ok, element, t} | :done
  def at(iter, index) when is_iter(iter) and is_integer(index) and index >= 0 do
    with {:ok, element, iterable} <- Iterable.at(iter.iterable, index) do
      {:ok, element, new(iterable)}
    end
  end

  @doc """
  Chunks the iterator by buffering elements for which `fun` returns the same
  value.

  Elements are only emitted when `fun` returns a new value or `iterable` is
  exhausted.

  ## Examples

      iex> [1, 2, 2, 3, 4, 4, 6, 7, 7]
      ...> |> Iter.from()
      ...> |> Iter.chunk_by(&(rem(&1, 2) == 1))
      ...> |> Iter.to_list()
      [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
  """
  @spec chunk_by(t, (element -> any)) :: t
  def chunk_by(iter, fun) when is_iter(iter) and is_function(fun, 1) do
    iter.iterable
    |> Iterable.chunk_by(fun)
    |> new()
  end

  @doc """
  Shortcut to `chunk_every(iterable, count, count)`.
  """
  @spec chunk_every(t, pos_integer) :: t
  def chunk_every(iter, count) when is_iter(iter) and is_integer(count) and count > 0 do
    iter.iterable
    |> Iterable.chunk_every(count, count, empty())
    |> Iterable.map(&new/1)
    |> new()
  end

  @doc """
  Consumes the iterator in chunks, containing `count` elements each, where each
  new chunk steps `step` elements into the iterator.

  `step` is optional and, if not passed defaults to `count`, i.e. chunks do not
  overlap.  Chunking will stop as soon as the iterable is exhausted or when we
  emit an incomplete chunk.

  If the last chunk does not have `chunk` elements to fill the chunk, elements
  are taken from `leftover` to fill in the chunk, if `leftover` does not have
  enough elements to fill the chunk, then a partial chunk is returned with less
  than `count` elements.

  If `:discard` is given in `leftover` the last chunk is discarded unless it has
  exactly `count` elements.

  ## Examples

      iex> [a, b, c] = [1, 2, 3, 4, 5, 6]
      ...> |> Iter.from()
      ...> |> Iter.chunk_every(2)
      ...> |> Iter.to_list()
      iex> Iter.to_list(a)
      [1, 2]
      iex> Iter.to_list(b)
      [3, 4]
      iex> Iter.to_list(c)
      [5, 6]

      iex> [a, b] = [1, 2, 3, 4, 5, 6]
      ...> |> Iter.from()
      ...> |> Iter.chunk_every(3, 2, :discard)
      ...> |> Iter.to_list()
      iex> Iter.to_list(a)
      [1, 2, 3]
      iex> Iter.to_list(b)
      [3, 4, 5]

      iex> [a, b, c] = [1, 2, 3, 4, 5, 6]
      ...> |> Iter.from()
      ...> |> Iter.chunk_every(3, 2, [7] |> Iter.from())
      ...> |> Iter.to_list()
      iex> Iter.to_list(a)
      [1, 2, 3]
      iex> Iter.to_list(b)
      [3, 4, 5]
      iex> Iter.to_list(c)
      [5, 6, 7]

      iex> [a, b] = [1, 2, 3, 4, 5, 6]
      ...> |> Iter.from()
      ...> |> Iter.chunk_every(3, 3, [] |> Iter.from())
      ...> |> Iter.to_list()
      iex> Iter.to_list(a)
      [1, 2, 3]
      iex> Iter.to_list(b)
      [4, 5, 6]

      iex> [a, b] = [1, 2, 3, 4]
      ...> |> Iter.from()
      ...> |> Iter.chunk_every(3, 3, [0] |> Iter.from() |> Iter.cycle())
      ...> |> Iter.to_list()
      iex> Iter.to_list(a)
      [1, 2, 3]
      iex> Iter.to_list(b)
      [4, 0, 0]
  """
  @spec chunk_every(t, pos_integer, pos_integer, t | :discard) :: Enumerable.t()
  def chunk_every(iter, count, step), do: chunk_every(iter, count, step, empty())

  def chunk_every(iter, count, step, :discard)
      when is_iter(iter) and is_integer(count) and is_integer(step) and count > 0 and step > 0 do
    iter.iterable
    |> Iterable.chunk_every(count, step, :discard)
    |> Iterable.map(&new/1)
    |> new()
  end

  def chunk_every(iter, count, step, leftover)
      when is_iter(iter) and is_iter(leftover) and is_integer(count) and is_integer(step) and
             count > 0 and step > 0 do
    iter.iterable
    |> Iterable.chunk_every(count, step, leftover.iterable)
    |> Iterable.map(&new/1)
    |> new()
  end

  @doc """
  Chunks the iterator with fine grained control of when every chunk is emitted.

  `chunk_fun` receives the current element and the accumulator and must return
  `{:cont, element, acc}` to emit the given chunk and continue with accumulator
  or `{:cont, acc}` to not emit any chunk and continue with the return
  accumulator.

  `after_fun` is invoked when iteration is done and must also return `{:cont,
  element, acc}` or `{:cont, acc}`.

  ## Examples

      iex> chunk_fun = fn element, acc ->
      ...>   if rem(element, 2) == 0 do
      ...>     {:cont, Enum.reverse([element | acc]), []}
      ...>   else
      ...>     {:cont, [element | acc]}
      ...>   end
      ...> end
      iex> after_fun = fn
      ...>   [] -> {:cont, []}
      ...>   acc -> {:cont, Enum.reverse(acc), []}
      ...> end
      iex> 1..10
      ...> |> Iter.from()
      ...> |> Iter.chunk_while([], chunk_fun, after_fun)
      ...> |> Iter.to_list()
      [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
  """
  @spec chunk_while(
          t,
          acc,
          (element, acc -> {:cont, chunk, acc} | {:cont, acc} | {:halt, acc}),
          (acc -> {:cont, chunk, acc} | {:cont, acc})
        ) :: t
        when acc: any, chunk: any
  def chunk_while(iter, acc, chunk_fun, after_fun)
      when is_iter(iter) and is_function(chunk_fun, 2) and is_function(after_fun, 1) do
    iter.iterable
    |> Iterable.chunk_while(acc, chunk_fun, after_fun)
    |> new()
  end

  @doc """
  Creates an iterator that concatenates an iterator of iterators.

  ## Example

      iex> [Iter.from(1..2), Iter.from(3..4)]
      ...> |> Iter.from()
      ...> |> Iter.concat()
      ...> |> Iter.to_list()
      [1, 2, 3, 4]
  """
  @spec concat(t) :: t
  def concat(iter) when is_iter(iter) do
    iter.iterable
    |> Iterable.map(&IntoIterable.into_iterable/1)
    |> Iterable.concat()
    |> new()
  end

  @doc """
  Creates an iterator that iterates the first argument, followed by the second argument.

  ## Example

      iex> lhs = Iter.from(1..3)
      ...> rhs = Iter.from(4..6)
      ...> Iter.concat(lhs, rhs) |> Iter.to_list()
      [1, 2, 3, 4, 5, 6]
  """
  @spec concat(t, t) :: t
  def concat(lhs, rhs) when is_iter(lhs) and is_iter(rhs) do
    [lhs.iterable, rhs.iterable]
    |> Iterable.concat()
    |> new()
  end

  @doc """
  Counts the elements in iterator stopping at `limit`.

  ## Examples

      iex> 1..20
      ...> |> Iter.from()
      ...> |> Iter.count_until(5)
      {:ok, 5, Iter.from(6..20)}

      iex> 1..3
      ...> |> Iter.from()
      ...> |> Iter.count_until(5)
      {:ok, 3, Iter.empty()}

      iex> []
      ...> |> Iter.from()
      ...> |> Iter.count_until(5)
      {:ok, 0, Iter.empty()}
  """
  @spec count_until(t, pos_integer) :: {:ok, non_neg_integer, t}
  def count_until(iter, limit) when is_iter(iter) and is_integer(limit) and limit > 0,
    do: do_count_until(iter.iterable, limit, 0)

  defp do_count_until(iter, limit, limit), do: {:ok, limit, new(iter)}

  defp do_count_until(iter, limit, so_far) do
    case Iterable.next(iter) do
      {:ok, _element, iter} -> do_count_until(iter, limit, so_far + 1)
      :done -> {:ok, so_far, empty()}
    end
  end

  @doc """
  Counts the elements of iterator for which `predicate` returns a truthy value, stopping at `limit`.

  ## Examples

      iex> 1..20
      ...> |> Iter.from()
      ...> |> Iter.count_until(&(rem(&1, 2) == 0), 7)
      {:ok, 7, Iter.from(15..20)}

      iex> 1..20
      ...> |> Iter.from()
      ...> |> Iter.count_until(&(rem(&1, 2) == 0), 11)
      {:ok, 10, Iter.empty()}
  """
  @spec count_until(t, predicate, pos_integer) :: {:ok, non_neg_integer, t}
  def count_until(iter, predicate, limit)
      when is_iter(iter) and is_function(predicate, 1) and is_integer(limit) and limit > 0,
      do: do_count_until_with(iter.iterable, predicate, limit, 0)

  defp do_count_until_with(iter, _predicate, limit, limit), do: {:ok, limit, new(iter)}

  defp do_count_until_with(iter, predicate, limit, so_far) do
    case Iterable.next(iter) do
      {:ok, element, iter} ->
        if predicate.(element) do
          do_count_until_with(iter, predicate, limit, so_far + 1)
        else
          do_count_until_with(iter, predicate, limit, so_far)
        end

      :done ->
        {:ok, so_far, empty()}
    end
  end

  @doc """
  Count the number of elements remaining in the iterator.

  Some iterators can be counted without consuming the iterator, but most cannot
  and you should consider the iterator passed to this function as having been
  exhausted.

  ## Example

      iex> 1..10
      ...> |> Iter.from()
      ...> |> Iter.count()
      10
  """
  @spec count(t) :: non_neg_integer
  def count(iter) when is_iter(iter),
    do: Iterable.count(iter.iterable)

  @doc """
  Count the number of elements for which `fun` returns a truthy value.

  ## Example

      iex> 1..5
      ...> |> Iter.from()
      ...> |> Iter.count(&(rem(&1, 2) == 0))
      2
  """
  @spec count(t, (element -> as_boolean(any))) :: non_neg_integer
  def count(iter, fun) when is_iter(iter) and is_function(fun, 1),
    do: Iterable.count(iter.iterable, fun)

  @doc """
  Create an iterator that cycles it's elements eternally.

      iex> [:a, :b, :c]
      ...> |> Iter.from()
      ...> |> Iter.cycle()
      ...> |> Iter.take(5)
      ...> |> Iter.to_list()
      [:a, :b, :c, :a, :b]
  """
  @spec cycle(t) :: t | no_return
  def cycle(iter) when is_iter(iter),
    do: iter.iterable |> Iterable.cycle() |> new()

  @doc """
  Remove consecutive elements for which `fun` returns duplicate values from the iterator.

  ## Example

    iex> [{1, :a}, {2, :b}, {2, :c}, {1, :a}]
    ...> |> Iter.from()
    ...> |> Iter.dedup_by(&elem(&1, 0))
    ...> |> Iter.to_list()
    [{1, :a}, {2, :b}, {1, :a}]
  """
  @spec dedup_by(t, (element -> any)) :: t
  def dedup_by(iter, fun) when is_iter(iter) and is_function(fun, 1),
    do: iter.iterable |> Iterable.dedup_by(fun) |> new()

  @doc """
  Remove consecutive duplicate elements from the iterator.

  ## Example

      iex> [1, 1, 2, 3, 3, 4, 5, 4]
      ...> |> Iter.from()
      ...> |> Iter.dedup()
      ...> |> Iter.to_list()
      [1, 2, 3, 4, 5, 4]
  """
  @spec dedup(t) :: t
  def dedup(iter) when is_iter(iter),
    do: iter.iterable |> Iterable.dedup() |> new()

  @doc """
  Returns a new iterator with every `nth` element in the iterator dropped,
  starting with the first element.

  ## Examples

      iex> 1..10
      ...> |> Iter.from()
      ...> |> Iter.drop_every(2)
      ...> |> Iter.to_list()
      [2, 4, 6, 8, 10]

      iex> 1..10
      ...> |> Iter.from()
      ...> |> Iter.drop_every(0)
      ...> |> Iter.to_list()
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

      iex> [1, 2, 3]
      ...> |> Iter.from()
      ...> |> Iter.drop_every(1)
      ...> |> Iter.to_list()
      []
  """
  @spec drop_every(t, non_neg_integer) :: t
  def drop_every(iter, nth) when is_iter(iter) and is_integer(nth) and nth >= 0,
    do: iter.iterable |> Iterable.drop_every(nth) |> new()

  @doc """
  Returns a new iterator which drops elements at the beginning of the iterator
  until `predicate` returns a truthy value.

  ## Example

      iex> [1, 2, 3, 2, 1]
      ...> |> Iter.from()
      ...> |> Iter.drop_while(&(&1 < 3))
      ...> |> Iter.to_list()
      [3, 2, 1]
  """
  @spec drop_while(t, predicate) :: t
  def drop_while(iter, predicate),
    do: iter.iterable |> Iterable.drop_while(predicate) |> new()

  @doc """
  Returns a new iterator which drops the first `how_many` elements.

  ## Examples

      iex> 1..3
      ...> |> Iter.from()
      ...> |> Iter.drop(2)
      ...> |> Iter.to_list()
      [3]
  """
  @spec drop(t, non_neg_integer) :: t
  def drop(iter, how_many) when is_iter(iter) and is_integer(how_many) and how_many >= 0,
    do: iter.iterable |> Iterable.drop(how_many) |> new()

  @doc """
  Call `fun` for every element in the iterator.

  The return value is not used.

  ## Example

  ```elixir
  ["marty", "doc"]
  |> Iter.from()
  |> Iter.each(&IO.puts/1)
  "marty"
  "doc"
  #=> :done
  ```
  """
  @spec each(t, (element -> any)) :: :done
  def each(iter, fun) when is_iter(iter),
    do: Iterable.each(iter.iterable, fun)

  @doc """
  Determines if the iterator is empty.

  ## Examples

      iex> Iter.empty()
      ...> |> Iter.empty?()
      true

      iex> 1..20
      ...> |> Iter.from()
      ...> |> Iter.empty?
      false
  """
  @spec empty?(t) :: boolean
  def empty?(iter),
    do: Iterable.empty?(iter.iterable)

  @doc """
  Returns an iterator that contains no elements.

  ## Example

      iex> Iter.empty()
      ...> |> Iter.to_list()
      []
  """
  @spec empty :: t
  def empty, do: Iterable.Empty.new() |> new()

  @doc """
  Remove elements for which `predicate` returns a truthy value.

  ## Example

      iex> [1, 2, 3, 4]
      ...> |> Iter.from()
      ...> |> Iter.filter(&(rem(&1, 2) == 0))
      ...> |> Iter.to_list()
      [2, 4]
  """
  @spec filter(t, predicate) :: t
  def filter(iter, predicate) when is_iter(iter) and is_function(predicate, 1),
    do: iter.iterable |> Iterable.filter(predicate) |> new()

  @doc """
  Finds the index of the first value in the iterator that matches `predicate`.

  ## Example

      iex> [1, 2, 3, 4, 5]
      ...> |> Iter.from()
      ...> |> Iter.find_index(&(&1 > 3))
      {:ok, 3, Iter.from([5])}
  """
  @spec find_index(t, predicate) :: {:ok, non_neg_integer, t} | :done
  def find_index(iter, predicate) when is_iter(iter) and is_function(predicate, 1) do
    with {:ok, index, iterable} <- Iterable.find_index(iter.iterable, predicate) do
      {:ok, index, new(iterable)}
    end
  end

  @doc """
  Similar to `find/3`, but returns the value of the function invocation instead
  of the element itself.

  ## Example

      iex> [2, 3, 4]
      ...> |> Iter.from()
      ...> |> Iter.find_value(fn x ->
      ...>   if x > 2, do: x * x
      ...> end)
      {:ok, 9, Iter.from([4])}

      iex> [2, 4, 6]
      ...> |> Iter.from()
      ...> |> Iter.find_value(&(rem(&1, 2) == 1))
      :done

      iex> [2, 3, 4]
      ...> |> Iter.from()
      ...> |> Iter.find_value(&(rem(&1, 2) == 1))
      {:ok, true, Iter.from([4])}
  """
  @spec find_value(t, (element -> any)) :: {:ok, any, t} | :done
  def find_value(iter, fun) do
    with {:ok, result, iterable} <- Iterable.find_value(iter.iterable, fun) do
      {:ok, result, new(iterable)}
    end
  end

  @doc """
  Searches for the first element in the iterator which matches `predicate`.

  ## Example

      iex> [1, 2, 3, 4, 5]
      ...> |> Iter.from()
      ...> |> Iter.find(&(&1 > 3))
      {:ok, 4, Iter.from([5])}

      iex> [1, 2, 3]
      ...> |> Iter.from()
      ...> |> Iter.find(&(&1 > 4))
      :done
  """
  @spec find(t, predicate) :: {:ok, element, t} | :done
  def find(iter, predicate) when is_iter(iter) and is_function(predicate, 1) do
    with {:ok, element, iterable} <- Iterable.find(iter.iterable, predicate) do
      {:ok, element, new(iterable)}
    end
  end

  @doc """
  Maps `fun` over the iterator flattening the result.

  ## Example

      iex> [:a, :b, :c]
      ...> |> Iter.from()
      ...> |> Iter.flat_map(&Iter.from([&1, &1]))
      ...> |> Iter.to_list()
      [:a, :a, :b, :b, :c, :c]
  """
  @spec flat_map(t, mapper) :: t
  def flat_map(iter, mapper) when is_iter(iter) and is_function(mapper, 1),
    do: iter.iterable |> Iterable.flat_map(mapper) |> new()

  @doc """
  Flattens nested iterators.

  ## Example

      iex> [[:a, :a], [:b, :b], [:c, :c]]
      ...> |> Iter.from()
      ...> |> Iter.flatten()
      ...> |> Iter.to_list()
      [:a, :a, :b, :b, :c, :c]
  """
  @spec flatten(t) :: t
  def flatten(iter) when is_iter(iter),
    do: iter.iterable |> Iterable.flatten() |> new()

  @doc """
  Convert anything that implements `Iter.IntoIterable` into an `Iter`.
  """
  @spec from(IntoIterable.t()) :: t
  def from(iter) when is_iter(iter),
    do: iter

  def from(maybe_iterable),
    do: maybe_iterable |> IntoIterable.into_iterable() |> new()

  @doc """
  Convert an `Enumerable` into an `Iter`.

  Provides an `Enumerable` compatible source for `Iter` using a `GenServer` to
  orchestrate reduction and block as required.

  > #### Warning {: .warning}
  > You should almost always implement `IntoIterable` for your enumerable and
  > use `from/1` rather than resort to calling this function.  Unfortunately it
  > cannot always be avoided.
  """
  @spec from_enum(Enumerable.t()) :: t
  def from_enum(enumerable) do
    enumerable
    |> Iterable.Enumerable.new()
    |> new()
  end

  @doc """
  Intersperses `separator` between each element of the iterator.

  ## Examples

      iex> 1..3
      ...> |> Iter.from()
      ...> |> Iter.intersperse(0)
      ...> |> Iter.to_list()
      [1, 0, 2, 0, 3]

      iex> [1]
      ...> |> Iter.from()
      ...> |> Iter.intersperse(0)
      ...> |> Iter.to_list()
      [1]

      iex> []
      ...> |> Iter.from()
      ...> |> Iter.intersperse(0)
      ...> |> Iter.to_list()
      []
  """
  @spec intersperse(t, any) :: t
  def intersperse(iter, separator) when is_iter(iter),
    do: iter.iterable |> Iterable.intersperse(separator) |> new()

  @doc """
  Emits a sequence of values, starting with `start_value`. Successive values are
  generated by calling `next_fun` on the previous value.

      iex> Iter.iterate(0, &(&1 + 1))
      ...> |> Iter.take(5)
      ...> |> Iter.to_list()
      [0, 1, 2, 3, 4]
  """
  @spec iterate(element, (element -> element)) :: t
  def iterate(start_value, next_fun) when is_function(next_fun, 1) do
    Iterable.Resource.new(
      fn -> start_value end,
      fn acc ->
        {[acc], next_fun.(acc)}
      end,
      fn _ -> :ok end
    )
    |> new()
  end

  @doc """
  Creates a new iterator which applies `mapper` on every `nth` element of the
  iterator, starting with the first element.

  The first element is always mapped unless `nth` is `0`.

  ## Examples

      iex> 1..10
      ...> |> Iter.from()
      ...> |> Iter.map_every(2, fn x -> x + 1000 end)
      ...> |> Iter.to_list()
      [1001, 2, 1003, 4, 1005, 6, 1007, 8, 1009, 10]

      iex> 1..10
      ...> |> Iter.from()
      ...> |> Iter.map_every(3, fn x -> x + 1000 end)
      ...> |> Iter.to_list()
      [1001, 2, 3, 1004, 5, 6, 1007, 8, 9, 1010]

      iex> 1..5
      ...> |> Iter.from()
      ...> |> Iter.map_every(0, fn x -> x + 1000 end)
      ...> |> Iter.to_list()
      [1, 2, 3, 4, 5]

      iex> 1..3
      ...> |> Iter.from()
      ...> |> Iter.map_every(1, fn x -> x + 1000 end)
      ...> |> Iter.to_list()
      [1001, 1002, 1003]
  """
  @spec map_every(t, non_neg_integer, (element -> new_element)) :: t when new_element: any
  def map_every(iter, nth, mapper)
      when is_iter(iter) and is_integer(nth) and nth >= 0 and is_function(mapper, 1),
      do: iter.iterable |> Iterable.map_every(nth, mapper) |> new()

  @doc """
  Apply `fun` to each element in the iterator and collect the result.

  ## Example

      iex> [1, 2, 3, 4]
      ...> |> Iter.from()
      ...> |> Iter.map(&(&1 * 2))
      ...> |> Iter.to_list()
      [2, 4, 6, 8]
  """
  @spec map(t, mapper) :: t
  def map(iter, mapper) when is_iter(iter) and is_function(mapper, 1),
    do: iter.iterable |> Iterable.map(mapper) |> new()

  @doc """
  Returns the maximal element in the iterator according to Erlang's term sorting.

  ## Example

      iex> [1, 4, 3, 2]
      ...> |> Iter.from()
      ...> |> Iter.max()
      {:ok, 4}

      iex> Iter.empty()
      ...> |> Iter.max()
      :done
  """
  @spec max(t, sorter) :: {:ok, element} | :done
  def max(iter, sorter \\ &>=/2) when is_iter(iter) and is_function(sorter, 2),
    do: Iterable.max(iter.iterable, sorter)

  @doc """
  Returns the maximal element in the iterator as calculated by `mapper`.

  ## Example

      iex> ["a", "aa", "aaa"]
      ...> |> Iter.from()
      ...> |> Iter.max_by(&String.length/1)
      {:ok, "aaa"}

      iex> Iter.empty()
      ...> |> Iter.max_by(&String.length/1)
      :done
  """
  @spec max_by(t, mapper, sorter) :: {:ok, element} | :done
  def max_by(iter, mapper, sorter \\ &>=/2)
      when is_iter(iter) and is_function(mapper, 1) and is_function(sorter, 2),
      do: Iterable.max_by(iter.iterable, mapper, sorter)

  @doc """
  Checks if `element` is a member of `iterable`.

  ## Examples

      iex> 1..5
      ...> |> Iter.from()
      ...> |> Iter.member?(3)
      true

      iex> 1..5
      ...> |> Iter.from()
      ...> |> Iter.member?(6)
      false
  """
  @spec member?(t, element) :: boolean
  def member?(iter, element) when is_iter(iter),
    do: Iterable.member?(iter.iterable, element)

  @doc """
  Returns the minimal element in the iterator according to Erlang's term sorting.

  ## Example

      iex> [1, 4, 3, 2]
      ...> |> Iter.from()
      ...> |> Iter.min()
      {:ok, 1}
  """
  @spec min(t, sorter) :: {:ok, element} | :done
  def min(iter, sorter \\ &<=/2)
      when is_iter(iter) and is_function(sorter, 2),
      do: Iterable.min(iter.iterable, sorter)

  @doc """
  Returns the minimal element in the iterator as calculated by `mapper`.

  ## Example

      iex> ["a", "aa", "aaa"]
      ...> |> Iter.from()
      ...> |> Iter.min_by(&String.length/1)
      {:ok, "a"}

      iex> Iter.empty()
      ...> |> Iter.min_by(&String.length/1)
      :done
  """
  @spec min_by(t, mapper, sorter) :: {:ok, element} | :done
  def min_by(iter, mapper, sorter \\ &<=/2)
      when is_iter(iter) and is_function(mapper, 1) and is_function(sorter, 2),
      do: Iterable.min_by(iter.iterable, mapper, sorter)

  @doc """
  Returns the minimal and maximal element in the iterator according to Erlang's
  term ordering.

  ## Example

      iex> [2, 3, 1]
      ...> |> Iter.from()
      ...> |> Iter.min_max()
      {:ok, 1, 3}

      iex> Iter.empty()
      ...> |> Iter.min_max()
      :done
  """
  @spec min_max(t) :: {:ok, min, max} | :done when min: element, max: element
  def min_max(iter) when is_iter(iter),
    do: Iterable.min_max(iter.iterable)

  @doc """
  Advance the iterator and return the next value.

  ## Return values

    - `{:ok, element, new_iterator}` - returns the next element and an updated iterator.
    - `:done` - the iterator is exhausted.
  """
  @spec next(t) :: {:ok, element, t} | :done
  def next(iter) when is_iter(iter) do
    with {:ok, element, iterable} <- Iter.next(iter.iterable) do
      {:ok, element, new(iterable)}
    end
  end

  @doc """
  Peeks at the first element of the iterator, without consuming it.

  > #### Warning {: .warning}
  > Many iterators cannot be peeked, so this function simulates peeking by
  > consuming an element from the iterator and returning a new iterator which
  > pushes that element back onto the front.

  ## Example

      iex> {:ok, 1, iter} = 1..3
      ...> |> Iter.from()
      ...> |> Iter.peek()
      ...> Iter.to_list(iter)
      [1, 2, 3]
  """
  @spec peek(t) :: {:ok, element, t} | :done
  def peek(iter) when is_iter(iter) do
    with {:ok, element, iterable} <- Iterable.peek(iter.iterable) do
      {:ok, element, new(iterable)}
    end
  end

  @doc """
  Peeks at the first `how_many` elements of the iterator, without consuming
  them.

  > #### Warning {: .warning}
  > Many iterables cannot be peeked, so this function simulates peeking by
  > consuming elements from the iterator and returning a new iterator which
  > pushes those elements back on to the front.

  Because it's possible to try and peek past the end of an iterator you
  shouldn't expect the number of elements returned to always be the same as how
  many you asked for.  For this reason the return value includes the number of
  elements that were able to be peeked.

  ## Example

      iex> {:ok, peeks, 3, iter} = 1..5
      ...> |> Iter.from()
      ...> |> Iter.peek(3)
      iex> Iter.to_list(peeks)
      [1, 2, 3]
      iex> Iter.to_list(iter)
      [1, 2, 3, 4, 5]

      iex> {:ok, peeks, 3, iter} = 1..3
      ...> |> Iter.from()
      ...> |> Iter.peek(5)
      iex> Iter.to_list(peeks)
      [1, 2, 3]
      iex> Iter.to_list(iter)
      [1, 2, 3]
  """
  @spec peek(t, how_many :: pos_integer) :: {:ok, [element], non_neg_integer, t} | :done
  def peek(iter, how_many) when is_iter(iter) do
    with {:ok, peeks, got, iterable} <- Iterable.peek(iter.iterable, how_many) do
      {:ok, from(peeks), got, new(iterable)}
    end
  end

  @doc """
  Prepend a new element to the beginning of the iterable.

  ## Example

      iex> 1..3
      ...> |> Iter.from()
      ...> |> Iter.prepend(4)
      ...> |> Iter.to_list()
      [4, 1, 2, 3]
  """
  @spec prepend(t, element) :: t
  def prepend(iter, element) when is_iter(iter),
    do: iter.iterable |> Iterable.prepend(element) |> new()

  @doc """
  Keep elements for which `predicate` returns a truthy value.

  ## Example

      iex> [1, 2, 3, 4]
      ...> |> Iter.from()
      ...> |> Iter.reject(&(rem(&1, 2) == 0))
      ...> |> Iter.to_list()
      [1, 3]
  """
  @spec reject(t, predicate) :: t
  def reject(iter, predicate) when is_iter(iter) and is_function(predicate, 1) do
    iter.iterable
    |> Iterable.filter(fn element ->
      if predicate.(element), do: false, else: true
    end)
    |> new()
  end

  @doc """
  Returns an iterator generated by calling `generator_fun` repeatedly.

  ## Examples

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsss, {1, 2, 3})
      iex> Iter.repeatedly(&:rand.uniform/0) |> Iter.take(3) |> Iter.to_list()
      [0.5455598952593053, 0.6039309974353404, 0.6684893034823949]
  """
  @spec repeatedly((-> element)) :: t
  def repeatedly(generator_fun) when is_function(generator_fun, 0) do
    resource(
      fn -> nil end,
      fn _ -> {[generator_fun.()], nil} end,
      fn _ -> nil end
    )
  end

  @doc """
  Create an iterator from a resource.

      iex> Iter.resource(
      ...>   fn ->
      ...>     {:ok, pid} = StringIO.open("Marty")
      ...>     pid
      ...>   end,
      ...>   fn pid ->
      ...>     case IO.read(pid, 1) do
      ...>       :eof -> {:halt, pid}
      ...>       char -> {[char], pid}
      ...>     end
      ...>   end,
      ...>   fn pid ->
      ...>     StringIO.close(pid)
      ...>   end
      ...> )
      ...> |> Iter.to_list()
      ["M", "a", "r", "t", "y"]
  """
  @spec resource(
          start_fun :: (-> acc),
          next_fun :: (acc -> {[element], acc} | {:halt, acc}),
          after_fun :: (acc -> any)
        ) :: t
        when acc: any
  def resource(start_fun, next_fun, after_fun)
      when is_function(start_fun, 0) and is_function(next_fun, 1) and is_function(after_fun, 1),
      do: Iterable.Resource.new(start_fun, next_fun, after_fun) |> new()

  @doc """
  Creates an iterator starting at the same point, but stepping by `step_size`
  each iteration.

  The first element of the iterator will always be returned, regardless of the step given.

  ## Examples

        iex> 1..9
        ...> |> Iter.from()
        ...> |> Iter.step_by(3)
        ...> |> Iter.to_list()
        [1, 4, 7]
  """
  @spec step_by(t, pos_integer) :: t
  def step_by(iter, step) when is_iter(iter) and is_integer(step) and step > 0,
    do: iter.iterable |> Iterable.step_by(step) |> new()

  @doc """
  Collects the first `how_many` elements into a new iterator and returns it
  along with the advanced initial iterator.

  This is very much like `take/2` except that it returns the remaining iterator
  so that it can be called repeatedly.

  ## Example

      iex> iter = Iter.from(1..9)
      ...> {:ok, chunk_a, iter} = Iter.take_chunk(iter, 3)
      ...> {:ok, chunk_b, remainder} = Iter.take_chunk(iter, 3)
      ...> Iter.to_list(chunk_a)
      [1, 2, 3]
      iex> Iter.to_list(chunk_b)
      [4, 5, 6]
      iex> Iter.to_list(remainder)
      [7, 8, 9]
  """
  @spec take_chunk(t, pos_integer()) :: {:ok, t, t} | {:done, t}
  def take_chunk(iter, how_many) when is_iter(iter) and is_integer(how_many) and how_many > 0 do
    case Iterable.take_chunk(iter.iterable, how_many) do
      {:ok, chunk, remainder} -> {:ok, new(chunk), new(remainder)}
      {:done, chunk} -> {:done, new(chunk)}
    end
  end

  @doc """
  Creates an iterable which emits elements until `predicate` returns `false`.

  The rest of the underlying iterable is discarded.

  ## Example

      iex> 1..3
      ...> |> Iter.from()
      ...> |> Iter.take_while(&(&1 < 3))
      ...> |> Iter.to_list()
      [1, 2]
  """
  @spec take_while(t, predicate) :: t
  def take_while(iter, predicate) when is_iter(iter) and is_function(predicate, 1),
    do: iter.iterable |> Iterable.take_while(predicate) |> new()

  @doc """
  Takes the next `count` elements from the iterable and stops iteration.

  If a negative count is given, the last count values will be taken. For such,
  the collection is fully enumerated keeping up to `count` elements in memory.
  Once the end of the collection is reached, the last `count` elements will be
  iterated. Therefore, using a negative count on an infinite collection will
  never return.

  The rest of the underlying iterable is discarded.

  ## Examples

      iex> Iter.empty()
      ...> |> Iter.take(3)
      ...> |> Iter.to_list()
      []

      iex> Iter.empty()
      ...> |> Iter.take(-3)
      ...> |> Iter.to_list()
      []

      iex> 1..5
      ...> |> Iter.from()
      ...> |> Iter.take(3)
      ...> |> Iter.to_list()
      [1, 2, 3]

      iex> 1..5
      ...> |> Iter.from()
      ...> |> Iter.take(-3)
      ...> |> Iter.to_list()
      [3, 4, 5]
  """
  @spec take(t, integer) :: t
  def take(iter, count) when is_iter(iter) and is_integer(count),
    do: do_take(iter, count)

  defp do_take(iter, count) when count >= 0,
    do: iter.iterable |> Iterable.take_head(count) |> new()

  defp do_take(iter, count) when count < 0,
    do: iter.iterable |> Iterable.take_tail(0 - count) |> new()

  @doc """
  Convert an iterator into a list.
  """
  @spec to_list(t) :: [element]
  def to_list(iter) when is_iter(iter),
    do: Iterable.to_list(iter.iterable)

  @doc """
  Convert an iterator into an Elixir stream.

  ## Example

      iex> [:a, :b, :c]
      ...> |> Iter.from()
      ...> |> Iter.cycle()
      ...> |> Iter.to_stream()
      ...> |> Enum.take(5)
      [:a, :b, :c, :a, :b]
  """
  @spec to_stream(t) :: Enumerable.t()
  def to_stream(iter) do
    Stream.resource(
      fn -> iter end,
      fn iter ->
        case Iterable.next(iter.iterable) do
          {:ok, element, iterable} -> {[element], %{iter | iterable: iterable}}
          :done -> {:halt, iter}
        end
      end,
      fn _ -> :ok end
    )
  end

  @doc """
  Creates a new iterator which returns only unique elements.

  > #### Warning {: .warning}
  > Except for specific data structures (eg `MapSet` and `Range`) most iterators
  > will need to store a set of "seen values" in order to provide this function.
  > In such cases memory usage will grow in direct relation to the number of
  > unique elements in the iterator.

  ## Example

      iex> 1..5
      ...> |> Iter.from()
      ...> |> Iter.uniq()
      Iter.from(1..5)

      iex> [1, 2, 3, 2, 1]
      ...> |> Iter.from()
      ...> |> Iter.uniq()
      ...> |> Iter.to_list()
      [1, 2, 3]
  """
  @spec uniq(t) :: t
  def uniq(iter) when is_iter(iter),
    do: iter.iterable |> Iterable.uniq() |> new()

  @doc """
  Creates a new iterator which replaces each element with a tuple containing the
  original element and the count of elements so far.

  ## Example

      iex> 1..3
      ...> |> Iter.from()
      ...> |> Iter.with_index()
      ...> |> Iter.to_list()
      [{1, 0}, {2, 1}, {3, 2}]
  """
  @spec with_index(t) :: t
  def with_index(iter) when is_iter(iter),
    do: iter.iterable |> Iterable.with_index() |> new()

  @doc """
  Zips corresponding elements from a finite collection of iterators into a new
  iterator, transforming them with `zip_fun` as it goes.

  The first element from each of the iterators will be put into a list which is
  then passed to the one-arity `zip_fun` function.  Then, the second elements
  from each of the iterators are put into a list, and so on until any of the
  iterators are exhausted.

  ## Example

      iex> first = Iter.from(1..3)
      ...> second = Iter.from(4..6)
      ...> third = Iter.from(7..9)
      ...> [first, second, third]
      ...> |> Iter.from()
      ...> |> Iter.zip_with(fn [a, b, c] -> a + b + c end)
      ...> |> Iter.to_list()
      [12, 15, 18]
  """
  @spec zip_with(t, ([element] -> any)) :: t
  def zip_with(iter, zipper) when is_iter(iter) and is_function(zipper, 1) do
    iter.iterable
    |> Iterable.map(&IntoIterable.into_iterable/1)
    |> Iterable.zip(zipper)
    |> new()
  end

  @doc """
  Zips corresponding elements from two iterators into a new one, transforming
  them with `zip_fun` as it goes.

  The `zip_fun` will be called with the first elements from the iterators, then
  the second elements and so on.

  ## Example

      iex> first = Iter.from(1..3)
      ...> second = Iter.from(4..6)
      ...> Iter.zip_with(first, second, &(&1 + &2))
      ...> |> Iter.to_list()
      [5, 7, 9]
  """
  @spec zip_with(t, t, (element, element -> any)) :: t
  def zip_with(lhs, rhs, zipper) when is_iter(lhs) and is_iter(rhs) and is_function(zipper, 2) do
    [lhs.iterable, rhs.iterable]
    |> Iterable.zip(fn [a, b] -> zipper.(a, b) end)
    |> new()
  end

  @doc """
  Zips corresponding elements from a finite collection of iterators into one iterator of tuples.

  The zipping finishes as soon as any iterable in the collection is exhausted.

  ## Example

      iex> first = Iter.from(1..3)
      ...> second = Iter.from([:a, :b, :c])
      ...> third = Iter.from(["a", "b", "c"])
      ...> [first, second, third]
      ...> |> Iter.from()
      ...> |> Iter.zip()
      ...> |> Iter.to_list()
      [{1, :a, "a"}, {2, :b, "b"}, {3, :c, "c"}]
  """
  @spec zip(t) :: t
  def zip(iter) when is_iter(iter) do
    iter.iterable
    |> Iterable.map(&IntoIterable.into_iterable/1)
    |> Iterable.zip(&List.to_tuple/1)
    |> new()
  end

  @doc """
  Zips to iterators together.

  The zipping finishes as soon as either iterator is exhausted.

  ## Example

      iex> first = Iter.from(1..3)
      ...> second = Iter.from([:a, :b, :c])
      ...> Iter.zip(first, second)
      ...> |> Iter.to_list()
      [{1, :a}, {2, :b}, {3, :c}]
  """
  @spec zip(t, t) :: t
  def zip(lhs, rhs) when is_iter(lhs) and is_iter(rhs) do
    [lhs.iterable, rhs.iterable]
    |> Iterable.zip(&List.to_tuple/1)
    |> new()
  end

  defp new(iterable), do: %__MODULE__{iterable: iterable}
end
