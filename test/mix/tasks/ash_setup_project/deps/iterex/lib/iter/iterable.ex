defprotocol Iter.Iterable do
  @moduledoc """
  This is the main iterable protocol.

  It is intentionally huge, however rte only function you have to implement is
  `next/1`, for the remainder you can rely on the default implementations from
  `Iter.Impl` unless your data structure can provide a more
  efficient method of generating the correct answer.
  """

  @default_impl """
  > #### Optional callback {: .tip}
  > A default implementation of this function exists in the `Iter.Impl` module.
  >
  > You can add it to your protocol implementation by adding `use Iter.Impl`.
  """

  @type element :: Iter.element()
  @type predicate :: Iter.predicate()
  @type mapper :: Iter.mapper()
  @type t :: any

  @doc """
  Advance the iterable and return the next value.

  This is the only required callback in the `Iterable` protocol.

  ## Return values

    - `{:ok, element, new_iterable}` - returns the next element and an updated iterable.
    - `:done` - the iterable is exhausted.
  """
  @spec next(t) :: {:ok, element, t} | :done
  def next(iterable)

  @doc """
  Tests if every element in the iterable matches `predicate`.

  #{@default_impl}
  """
  @spec all?(t, predicate) :: boolean
  def all?(iterable, predicate)

  @doc """
  Tests if any element in the iterable matches `predicate`.

  #{@default_impl}
  """
  @spec any?(t, predicate) :: boolean
  def any?(iterable, predicate)

  @doc """
  Append an element onto the end of the iterable.

  #{@default_impl}
  """
  @spec append(t, element) :: t
  def append(iterable, element)

  @doc """
  Returns the element `index` items from the beginning of the iterator.

  Note that all preceding elements, as well as the returned element, will be consumed from the iterable.

  ## Return values

    - `{:ok, element, new_iterable}` - the next element and an updated iterable.
    - `:done` - the iterable was exhausted before the element was found.

  #{@default_impl}
  """
  @spec at(t, non_neg_integer) :: {:ok, non_neg_integer, t} | :done
  def at(iterable, index)

  @doc """
  Creates an iterable that only emits elements for which `fun` returns a new
  value.

  #{@default_impl}
  """
  @spec chunk_by(t, (element -> any)) :: t
  def chunk_by(iterable, chunker)

  @doc """
  Creates an iterable that chunks into `count` size elements, where each new
  chunk starts `step` elements into the enumerable.

  #{@default_impl}
  """
  @spec chunk_every(t, pos_integer, pos_integer, t | :discard) :: t
  def chunk_every(iterable, count, step, leftover)

  @doc """
  Creates an iterable that chunks based on a chunk function.

  #{@default_impl}
  """
  @spec chunk_while(
          t,
          acc,
          (element, acc -> {:cont, chunk, acc} | {:cont, acc} | {:halt, acc}),
          (acc -> {:cont, chunk, acc} | {:cont, acc})
        ) :: t
        when acc: any, chunk: any
  def chunk_while(iterable, acc, chunk_fun, after_fun)

  @doc """
  Creates an iterable that iterates each iterable in an iterable.

  #{@default_impl}
  """
  @spec concat(t) :: t
  def concat(iterable)

  @doc """
  Consumes the iterable, counting the number of iterations remaining.

  #{@default_impl}
  """
  @spec count(t) :: non_neg_integer
  def count(iterable)

  @doc """
  Consumes the iterable, counting the number of elements for which `fun` returns a truthy value.

  #{@default_impl}
  """
  @spec count(t, (element -> as_boolean(any))) :: non_neg_integer
  def count(iterable, fun)

  @doc """
  Creates an iterable that cycles it's elements eternally.

  #{@default_impl}
  """
  @spec cycle(t) :: t
  def cycle(iterable)

  @doc """
  Creates an iterable that only emits elements if they are different from the previous element.

  The function `fun` maps every element to a term which is used to determine if two elements are duplicates.

  #{@default_impl}
  """
  @spec dedup_by(t, (element -> any)) :: t
  def dedup_by(iterable, fun)

  @doc """
  Creates an iterable that only emits elements if they are different from the previous element.

  #{@default_impl}
  """
  @spec dedup(t) :: t
  def dedup(iterable)

  @doc """
  Returns a new iterable with every `nth` element in the `iterable` dropped,
  starting with the first element.

  #{@default_impl}
  """
  @spec drop_every(t, non_neg_integer) :: t
  def drop_every(iterable, nth)

  @doc """
  Drops elements at the beginning of the `iterable` while `predicate` returns a
  truthy value.

  #{@default_impl}
  """
  @spec drop_while(t, predicate) :: t
  def drop_while(iterable, predicate)

  @doc """
  Creates an iterable which drops the first `how_many` elements.

  #{@default_impl}
  """
  @spec drop(t, non_neg_integer()) :: t
  def drop(iterable, how_many)

  @doc """
  Consumes the iterable and applies `fun` to each element.

  Primarily used for side-effects.

  Always returns `:done`.

  #{@default_impl}
  """
  @spec each(t, (element -> any)) :: :done
  def each(iterable, fun)

  @doc """
  Determines if the iterable is empty.

  #{@default_impl}
  """
  @spec empty?(t) :: boolean
  def empty?(iter)

  @doc """
  Creates an iterable which drops elements for which `predicate` doesn't return
  a truthy value.

  #{@default_impl}
  """
  @spec filter(t, predicate) :: t
  def filter(iterable, predicate)

  @doc """
  Searches for the first element in the iterable which matches `predicate`.

  #{@default_impl}
  """
  @spec find(t, predicate) :: {:ok, element, t} | :done
  def find(iterable, predicate)

  @doc """
  Returns the index of the first element in the iterable which matches `predicate`.

  #{@default_impl}
  """
  @spec find_index(t, predicate) :: {:ok, non_neg_integer(), t} | :done
  def find_index(iterable, predicate)

  @doc """
  Returns the first non-falsy result of `fun`.

  #{@default_impl}
  """
  @spec find_value(t, (element -> result)) :: {:ok, result, t} | :done when result: any
  def find_value(iterable, fun)

  @doc """
  Creates an iterable which works like `map/2` but flattens nested iterables.

  #{@default_impl}
  """
  @spec flat_map(t, (element -> t | element)) :: t
  def flat_map(iterable, fun)

  @doc """
  Creates an iterable which flattens nested iterables.

  #{@default_impl}
  """
  @spec flatten(t) :: t
  def flatten(iterable)

  @doc """
  Creates a new iterable which places `separator` between adjacent items of the original iterable.

  #{@default_impl}
  """
  @spec intersperse(t, any) :: t
  def intersperse(iterable, separator)

  @doc """
  Creates a new iterable which applies `mapper` on every `nth` element of the
  iterable, starting with the first element.

  The first element is always mapped unless `nth` is `0`.

  #{@default_impl}
  """
  @spec map_every(t, non_neg_integer, (element -> new_element)) :: t when new_element: any
  def map_every(iterable, nth, mapper)

  @doc """
  Creates a new iterable which applies `mapper` to each element and using it's
  result as the new element value.

  #{@default_impl}
  """
  @spec map(t, (element -> new_element)) :: t when new_element: any
  def map(iterable, mapper)

  @doc """
  Returns the maximal element in the `iterable` according to Erlang's term ordering.

  #{@default_impl}
  """
  @spec max(t, (element, element -> boolean)) :: {:ok, element} | :done
  def max(iterable, sorter)

  @doc """
  Returns the maximal element in the `iterable` as calculated by `mapper`.

  #{@default_impl}
  """
  @spec max_by(t, (element -> new_element), (new_element, new_element -> boolean)) ::
          {:ok, element} | :done
        when new_element: element
  def max_by(iterable, mapper, sorter)

  @doc """
  Is the element a member of the iterable?

  #{@default_impl}
  """
  @spec member?(t, element) :: boolean
  def member?(iterable, element)

  @doc """
  Returns the minimal element in the `iterable` according to Erlang's term ordering.

  #{@default_impl}
  """
  @spec min(t, (element, element -> boolean)) :: {:ok, element} | :done
  def min(iterable, sorter)

  @doc """
  Returns the minimal element in the `iterable` as calculated by `mapper`.

  #{@default_impl}
  """
  @spec min_by(t, (element -> new_element), (new_element, new_element -> boolean)) ::
          {:ok, element} | :done
        when new_element: element
  def min_by(iterable, mapper, sorter)

  @doc """
  Return the minimal and maximal element of the iterable.

  #{@default_impl}
  """
  @spec min_max(t) :: {:ok, element, element} | :done
  def min_max(iterable)

  @doc """
  Peeks at the first element of the iterable, without consuming it.

  ## Return values

    - `{:ok, element, new_iterable}` - the next element and an updated iterable.
    - `:done` - the iterable is exhausted.

  #{@default_impl}
  """
  @spec peek(t) :: {:ok, element, t} | :done
  def peek(iterable)

  @doc """
  Peeks at the first n elements of the iterable, without consuming it.

  ## Return values

    - `{:ok, [element], how_many, new_iterable}` - the peekable elements and an
      updated iterable.  Note that `how_many` may not be the same as you asked
      for if the underlying iterable is exhausted.
    - `:done` - the iterable is exhausted.

  #{@default_impl}
  """
  @spec peek(t) :: {:ok, [element], non_neg_integer, t} | :done
  def peek(iterable, how_many)

  @doc """
  Creates an iterable which prepends an element to the beginning of another
  iterable.

  #{@default_impl}
  """
  @spec prepend(t, element) :: t
  def prepend(iterable, element)

  @doc """
  Creates an iterable starting at the same point, but stepping by `step_size` each iteration.

  The first element of the iterable will always be returned, regardless of the step given.

  #{@default_impl}
  """
  @spec step_by(t, pos_integer()) :: t
  def step_by(iterable, step_size)

  @doc """
  Collects `how_many` elements into a chunk and returns it as well as the
  remaining iterable.

  #{@default_impl}
  """
  @spec take_chunk(t, non_neg_integer()) :: {:ok, t, t} | {:done, t}
  def take_chunk(iterable, how_many)

  @doc """
  Creates an iterable which takes the first `how_many` elements.

  #{@default_impl}
  """
  @spec take_head(t, non_neg_integer()) :: t
  def take_head(iterable, how_many)

  @doc """
  Creates an iterable which takes the last `how_many` elements.

  #{@default_impl}
  """
  @spec take_tail(t, non_neg_integer()) :: t
  def take_tail(iterable, how_many)

  @doc """
  Creates an iterable which emits elements until `predicate` returns `false`.

  #{@default_impl}
  """
  @spec take_while(t, predicate) :: t
  def take_while(iterable, predicate)

  @doc """
  Convert the iterable into a list.

  #{@default_impl}
  """
  @spec to_list(t) :: [element]
  def to_list(iterable)

  @doc """
  Creates an iterable that returns only unique elements.

  #{@default_impl}
  """
  @spec uniq(t) :: t
  def uniq(iterable)

  @doc """
  Creates an iterable which emits the current iteration count as well as the next value.

  #{@default_impl}
  """
  @spec with_index(t) :: t
  def with_index(iterable)

  @doc """
  Zips corresponding elements from a number of iterables into an iterable of
  results as computed by `zipper`.

  #{@default_impl}
  """
  @spec zip(t, ([element] -> any)) :: t
  def zip(t, zipper)
end
