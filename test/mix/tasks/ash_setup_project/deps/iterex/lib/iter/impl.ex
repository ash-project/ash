defmodule Iter.Impl do
  @moduledoc """
  The default implementations of all `Iter.Iterable` callbacks except `next/1`.

  By adding `use Iter.Impl` to your `Iter.Iterable` definition all of the
  default functions will be automatically delegated and marked as overridable.

  This allows you to implement only those callbacks which can reasonably be made
  faster for your particular iterable, and not have to implement all of them.

  For example, here's a fictional implementation of iterable for `List`:

  ```elixir
  defimpl Iter.Iterable, for: List do
    use Iter.Impl

    def next([head | tail]), do: {:ok, head, tail}
    def next([]), do: :done

    def peek([]), do: :done
    def peek([head | _] = list), do: {:ok, head, list}

    def empty?([]), do: true
    def empty?(_), do: false
  end
  ```

  Be aware that all the default implementations rely on your implementation of
  `next/1` which you always must provide.
  """

  alias Iter.{Impl, Iterable}

  @doc """
  Generate overridable delegations to the default iterable callbacks.
  """
  defmacro __using__(_) do
    quote generated: true do
      @impl Iterable
      defdelegate all?(iterable, predicate), to: Impl
      @impl Iterable
      defdelegate any?(iterable, predicate), to: Impl
      @impl Iterable
      defdelegate append(iterable, element), to: Impl
      @impl Iterable
      defdelegate at(iterable, index), to: Impl
      @impl Iterable
      defdelegate chunk_by(iterable, chunker), to: Impl
      @impl Iterable
      defdelegate chunk_every(iterable, count, step, leftover), to: Impl
      @impl Iterable
      defdelegate chunk_while(iterable, acc, chunk_fun, after_fun), to: Impl
      @impl Iterable
      defdelegate concat(iterable), to: Impl
      @impl Iterable
      defdelegate count(iterable), to: Impl
      @impl Iterable
      defdelegate count(iterable, fun), to: Impl
      @impl Iterable
      defdelegate cycle(iterable), to: Impl
      @impl Iterable
      defdelegate dedup_by(iterable, fun), to: Impl
      @impl Iterable
      defdelegate dedup(iterable), to: Impl
      @impl Iterable
      defdelegate drop_every(iterable, nth), to: Impl
      @impl Iterable
      defdelegate drop_while(iterable, predicate), to: Impl
      @impl Iterable
      defdelegate drop(iterable, how_many), to: Impl
      @impl Iterable
      defdelegate each(iterable, fun), to: Impl
      @impl Iterable
      defdelegate empty?(iterable), to: Impl
      @impl Iterable
      defdelegate filter(iterable, predicate), to: Impl
      @impl Iterable
      defdelegate find_index(iterable, predicate), to: Impl
      @impl Iterable
      defdelegate find_value(iterable, fun), to: Impl
      @impl Iterable
      defdelegate find(iterable, predicate), to: Impl
      @impl Iterable
      defdelegate flat_map(iterable, fun), to: Impl
      @impl Iterable
      defdelegate flatten(iterable), to: Impl
      @impl Iterable
      defdelegate intersperse(iterable, separator), to: Impl
      @impl Iterable
      defdelegate map_every(iterable, nth, mapper), to: Impl
      @impl Iterable
      defdelegate map(iterable, mapper), to: Impl
      @impl Iterable
      defdelegate max_by(iterable, mapper, sorter), to: Impl
      @impl Iterable
      defdelegate max(iterable, sorter), to: Impl
      @impl Iterable
      defdelegate member?(iterable, element), to: Impl
      @impl Iterable
      defdelegate min_by(iterable, mapper, sorter), to: Impl
      @impl Iterable
      defdelegate min_max(iterable), to: Impl
      @impl Iterable
      defdelegate min(iterable, sorter), to: Impl
      @impl Iterable
      defdelegate peek(iterable, how_many), to: Impl
      @impl Iterable
      defdelegate peek(iterable), to: Impl
      @impl Iterable
      defdelegate prepend(iterable, element), to: Impl
      @impl Iterable
      defdelegate step_by(iterable, step_size), to: Impl
      @impl Iterable
      defdelegate take_chunk(iterable, how_many), to: Impl
      @impl Iterable
      defdelegate take_head(iterable, how_many), to: Impl
      @impl Iterable
      defdelegate take_tail(iterable, how_many), to: Impl
      @impl Iterable
      defdelegate take_while(iterable, predicate), to: Impl
      @impl Iterable
      defdelegate to_list(iterable), to: Impl
      @impl Iterable
      defdelegate uniq(iterable), to: Impl
      @impl Iterable
      defdelegate with_index(iterable), to: Impl
      @impl Iterable
      defdelegate zip(iterables, zipper), to: Impl

      defoverridable all?: 2,
                     any?: 2,
                     append: 2,
                     at: 2,
                     chunk_by: 2,
                     chunk_every: 4,
                     chunk_while: 4,
                     concat: 1,
                     count: 1,
                     count: 2,
                     cycle: 1,
                     dedup_by: 2,
                     dedup: 1,
                     drop_every: 2,
                     drop_while: 2,
                     drop: 2,
                     each: 2,
                     empty?: 1,
                     filter: 2,
                     find_index: 2,
                     find_value: 2,
                     find: 2,
                     flat_map: 2,
                     flatten: 1,
                     intersperse: 2,
                     map_every: 3,
                     map: 2,
                     max_by: 3,
                     max: 2,
                     member?: 2,
                     min_by: 3,
                     min_max: 1,
                     min: 2,
                     peek: 1,
                     peek: 2,
                     prepend: 2,
                     step_by: 2,
                     take_chunk: 2,
                     take_head: 2,
                     take_tail: 2,
                     take_while: 2,
                     to_list: 1,
                     uniq: 1,
                     with_index: 1,
                     zip: 2
    end
  end

  @type iterable :: Iterable.t()
  @type element :: Iterable.element()
  @type predicate :: Iterable.predicate()

  @doc """
  Tests if every element in the iterable matches `predicate`.

  ## Examples

      iex> Impl.all?([2, 4, 6, 8], &(rem(&1, 2) == 0))
      true

      iex> Impl.all?([2, 3, 4], &(rem(&1, 2) == 0))
      false
  """
  @spec all?(iterable, predicate) :: boolean
  def all?(iterable, predicate) when is_function(predicate, 1),
    do: do_all(iterable, predicate, true)

  defp do_all(_iterable, _predicate, nil), do: false
  defp do_all(_iterable, _predicate, false), do: false

  defp do_all(iterable, predicate, _) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} -> do_all(iterable, predicate, predicate.(element))
      :done -> true
    end
  end

  @doc """
  Tests if any element in the iterable matches `predicate`.

  ## Examples

      iex> Impl.any?([2, 4, 6], &(rem(&1, 2) == 1))
      false

      iex> Impl.any?([2, 3, 4], &(rem(&1, 2) == 1))
      true
  """
  @spec any?(iterable, predicate) :: boolean
  def any?(iterable, predicate) when is_function(predicate, 1),
    do: do_any(iterable, predicate, false)

  defp do_any(_iterable, _predicate, truthy) when truthy not in [nil, false], do: true

  defp do_any(iterable, predicate, _) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} -> do_any(iterable, predicate, predicate.(element))
      :done -> false
    end
  end

  @doc """
  Creates an iterable that appends an element to the end of the iterable.

  ## Examples

      iex> Impl.append(1..3, 4)
      ...> |> Impl.to_list()
      [1, 2, 3, 4]
  """
  @spec append(iterable, element) :: iterable
  def append(iterable, element), do: Iterable.Appender.new(iterable, element)

  @doc """
  Returns the element `index` items from the beginning of the iterable.

  ## Example

      iex> Impl.at([:a, :b, :c], 1)
      {:ok, :b, [:c]}
  """
  @spec at(iterable, non_neg_integer()) :: {:ok, element, iterable} | :done
  def at(iterable, 0), do: Iterable.next(iterable)

  def at(iterable, n) do
    case Iterable.next(iterable) do
      {:ok, _element, iterable} -> at(iterable, n - 1)
      :done -> :done
    end
  end

  @doc """
  Creates an iterable that chunks elements by subsequent return values of `fun`.

  ## Example

      iex> Impl.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1))
      ...> |> Impl.to_list()
      [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
  """
  @spec chunk_by(iterable, (element -> any)) :: iterable
  def chunk_by(iterable, chunker), do: Iterable.ByChunker.new(iterable, chunker)

  @doc """
  Creates an iterable that chunks elements into `count` sized chunks of `step` spacing.

  ## Examples

      iex> Impl.chunk_every([1, 2, 3, 4, 5, 6], 2, 2, []) |> Impl.to_list()
      [[1, 2], [3, 4], [5, 6]]

      iex> Impl.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, :discard) |> Impl.to_list()
      [[1, 2, 3], [3, 4, 5]]

      iex> Impl.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, [7]) |> Impl.to_list()
      [[1, 2, 3], [3, 4, 5], [5, 6, 7]]

      iex> Impl.chunk_every([1, 2, 3, 4, 5, 6], 3, 3, []) |> Impl.to_list()
      [[1, 2, 3], [4, 5, 6]]

      iex> cycler = Impl.cycle([0])
      iex> Impl.chunk_every([1, 2, 3, 4], 3, 3, cycler) |> Impl.to_list()
      [[1, 2, 3], [4, 0, 0]]
  """
  @spec chunk_every(iterable, pos_integer, pos_integer, iterable | :discard) :: iterable
  def chunk_every(iterable, count, step, leftover),
    do: Iterable.EveryChunker.new(iterable, count, step, leftover)

  @doc """
  Creates an iterable that chunks based on a chunk function.

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
      iex> iter = 1..10 |> Impl.chunk_while([], chunk_fun, after_fun)
      iex> Impl.to_list(iter)
      [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
  """
  @spec chunk_while(
          iterable,
          acc,
          (element, acc -> {:cont, chunk, acc} | {:cont, acc} | {:halt, acc}),
          (acc -> {:cont, chunk, acc} | {:cont, acc})
        ) :: iterable
        when acc: any, chunk: any
  def chunk_while(iterable, acc, chunk_fun, after_fun),
    do: Iterable.WhileChunker.new(iterable, acc, chunk_fun, after_fun)

  @doc """
  Takes an iterable and iterates each iterable in an iterable.

  ## Example

      iex> Impl.concat([1..3, 2..4, 3..5]) |> Impl.to_list()
      [1, 2, 3, 2, 3, 4, 3, 4, 5]
  """
  @spec concat(iterable) :: iterable
  def concat(iterable), do: Iterable.Concatenator.new(iterable)

  @doc """
  Consumes the iterable, returning the number of elements within

  ## Examples

      iex> Impl.count([])
      0

      iex> Impl.count([1,2,3])
      3
  """
  @spec count(iterable) :: non_neg_integer()
  def count(iterable), do: do_count(iterable, 0)

  defp do_count(iterable, so_far) do
    case Iterable.next(iterable) do
      {:ok, _element, iterable} -> do_count(iterable, so_far + 1)
      :done -> so_far
    end
  end

  @doc """
  Consumes the iterable, returning the number of elements for which `fun` returns a truthy value.

  ## Example

      iex> 1..5
      ...> |> Impl.count(&(rem(&1, 2) == 0))
      2
  """
  @spec count(iterable, (element -> as_boolean(any))) :: non_neg_integer()
  def count(iterable, fun) when is_function(fun, 1),
    do: do_count_matches(iterable, fun, 0)

  defp do_count_matches(iterable, fun, so_far) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} ->
        if fun.(element) do
          do_count_matches(iterable, fun, so_far + 1)
        else
          do_count_matches(iterable, fun, so_far)
        end

      :done ->
        so_far
    end
  end

  @doc """
  Creates an iterator that cycles it's elements eternally.

  ## Example

      iex> Impl.cycle(1..3)
      ...> |> Impl.take_head(5)
      ...> |> Impl.to_list()
      [1, 2, 3, 1, 2]
  """
  @spec cycle(iterable) :: iterable
  def cycle(iterable), do: Iterable.Cycler.new(iterable)

  @doc """
  Creates an iterable that only emits elements if they are different from the previous element.

  The function `fun` maps every element to a term which is used to determine if two elements are duplicates.

  ## Example

      iex> [{1, :a}, {2, :b}, {2, :c}, {1, :a}]
      ...> |> Impl.dedup_by(&elem(&1, 0))
      ...> |> Impl.to_list()
      [{1, :a}, {2, :b}, {1, :a}]
  """
  @spec dedup_by(iterable, (element -> any)) :: iterable
  def dedup_by(iterable, fun), do: Iterable.Deduper.new(iterable, fun)

  @doc """
  Creates an iterable that only emits elements if they are different from the previous element.

  ## Example

      iex> Impl.dedup([:a, :a, :b, :c, :b, :c, :c, :d])
      ...> |> Impl.to_list()
      [:a, :b, :c, :b, :c, :d]
  """
  @spec dedup(iterable) :: iterable
  def dedup(iterable), do: Iterable.Deduper.new(iterable)

  @doc """
  Returns a new iterable with every `nth` element in the `iterable` dropped,
  starting with the first element.

  ## Examples

      iex> 1..10
      ...> |> Impl.drop_every(2)
      ...> |> Impl.to_list()
      [2, 4, 6, 8, 10]

      iex> 1..12
      ...> |> Impl.drop_every(3)
      ...> |> Impl.to_list()
      [2, 3, 5, 6, 8, 9, 11, 12]

      iex> 1..10
      ...> |> Impl.drop_every(0)
      ...> |> Impl.to_list()
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

      iex> [1, 2, 3]
      ...> |> Impl.drop_every(1)
      ...> |> Impl.to_list()
      []
  """
  @spec drop_every(iterable, non_neg_integer) :: iterable
  def drop_every(iterable, nth), do: Iterable.EveryDropper.new(iterable, nth)

  @doc """
  Drops elements at the beginning of `iterable` while fun returns a truthy
  value.

  ## Example

      iex> [1, 2, 3, 2, 1]
      ...> |> Impl.drop_while(&(&1 < 3))
      ...> |> Impl.to_list()
      [3, 2, 1]
  """
  @spec drop_while(iterable, predicate) :: iterable
  def drop_while(iterable, predicate), do: Iterable.WhileDropper.new(iterable, predicate)

  @doc """
  Creates an iterable which drops the first `how_many` elements.

  ## Examples

      iex> Impl.drop([1, 2, 3], 2)
      ...> |> Impl.to_list()
      [3]

      iex> Impl.drop([1, 2, 3], 0)
      ...> |> Impl.to_list()
      [1, 2, 3]

      iex> Impl.drop([1, 2, 3], -2)
      ...> |> Impl.to_list()
      [1]
  """
  @spec drop(iterable, non_neg_integer()) :: iterable
  def drop(iterable, how_many) when is_integer(how_many), do: do_drop(iterable, how_many)

  defp do_drop(iterable, 0), do: iterable

  defp do_drop(iterable, how_many) when how_many > 0,
    do: Iterable.HeadDropper.new(iterable, how_many)

  defp do_drop(iterable, how_many) when how_many < 0,
    do: Iterable.TailDropper.new(iterable, 0 - how_many)

  @doc ~S"""
  Consumes the iterable and applies `fun` to each element.

  Primarily used for side-effects.

  Always returns `:done`.

  ## Example

  ```elixir
  Impl.each([1, 2, 3], &IO.puts("#{&1}"))
  "1"
  "2"
  "3"
  #=> :done
  ```
  """
  @spec each(iterable, (element -> any)) :: :done
  def each(iterable, fun) when is_function(fun, 1) do
    with {:ok, element, iterable} <- Iterable.next(iterable) do
      fun.(element)
      each(iterable, fun)
    end
  end

  @doc """
  Determines if the iterable is empty.

  ## Example

      iex> Impl.empty?([])
      true

      iex> Impl.empty?(1..20)
      false
  """
  @spec empty?(iterable) :: boolean
  def empty?(iterable) do
    case Iterable.peek(iterable) do
      {:ok, _element, _iter} -> false
      :done -> true
    end
  end

  @doc """
  Creates an iterable which drops elements for which `predicate` doesn't return true.

  ## Example

      iex> Impl.filter([1, 2, 3, 4], &(rem(&1, 2) == 1))
      ...> |> Impl.to_list()
      [1, 3]
  """
  @spec filter(iterable, predicate) :: iterable
  def filter(iterable, predicate) when is_function(predicate, 1),
    do: Iterable.Filterer.new(iterable, predicate)

  @doc """
  Returns the index of the first element in the iterable which matches `predicate`.

  ## Examples

      iex> Impl.find_index([1, 2, 3, 4], &(&1 > 2))
      {:ok, 2, [4]}

      iex> Impl.find_index([1, 2, 3, 4], &(&1 > 4))
      :done
  """
  @spec find_index(iterable, predicate) :: {:ok, non_neg_integer(), iterable} | :done
  def find_index(iterable, predicate), do: do_find_index(iterable, predicate, 0)

  defp do_find_index(iterable, predicate, index) do
    with {:ok, element, iterable} <- Iterable.next(iterable) do
      if predicate.(element) == true do
        {:ok, index, iterable}
      else
        do_find_index(iterable, predicate, index + 1)
      end
    end
  end

  @doc """
  Returns the first truthy value returned by `fun`.

  ## Example

      iex> Impl.find_value([1, 2, 3, 4], fn
      ...>   i when i > 2 -> i * 2
      ...>   _ -> nil
      ...> end)
      {:ok, 6, [4]}
  """
  @spec find_value(iterable, (element -> result)) :: {:ok, result, iterable} | :done
        when result: any
  def find_value(iterable, fun) do
    with {:ok, element, iterable} <- Iterable.next(iterable) do
      result = fun.(element)

      if result do
        {:ok, result, iterable}
      else
        find_value(iterable, fun)
      end
    end
  end

  @doc """
  Searches for the first element in the iterable which matches `predicate`.

  ## Example

      iex> Impl.find([1, 2, 3, 4], &(&1 > 2))
      {:ok, 3, [4]}

      iex> Impl.find([1, 2, 3, 4], &(&1 > 4))
      :done
  """
  @spec find(iterable, predicate) :: {:ok, element} | :done
  def find(iterable, predicate) do
    with {:ok, element, iterable} <- Iterable.next(iterable) do
      if predicate.(element) == true do
        {:ok, element, iterable}
      else
        find(iterable, predicate)
      end
    end
  end

  @doc """
  Creates an iterable which works like `map/2` but flattens nested iterables.

  ## Example

      iex> [:a, :b, :c]
      ...> |> Impl.flat_map(&[&1, &1])
      ...> |> Impl.to_list()
      [:a, :a, :b, :b, :c, :c]
  """
  @spec flat_map(iterable, (element -> iterable | element)) ::
          iterable
  def flat_map(iterable, fun) when is_function(fun, 1),
    do: Iterable.FlatMapper.new(iterable, fun)

  @doc """
  Creates an iterable which flattens nested iterables.

  ## Example

      iex> Impl.flatten([[1, 2], [3, [4, [5, 6]]]])
      ...> |> Impl.to_list()
      [1, 2, 3, 4, 5, 6]
  """
  @spec flatten(iterable) :: iterable
  def flatten(iterable), do: Iterable.Flattener.new(iterable)

  @doc """
  Creates a new iterable which applies `mapper` on every `nth` element of the
  iterable, starting with the first element.

  The first element is always mapped unless `nth` is `0`.

  ## Examples

      iex> Impl.map_every(1..10, 2, fn x -> x + 1000 end)
      ...> |> Impl.to_list()
      [1001, 2, 1003, 4, 1005, 6, 1007, 8, 1009, 10]

      iex> Impl.map_every(1..10, 3, fn x -> x + 1000 end)
      ...> |> Impl.to_list()
      [1001, 2, 3, 1004, 5, 6, 1007, 8, 9, 1010]

      iex> Impl.map_every(1..5, 0, fn x -> x + 1000 end)
      ...> |> Impl.to_list()
      [1, 2, 3, 4, 5]

      iex> Impl.map_every([1, 2, 3], 1, fn x -> x + 1000 end)
      ...> |> Impl.to_list()
      [1001, 1002, 1003]
  """
  @spec map_every(iterable, non_neg_integer, (element -> new_element)) :: iterable
        when new_element: any
  def map_every(iterable, nth, mapper)
      when is_integer(nth) and nth >= 0 and is_function(mapper, 1),
      do: Iterable.EveryMapper.new(iterable, nth, mapper)

  @doc """
  Creates a new iterable which applies `mapper` to each element and using it's
  result as the new element value.

  ## Example

      iex> Impl.map([1, 2, 3], &(&1 * &1))
      ...> |> Impl.to_list()
      [1, 4, 9]
  """
  @spec map(iterable, (element -> new_element)) :: iterable when new_element: any
  def map(iterable, mapper) when is_function(mapper, 1), do: Iterable.Mapper.new(iterable, mapper)

  @doc """
  Returns the maximal element in the `iterable` as calculated by `mapper`.

  ## Example

      iex> Impl.max_by(["a", "aa", "aaa"], &String.length/1, &>=/2)
      {:ok, "aaa"}

      iex> Impl.max_by([], &String.length/1, &>=/2)
      :done
  """
  @spec max_by(iterable, (element -> new_element), (new_element, new_element -> boolean)) ::
          {:ok, element} | :done
        when new_element: element
  def max_by(iterable, mapper, sorter) do
    with {:ok, element, iterable} <- Iterable.next(iterable) do
      do_max_by(iterable, mapper, sorter, element, mapper.(element))
    end
  end

  defp do_max_by(iterable, mapper, sorter, current_max_element, current_max_mapped) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} ->
        element_mapped = mapper.(element)

        if sorter.(element_mapped, current_max_mapped) do
          do_max_by(iterable, mapper, sorter, element, element_mapped)
        else
          do_max_by(iterable, mapper, sorter, current_max_element, current_max_mapped)
        end

      :done ->
        {:ok, current_max_element}
    end
  end

  @doc """
  Returns the maximal element in the `iterable` according to Erlang's term ordering.

  ## Examples

      iex> Impl.max([1, 3, 2], &>=/2)
      {:ok, 3}

      iex> Impl.max([], &>=/2)
      :done
  """
  @spec max(iterable, (element, element -> boolean)) :: {:ok, element} | :done
  def max(iterable, sorter) do
    with {:ok, element, iterable} <- Iterable.next(iterable) do
      do_max(iterable, sorter, element)
    end
  end

  defp do_max(iterable, sorter, current_max) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} ->
        if sorter.(element, current_max) do
          do_max(iterable, sorter, element)
        else
          do_max(iterable, sorter, current_max)
        end

      :done ->
        {:ok, current_max}
    end
  end

  @doc """
  Is the element a member of the iterable?
  """
  @spec member?(iterable, element) :: boolean
  def member?(iterable, element), do: any?(iterable, &(&1 == element))

  @doc """
  Returns the minimal element in the `iterable` as calculated by `mapper`.

  ## Example

      iex> Impl.min_by(["a", "aa", "aaa"], &String.length/1, &<=/2)
      {:ok, "a"}

      iex> Impl.min_by([], &String.length/1, &<=/2)
      :done
  """
  @spec min_by(iterable, (element -> new_element), (new_element, new_element -> boolean)) ::
          {:ok, element} | :done
        when new_element: element
  def min_by(iterable, mapper, sorter) do
    with {:ok, element, iterable} <- Iterable.next(iterable) do
      do_min_by(iterable, mapper, sorter, element, mapper.(element))
    end
  end

  defp do_min_by(iterable, mapper, sorter, current_min_element, current_min_mapped) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} ->
        element_mapped = mapper.(element)

        if sorter.(element_mapped, current_min_mapped) do
          do_min_by(iterable, mapper, sorter, element, element_mapped)
        else
          do_min_by(iterable, mapper, sorter, current_min_element, current_min_mapped)
        end

      :done ->
        {:ok, current_min_element}
    end
  end

  @doc """
  Finds the minimal and maximal elements in the iterable.

  ## Example

      iex> Impl.min_max(1..12)
      {:ok, 1, 12}

      iex> Impl.min_max([])
      :done
  """
  @spec min_max(iterable) :: {:ok, element, element} | :done
  def min_max(iterable) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} -> do_min_max(iterable, %{min: element, max: element})
      :done -> :done
    end
  end

  defp do_min_max(iterable, state) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} when element > state.max ->
        do_min_max(iterable, %{state | max: element})

      {:ok, element, iterable} when element < state.min ->
        do_min_max(iterable, %{state | min: element})

      :done ->
        {:ok, state.min, state.max}
    end
  end

  @doc """
  Returns the minimal element in the `iterable` according to Erlang's term ordering.

  ## Examples

      iex> Impl.min([1, 3, 2], &<=/2)
      {:ok, 1}

      iex> Impl.min([], &<=/2)
      :done
  """
  @spec min(iterable, (element, element -> boolean)) :: {:ok, element} | :done
  def min(iterable, sorter) do
    with {:ok, element, iterable} <- Iterable.next(iterable) do
      do_min(iterable, sorter, element)
    end
  end

  defp do_min(iterable, sorter, current_min) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} ->
        if sorter.(element, current_min) do
          do_min(iterable, sorter, element)
        else
          do_min(iterable, sorter, current_min)
        end

      :done ->
        {:ok, current_min}
    end
  end

  @doc """
  Peeks at the first element of the iterable, without consuming it.

  > #### Warning {: .warning}
  > Many iterables cannot be peeked, so this function simulates peeking by
  > consuming an element from the iterable and returning a new iterable which
  > pushes that element back on to the front.

  ## Example

      iex> {:ok, :a, iterable} = Impl.peek([:a, :b, :c])
      ...> Impl.to_list(iterable)
      [:a, :b, :c]
  """
  @spec peek(iterable) :: {:ok, element, iterable} | :done
  def peek(iterable),
    do: iterable |> Iterable.Peeker.new() |> Iterable.peek()

  @doc """
  Peeks at the first `how_many` elements of the iterable, without consuming them.

  > #### Warning {: .warning}
  > Many iterables cannot be peeked, so this function simulates peeking by
  > consuming elements from the iterable and returning a new iterable which
  > pushes those elements back on to the front.

  ## Example

      iex> {:ok, [:a, :b, :c], 3, iterable} = Impl.peek([:a, :b, :c, :d], 3)
      ...> Impl.to_list(iterable)
      [:a, :b, :c, :d]
  """
  @spec peek(iterable, how_many :: pos_integer) ::
          {:ok, [element], non_neg_integer, iterable} | :done
  def peek(iterable, how_many),
    do: iterable |> Iterable.Peeker.new() |> Iterable.peek(how_many)

  @doc """
  Creates a new iterable which places `element` at the beginning of the iterable.

  ## Example

      iex> 1..5
      ...> |> Impl.prepend(6)
      ...> |> Impl.to_list()
      [6, 1, 2, 3, 4, 5]
  """
  @spec prepend(iterable, element) :: iterable
  def prepend(iterable, element), do: Iterable.Prepender.new(iterable, element)

  @doc """
  Creates a new iterable which places `separator` between adjacent items of the original iterable.

  ## Example

      iex> Impl.intersperse([:a, :b, :c], :wat)
      ...> |> Impl.to_list()
      [:a, :wat, :b, :wat, :c]
  """
  @spec intersperse(iterable, any) :: iterable
  def intersperse(iterable, separator), do: Iterable.Intersperser.new(iterable, separator)

  @doc """
  Creates an iterable starting at the same point, but stepping by `how_many` each iteration.

  ## Example

      iex> [0, 1, 2, 3, 4, 5]
      ...> |> Impl.step_by(2)
      ...> |> Impl.to_list()
      [0, 2, 4]
  """
  @spec step_by(iterable, non_neg_integer()) :: iterable
  def step_by(iterable, step_size), do: Iterable.Stepper.new(iterable, step_size)

  @doc """
  Collects the first `how_many` elements into a new iterable and returns it
  along with the advanced initial iterable.

  This is very much like `take/2` except that it returns the remaining iterable
  so that it can be called repeatedly.

  ## Example

      iex> iter = 1..9
      ...> {:ok, [1, 2, 3], iter} = Impl.take_chunk(iter, 3)
      ...> {:ok, [4, 5, 6], iter} = Impl.take_chunk(iter, 3)
      ...> Impl.to_list(iter)
      [7, 8, 9]
  """
  @spec take_chunk(iterable, pos_integer) :: {:ok, iterable, iterable} | {:done, iterable}
  def take_chunk(iterable, how_many) when is_integer(how_many) and how_many > 0,
    do: do_take_chunk(iterable, [], how_many)

  defp do_take_chunk(iterable, result, 0), do: {:ok, :lists.reverse(result), iterable}

  defp do_take_chunk(iterable, result, how_many) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} -> do_take_chunk(iterable, [element | result], how_many - 1)
      :done -> {:done, :lists.reverse(result)}
    end
  end

  @doc """
  Creates an iterable which takes the first `how_many` elements.

  ## Example

      iex> Impl.take_head(1..5, 0)
      ...> |> Impl.to_list()
      []

      iex> Impl.take_head(1..5, 3)
      ...> |> Impl.to_list()
      [1, 2, 3]

  """
  @spec take_head(iterable, non_neg_integer()) :: iterable
  def take_head(iterable, how_many) when is_integer(how_many),
    do: do_take_head(iterable, how_many)

  defp do_take_head(_iterable, 0), do: Iterable.Empty.new()

  defp do_take_head(iterable, how_many) when how_many > 0,
    do: Iterable.HeadTaker.new(iterable, how_many)

  @doc """
  Creates an iterable which takes the last `how_many` elements.

  ## Example

      iex> Impl.take_tail(1..5, 0)
      ...> |> Impl.to_list()
      []

      iex> Impl.take_tail(1..5, 3)
      ...> |> Impl.to_list()
      [3, 4, 5]

  """
  @spec take_tail(iterable, non_neg_integer()) :: iterable
  def take_tail(iterable, how_many) when is_integer(how_many),
    do: do_take_tail(iterable, how_many)

  defp do_take_tail(_iterable, 0), do: Iterable.Empty.new()

  defp do_take_tail(iterable, how_many) when how_many > 0,
    do: Iterable.TailTaker.new(iterable, how_many)

  @doc """
  Creates an iterable which emits elements until `predicate` returns `false`.

  ## Example

      iex> [1, 2, 3]
      ...> |> Impl.take_while(&(&1 < 3))
      ...> |> Impl.to_list()
      [1, 2]
  """
  @spec take_while(iterable, predicate) :: iterable
  def take_while(iterable, predicate) when is_function(predicate, 1),
    do: Iterable.WhileTaker.new(iterable, predicate)

  @doc """
  Convert the iterable into a list.
  """
  @spec to_list(iterable) :: [element]
  def to_list(iterable), do: do_to_list(iterable, [])

  defp do_to_list(iterable, result) do
    case Iterable.next(iterable) do
      {:ok, element, iterable} -> do_to_list(iterable, [element | result])
      :done -> :lists.reverse(result)
    end
  end

  @doc """
  Creates an iterable that only emits unique elements.

  ## Example

      iex> Impl.uniq([:a, :a, :b, :c, :b, :c, :c, :d])
      ...> |> Impl.to_list()
      [:a, :b, :c, :d]
  """
  @spec uniq(iterable) :: iterable
  def uniq(iterable), do: Iterable.Uniquer.new(iterable)

  @doc """
  Creates an iterable which emits the current iteration count as well as the
  next value.

  This is analogous to `Enum.with_index/1` except that counting starts from the
  beginning of the iterable, meaning you can convert an iterable into an
  enumerator after consuming some if it.

  ## Examples

    iex> Impl.with_index([:a, :b, :c])
    ...> |> Impl.to_list()
    [a: 0, b: 1, c: 2]

     iex> [:a, :b, :c, :d]
     ...> |> Impl.drop(2)
     ...> |> Impl.with_index()
     ...> |> Impl.to_list()
     [c: 0, d: 1]
  """
  @spec with_index(iterable) :: iterable
  def with_index(iterable), do: Iterable.WithIndexer.new(iterable)

  @doc """
  Zips corresponding elements from a finite collection of iterables into one iterable of results as computed by `zipper`.

  ## Example

    iex> Impl.zip([1..3, 4..6, 7..9], &List.to_tuple/1)
    ...> |> Impl.to_list()
    [{1, 4, 7}, {2, 5, 8}, {3, 6, 9}]
  """
  @spec zip(iterable, ([element] -> any)) :: iterable
  def zip(iterable, zipper), do: Iterable.Zipper.new(iterable, zipper)
end
