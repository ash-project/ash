defmodule PriorityQueue do
  @moduledoc """
  This module defines a priority queue datastructure, intended for use with graphs, as it prioritizes
  lower priority values over higher priority values (ideal for priorities based on edge weights, etc.).

  This implementation makes use of `:gb_trees` under the covers. It is also very fast, even for a very large
  number of distinct priorities. Other priority queue implementations I've looked at are either slow when working
  with large numbers of priorities, or restrict themselves to a specific number of allowed priorities, which is
  why I've ended up writing my own.
  """
  defstruct priorities: nil

  @type t :: %__MODULE__{
          priorities: :gb_trees.tree(integer, :queue.queue(term))
        }

  @doc """
  Create a new priority queue
  """
  @spec new() :: t
  def new do
    %__MODULE__{priorities: :gb_trees.empty()}
  end

  @doc """
  Push a new element into the queue with the given priority.

  Priorities must be integer or float values.

  ## Example

      iex> pq = PriorityQueue.new
      ...> pq = PriorityQueue.push(pq, :foo, 1)
      ...> {result, _} = PriorityQueue.pop(pq)
      ...> result
      {:value, :foo}

      iex> pq = PriorityQueue.new
      ...> pq = PriorityQueue.push(pq, :foo, 1)
      ...> {{:value, :foo}, pq} = PriorityQueue.pop(pq)
      ...> pq = PriorityQueue.push(pq, :bar, 1)
      ...> {result, _} = PriorityQueue.pop(pq)
      ...> result
      {:value, :bar}
  """
  @spec push(t, term, integer | float) :: t
  def push(%__MODULE__{priorities: tree} = pq, term, priority) do
    if :gb_trees.size(tree) > 0 do
      case :gb_trees.lookup(priority, tree) do
        :none ->
          q = :queue.in(term, :queue.new())
          %__MODULE__{pq | priorities: :gb_trees.insert(priority, q, tree)}

        {:value, q} ->
          q = :queue.in(term, q)
          %__MODULE__{pq | priorities: :gb_trees.update(priority, q, tree)}
      end
    else
      q = :queue.in(term, :queue.new())
      %__MODULE__{pq | priorities: :gb_trees.insert(priority, q, tree)}
    end
  end

  @doc """
  This function returns the value at the top of the queue. If the queue is empty, `:empty`
  is returned, otherwise `{:value, term}`. This function does not modify the queue.

  ## Example

      iex> pq = PriorityQueue.new |> PriorityQueue.push(:foo, 1)
      ...> {:value, :foo} = PriorityQueue.peek(pq)
      ...> {{:value, val}, _} = PriorityQueue.pop(pq)
      ...> val
      :foo
  """
  @spec peek(t) :: :empty | {:value, term}
  def peek(%__MODULE__{} = pq) do
    case pop(pq) do
      {:empty, _} ->
        :empty

      {{:value, _} = val, _} ->
        val
    end
  end

  @doc """
  Pops an element from the queue with the lowest integer value priority.

  Returns `{:empty, PriorityQueue.t}` if there are no elements left to dequeue.

  Returns `{{:value, term}, PriorityQueue.t}` if the dequeue is successful

  This is equivalent to the `extract-min` operation described in priority queue theory.

  ## Example

      iex> pq = PriorityQueue.new
      ...> pq = Enum.reduce(Enum.shuffle(0..4), pq, fn i, pq -> PriorityQueue.push(pq, ?a+i, i) end)
      ...> {{:value, ?a}, pq} = PriorityQueue.pop(pq)
      ...> {{:value, ?b}, pq} = PriorityQueue.pop(pq)
      ...> {{:value, ?c}, pq} = PriorityQueue.pop(pq)
      ...> {{:value, ?d}, pq} = PriorityQueue.pop(pq)
      ...> {{:value, ?e}, pq} = PriorityQueue.pop(pq)
      ...> {result, _} = PriorityQueue.pop(pq)
      ...> result
      :empty
  """
  @spec pop(t) :: {:empty, t} | {{:value, term}, t}
  def pop(%__MODULE__{priorities: tree} = pq) do
    if :gb_trees.size(tree) > 0 do
      {min_pri, q, tree2} = :gb_trees.take_smallest(tree)

      case :queue.out(q) do
        {:empty, _} ->
          pop(%__MODULE__{pq | priorities: tree2})

        {{:value, _} = val, q2} ->
          {val, %__MODULE__{pq | priorities: :gb_trees.update(min_pri, q2, tree)}}
      end
    else
      {:empty, pq}
    end
  end

  defimpl Inspect do
    def inspect(%PriorityQueue{priorities: tree}, opts) do
      if :gb_trees.size(tree) > 0 do
        items =
          tree
          |> :gb_trees.to_list()
          |> Enum.flat_map(fn {_priority, q} -> :queue.to_list(q) end)

        count = Enum.count(items)
        doc = Inspect.Algebra.to_doc(items, opts)
        Inspect.Algebra.concat(["#PriorityQueue<size: #{count}, queue: ", doc, ">"])
      else
        "#PriorityQueue<size: 0, queue: []>"
      end
    end
  end
end
