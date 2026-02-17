defmodule StreamData.LazyTree do
  @moduledoc false

  # A lazy tree structure.
  #
  # A lazy tree has a root (which is always a realized term) and a possibly lazy
  # enumerable of children (which are in turn lazy trees). This allows to create
  # infinitely deep trees where the children are a lazy stream that can be
  # realized on demand.

  defstruct [:root, children: []]

  @type t(node) :: %__MODULE__{
          root: node,
          children: Enumerable.t()
        }

  @doc """
  Maps the given `fun` over the given `lazy_tree`.

  The given function `fun` is applied eagerly to the root of the given tree,
  and then lazily to the children of such tree. This means that mapping over a tree
  is a cheap operation because it only actually calls `fun` once until children
  are realized.

  ## Examples

      iex> tree = %StreamData.LazyTree{root: 1, children: []}
      iex> mapped_tree = StreamData.LazyTree.map(tree, & -&1)
      iex> mapped_tree.root
      -1

  """
  @spec map(t(a), (a -> b)) :: t(b) when a: term(), b: term()
  def map(%__MODULE__{root: root, children: children}, fun) when is_function(fun, 1) do
    %__MODULE__{root: fun.(root), children: Stream.map(children, &map(&1, fun))}
  end

  @doc """
  Maps and filters the given `lazy_tree` in one go using the given function `fun`.

  `fun` can return either `{:cont, mapped_term}` or `:skip`. If it returns
  `{:cont, mapped_term}`, then `mapped_term` will replace the original item passed
  to `fun` in the given tree. If it returns `:skip`, the tree the item passed to
  `fun` belongs to is filtered out of the resulting tree (the whole tree is filtered
  out, not just the root).

  ## Examples

      iex> tree = %StreamData.LazyTree{root: 2, children: []}
      iex> {:ok, mapped_tree} =
      ...>   StreamData.LazyTree.filter_map(tree, fn integer ->
      ...>     if rem(integer, 2) == 0 do
      ...>       {:cont, -integer}
      ...>     else
      ...>       :skip
      ...>     end
      ...>   end)
      iex> mapped_tree.root
      -2

  """
  @spec filter_map(t(a), (a -> {:cont, b} | :skip)) :: {:ok, t(b)} | :error
        when a: term(),
             b: term()
  def filter_map(%__MODULE__{root: root, children: children}, fun) when is_function(fun, 1) do
    case fun.(root) do
      {:cont, new_root} ->
        new_children =
          Stream.flat_map(children, fn child ->
            case filter_map(child, fun) do
              {:ok, new_child} -> [new_child]
              :error -> []
            end
          end)

        {:ok, %__MODULE__{root: new_root, children: new_children}}

      :skip ->
        :error
    end
  end

  @doc """
  Takes a tree of trees and flattens it to a tree of elements in those trees.

  The tree is flattened so that the root and its children always come "before"
  (as in higher or more towards the left in the tree) the children of `tree`.

  ## Examples

      iex> tree =
      ...>   %StreamData.LazyTree{root: 1, children: []}
      ...>   |> StreamData.LazyTree.map(&%StreamData.LazyTree{root: &1, children: []})
      ...>   |> StreamData.LazyTree.flatten()
      iex> tree.root
      1

  """
  @spec flatten(t(t(a))) :: t(a) when a: term()
  def flatten(%__MODULE__{root: child, children: children}) do
    %__MODULE__{root: child_root, children: child_children} = child

    %__MODULE__{
      root: child_root,
      children: Stream.concat(child_children, Stream.map(children, &flatten/1))
    }
  end

  @doc """
  Filters element out of `tree` that don't satisfy the given `predicate`.

  When an element of `tree` doesn't satisfy `predicate`, the whole tree whose
  root is that element is filtered out of the original `tree`.

  Note that this function does not apply `predicate` to the root of `tree`, just
  to its children (and recursively down). This behaviour exists because if the
  root of `tree` did not satisfy `predicate`, the return value couldn't be a
  tree at all.

  ## Examples

      iex> children = [%StreamData.LazyTree{root: 3, children: []}]
      iex> tree = %StreamData.LazyTree{root: 2, children: children}
      iex> filtered_tree = StreamData.LazyTree.filter(tree, &(rem(&1, 2) == 0))
      iex> filtered_tree.root
      2
      iex> Enum.to_list(filtered_tree.children)
      []

  """
  @spec filter(t(a), (a -> as_boolean(term()))) :: t(a) when a: term()
  def filter(%__MODULE__{children: children} = tree, predicate) when is_function(predicate, 1) do
    children =
      Stream.flat_map(children, fn %__MODULE__{root: child_root} = child ->
        if predicate.(child_root) do
          [filter(child, predicate)]
        else
          []
        end
      end)

    %__MODULE__{tree | children: children}
  end

  @doc """
  Zips a list of trees into a single tree.

  Each element in the resulting tree is a list of as many elements as there are
  trees in `trees`. Each of these elements is going to be a list where each element
  comes from the corresponding tree in `tree`. All permutations of children are
  generated (lazily).

  ## Examples

      iex> trees = [%StreamData.LazyTree{root: 1, children: []}, %StreamData.LazyTree{root: 2, children: []}]
      iex> StreamData.LazyTree.zip(trees).root
      [1, 2]

  """
  @spec zip([t(a)]) :: t([a]) when a: term()
  def zip(trees) do
    %__MODULE__{
      root: Enum.map(trees, fn %{root: root} -> root end),
      children:
        trees
        |> permutations()
        |> Stream.map(&zip/1)
    }
  end

  defp permutations(trees) when is_list(trees) do
    trees
    |> Stream.with_index()
    |> Stream.flat_map(fn {%__MODULE__{children: children}, index} ->
      Enum.map(children, &List.replace_at(trees, index, &1))
    end)
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(tree, options) do
      children = if Enum.empty?(tree.children), do: "[]", else: "[...]"
      concat(["#LazyTree<", to_doc(tree.root, options), ", #{children}>"])
    end
  end
end
