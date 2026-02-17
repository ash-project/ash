defmodule Graph.Reducer do
  @moduledoc """
  Reducers provide a way to traverse a graph while applying a function at each vertex. This
  can be used for a variety of things, most notably though is pre-processing a graph, for example
  one might decorate vertices with their distance from some known landmarks for later use in
  a cost function for A*.

  The `reduce` function takes a callback which has some control over when to terminate the traversal,
  to move to the next vertex, return `{:next, acc}`, but to stop traversal and return the accumulator,
  return `{:halt, acc}`. The `map` function is built on top of `reduce` but does not expose this control,
  so if you need to map over the graph but stop early, you'll want to build your own `map` implementation
  on top of `reduce`.

  Provided out of the box are two reducers, `Graph.Reducers.Bfs` (for breadth-first traversals), and
  `Graph.Reducers.Dfs` (for depth-first traversals). Simply choose the best one for your use case.
  """

  @callback map(g :: Graph.t(), mapper :: (Graph.vertex() -> term)) :: term
  @callback reduce(g :: Graph.t(), acc :: term, reducer :: (Graph.vertex(), term -> term)) ::
              {:next, term}
              | {:halt, term}

  defmacro __using__(_) do
    quote do
      @behaviour Graph.Reducer
    end
  end
end
