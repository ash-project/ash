defmodule Graph.Reducers.Dfs do
  @moduledoc """
  This reducer traverses the graph using Depth-First Search.
  """
  use Graph.Reducer

  @doc """
  Performs a depth-first traversal of the graph, applying the provided mapping function to
  each new vertex encountered.

  NOTE: The algorithm will follow lower-weighted edges first.

  Returns a list of values returned from the mapper in the order they were encountered.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 4}])
      ...> #{__MODULE__}.map(g, fn v -> v end)
      [1, 3, 2, 4]
  """
  def map(g, fun) when is_function(fun, 1) do
    reduce(g, [], fn v, results -> {:next, [fun.(v) | results]} end)
    |> Enum.reverse()
  end

  @doc """
  Performs a depth-first traversal of the graph, applying the provided reducer function to
  each new vertex encountered and the accumulator.

  NOTE: The algorithm will follow lower-weighted edges first.

  The result will be the state of the accumulator after the last reduction.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 4}])
      ...> #{__MODULE__}.reduce(g, [], fn v, acc -> {:next, [v|acc]} end)
      [4, 2, 3, 1]

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4, 5])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 4}, {4, 5}])
      ...> #{__MODULE__}.reduce(g, [], fn 5, acc -> {:skip, acc}; v, acc -> {:next, [v|acc]} end)
      [4, 2, 3, 1]

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4, 5])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 4}, {4, 5}])
      ...> #{__MODULE__}.reduce(g, [], fn 4, acc -> {:halt, acc}; v, acc -> {:next, [v|acc]} end)
      [2, 3, 1]
  """
  def reduce(%Graph{vertices: vs} = g, acc, fun) when is_function(fun, 2) do
    traverse(Map.keys(vs), g, MapSet.new(), fun, acc)
  end

  ## Private

  defp traverse([v_id | rest], %Graph{out_edges: oe, vertices: vs} = g, visited, fun, acc) do
    if MapSet.member?(visited, v_id) do
      traverse(rest, g, visited, fun, acc)
    else
      v = Map.get(vs, v_id)

      case fun.(v, acc) do
        {:next, acc2} ->
          visited = MapSet.put(visited, v_id)

          out =
            oe
            |> Map.get(v_id, MapSet.new())
            |> MapSet.to_list()
            |> Enum.sort_by(fn id -> Graph.Utils.edge_weight(g, v_id, id) end)

          traverse(out ++ rest, g, visited, fun, acc2)

        {:skip, acc2} ->
          # Skip this vertex and it's out-neighbors
          visited = MapSet.put(visited, v_id)
          traverse(rest, g, visited, fun, acc2)

        {:halt, acc2} ->
          acc2
      end
    end
  end

  defp traverse([], _g, _visited, _fun, acc) do
    acc
  end
end
