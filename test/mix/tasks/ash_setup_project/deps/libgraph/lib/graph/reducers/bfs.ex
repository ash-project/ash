defmodule Graph.Reducers.Bfs do
  @moduledoc """
  This reducer traverses the graph using Breadth-First Search.
  """
  use Graph.Reducer

  @doc """
  Performs a breadth-first traversal of the graph, applying the provided mapping function to
  each new vertex encountered.

  NOTE: The algorithm will follow lower-weighted edges first.

  Returns a list of values returned from the mapper in the order they were encountered.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 3}])
      ...> #{__MODULE__}.map(g, fn v -> v end)
      [1, 3, 4, 2]
  """
  def map(g, fun) when is_function(fun, 1) do
    g
    |> reduce([], fn v, results -> {:next, [fun.(v) | results]} end)
    |> Enum.reverse()
  end

  @doc """
  Performs a breadth-first traversal of the graph, applying the provided reducer function to
  each new vertex encountered and the accumulator.

  NOTE: The algorithm will follow lower-weighted edges first.

  The result will be the state of the accumulator after the last reduction.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 3}])
      ...> #{__MODULE__}.reduce(g, [], fn v, acc -> {:next, [v|acc]} end)
      [2, 4, 3, 1]

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 3}, {4, 5}])
      ...> #{__MODULE__}.reduce(g, [], fn 5, acc -> {:skip, acc}; v, acc -> {:next, [v|acc]} end)
      [2, 4, 3, 1]

      iex> g = Graph.new |> Graph.add_vertices([1, 2, 3, 4])
      ...> g = Graph.add_edges(g, [{1, 3}, {1, 4}, {3, 2}, {2, 3}, {4, 5}])
      ...> #{__MODULE__}.reduce(g, [], fn 4, acc -> {:halt, acc}; v, acc -> {:next, [v|acc]} end)
      [3, 1]
  """
  def reduce(%Graph{vertices: vs} = g, acc, fun) when is_function(fun, 2) do
    vs
    # Start with a cost of zero
    |> Stream.map(fn {id, _} -> {id, 0} end)
    # Only populate the initial queue with those vertices which have no inbound edges
    |> Stream.reject(fn {id, _cost} -> inbound_edges?(g, id) end)
    |> Enum.reduce(PriorityQueue.new(), fn {id, cost}, q ->
      PriorityQueue.push(q, id, cost)
    end)
    |> traverse(g, MapSet.new(), fun, acc)
  end

  defp inbound_edges?(%Graph{in_edges: ie}, v_id) do
    case Map.get(ie, v_id) do
      nil -> false
      edges -> MapSet.size(edges) > 0
    end
  end

  defp traverse(q, %Graph{out_edges: oe, vertices: vertices} = g, visited, fun, acc) do
    case PriorityQueue.pop(q) do
      {{:value, v_id}, q1} ->
        if MapSet.member?(visited, v_id) do
          traverse(q1, g, visited, fun, acc)
        else
          v = Map.get(vertices, v_id)

          case fun.(v, acc) do
            {:next, acc2} ->
              visited = MapSet.put(visited, v_id)
              v_out = Map.get(oe, v_id, MapSet.new())

              q2 =
                v_out
                |> MapSet.to_list()
                |> Enum.reduce(q1, fn id, q ->
                  weight = Graph.Utils.edge_weight(g, v_id, id)
                  PriorityQueue.push(q, id, weight)
                end)

              traverse(q2, g, visited, fun, acc2)

            {:skip, acc2} ->
              visited = MapSet.put(visited, v_id)
              traverse(q1, g, visited, fun, acc2)

            {:halt, acc2} ->
              acc2
          end
        end

      {:empty, _} ->
        acc
    end
  end
end
