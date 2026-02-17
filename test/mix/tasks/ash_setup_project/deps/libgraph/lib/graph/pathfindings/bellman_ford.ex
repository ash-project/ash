defmodule Pathfindings.BellmanFord do
  @moduledoc """
  The Bellman–Ford algorithm is an algorithm that computes shortest paths from a single
  source vertex to all of the other vertices in a weighted digraph.
  It is capable of handling graphs in which some of the edge weights are negative numbers

  Time complexity: O(VLogV)
  """
  import Graph.Utils, only: [vertex_id: 1]

  @type distance :: %{Graph.vertex_id => integer}

  @doc """
  Returns nil when graph has negative cycle.
  """
  @spec call(Graph.t, Graph.vertex) :: [Graph.vertex] | nil
  def call(%Graph{vertices: vs, edges: meta}, a) do
    distances = a |> vertex_id |> init_distances(vs)

    distances = vs |> Enum.reduce(distances, fn (_vertex, distances) ->
      meta |> Enum.reduce(distances, &update_distance(&1, &2))
    end)

    if has_negative_cycle?(distances, meta) do
      nil
    else
      distances
    end
  end

  @spec init_distances(Graph.vertex, Graph.vertices) :: distance
  defp init_distances(vertex_id, vertices) do
    Enum.reduce(vertices, Map.new, fn {id, _vertex}, dist ->
      Map.put(dist, id, init_distance_value(id, vertex_id))
    end)
  end

  defp init_distance_value(vertex_id, id) when vertex_id == id, do: 0
  defp init_distance_value(_, _), do: :infinity

  @spec update_distance(term, distance) :: distance
  defp update_distance({{u, v}, _} = edge, distances) do
    weight = edge_weight(edge)

    if distances[u] != :infinity and distances[u] + weight < distances[v] do
      Map.replace!(distances, v, distances[u] + weight)
    else
      distances
    end
  end

  @spec edge_weight(term) :: float
  defp edge_weight({_, edge_value}), do: edge_value |> Map.values |> List.first

  defp has_negative_cycle?(distances, meta) do
    meta |> Enum.reduce(false, fn({{u, v}, _} = edge, has_cycle) ->
      weight = edge_weight(edge)

      has_cycle or distances[u] != :infinity and distances[u] + weight < distances[v]
    end)
  end
end
