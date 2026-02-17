defmodule Graph.Serializers.Edgelist do
  @moduledoc """
  This serializer converts a `Graph` to an edgelist suitable for using with
  graph libraries such as the polyglot igraph library.
  """

  use Graph.Serializer
  alias Graph.Serializer

  @spec serialize(Graph.t()) :: {:ok, String.t()}
  def serialize(g) do
    result = "#{serialize_edges(g)}\n"

    {:ok, result}
  end

  @spec serialize_edges(Graph.t()) :: String.t()
  defp serialize_edges(%Graph{vertices: vertices, out_edges: oe} = g) do
    Enum.reduce(vertices, [], fn {id, v}, acc ->
      v_label = Serializer.get_vertex_label(g, id, v)

      edges =
        oe
        |> Map.get(id, MapSet.new())
        |> Enum.map(fn id2 ->
          v2_label = Serializer.get_vertex_label(g, id2, Map.get(vertices, id2))
          {v_label, v2_label}
        end)

      case edges do
        [] -> acc
        _ -> acc ++ edges
      end
    end)
    |> Enum.map(fn {v_label, v2_label} -> "#{v_label} #{v2_label}" end)
    |> Enum.join("\n")
  end
end
