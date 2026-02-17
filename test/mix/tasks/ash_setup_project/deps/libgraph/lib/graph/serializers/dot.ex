defmodule Graph.Serializers.DOT do
  @moduledoc """
  This serializer converts a Graph to a DOT file, which can then be converted
  to a great many other formats using Graphviz, e.g. `dot -Tpng out.dot > out.png`.
  """
  use Graph.Serializer
  alias Graph.Serializer

  def serialize(%Graph{type: type} = g) do
    type = if type == :directed, do: "digraph", else: "graph"
    result = "strict #{type} {\n" <> serialize_nodes(g) <> serialize_edges(g) <> "}\n"
    {:ok, result}
  end

  defp serialize_nodes(%Graph{vertices: vertices} = g) do
    Enum.reduce(vertices, "", fn {id, v}, acc ->
      acc <> Serializer.indent(1) <> "#{id}" <> "[label=" <> Serializer.get_vertex_label(g, id, v) <> "]\n"
    end)
  end

  defp serialize_edges(%Graph{type: type, vertices: vertices, out_edges: oe, edges: em} = _g) do
    edges =
      Enum.reduce(vertices, [], fn {id, _v}, acc ->
        edges =
          oe
          |> Map.get(id, MapSet.new())
          |> Enum.flat_map(fn id2 ->
            Enum.map(Map.fetch!(em, {id, id2}), fn
              {nil, weight} ->
                {id, id2, weight}

              {label, weight} ->
                {id, id2, weight, Serializer.encode_label(label)}
            end)
          end)

        case edges do
          [] -> acc
          _ -> acc ++ edges
        end
      end)

    arrow = if type == :directed, do: "->", else: "--"

    Enum.reduce(edges, "", fn
      {v_id, v2_id, weight, edge_label}, acc ->
        acc <>
          Serializer.indent(1) <>
          "#{v_id}" <>
          " #{arrow} " <> "#{v2_id}" <> " [" <> "label=#{edge_label}; weight=#{weight}" <> "]\n"

      {v_id, v2_id, weight}, acc ->
        acc <>
          Serializer.indent(1) <>
          "#{v_id}" <> " #{arrow} " <> "#{v2_id}" <> " [" <> "weight=#{weight}" <> "]\n"
    end)
  end
end
