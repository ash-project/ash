defimpl Inspect, for: Graph do
  # For graphs with less than 100 vertices, we'll try to pretty print it,
  # however we should avoid doing so with larger graphs, as it will likely cause outrageous
  # memory consumption, not to mention be expensive to calculate, and the pretty form is not
  # very useful at that size anyway
  def inspect(%Graph{type: type, vertices: vs, out_edges: es, edges: meta}, opts)
      when map_size(vs) < 100 do
    # The goal here is to strip out the ids map, convert the vertices map to a list of vertices
    # and convert the map of edges to their reified forms (i.e. the actual vertex term is used in place of ids)
    # we also want to respect the inspect options as much as possible, so we do this all the hard way by
    # constructing the inspect algebra by hand
    vs_doc = Inspect.Algebra.to_doc(Map.values(vs), opts)

    doc =
      Inspect.Algebra.concat([
        Inspect.Algebra.empty(),
        "#Graph<type: #{type}, vertices:",
        " ",
        vs_doc,
        ",",
        " ",
        "edges: [",
        ""
      ])

    doc =
      Stream.flat_map(es, fn {v_id, out_neighbors} ->
        v = Inspect.Algebra.to_doc(Map.get(vs, v_id), opts)

        Enum.flat_map(out_neighbors, fn out_id ->
          out_v = Map.get(vs, out_id)
          out_v_doc = Inspect.Algebra.to_doc(out_v, opts)

          Enum.map(Map.fetch!(meta, {v_id, out_id}), fn
            {nil, _} when type == :directed ->
              [v, " -> ", out_v_doc]

            {nil, _} ->
              [v, " <-> ", out_v_doc]

            {label, _}
            when type == :directed and (is_binary(label) or is_number(label) or is_atom(label)) ->
              [v, " -[#{label}]-> ", out_v_doc]

            {label, _} when is_binary(label) or is_number(label) or is_atom(label) ->
              [v, " <-[#{label}]-> ", out_v_doc]

            {label, _} when type == :directed ->
              [v, " -[#{inspect(label)}]-> ", out_v_doc]

            {label, _} ->
              [v, " <-[#{inspect(label)}]-> ", out_v_doc]
          end)
        end)
      end)
      |> Enum.intersperse(", ")
      |> Enum.reduce(doc, fn
        doc_part, doc when is_list(doc_part) ->
          Inspect.Algebra.concat([doc | doc_part])

        doc_part, doc ->
          Inspect.Algebra.concat(doc, doc_part)
      end)

    Inspect.Algebra.concat(doc, "]>")
  end

  # For large graphs, just print summary information about the graph
  def inspect(%Graph{type: type} = g, _opts) do
    num_vertices = Graph.num_vertices(g)
    num_edges = Graph.num_edges(g)
    "#Graph<type: #{type}, num_vertices: #{num_vertices}, num_edges: #{num_edges}>"
  end
end
