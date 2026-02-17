defmodule Graph.Directed do
  @moduledoc false
  @compile {:inline, [in_neighbors: 2, in_neighbors: 3, out_neighbors: 2, out_neighbors: 3]}

  def topsort(%Graph{vertices: vs} = g) do
    l = reverse_postorder(g)

    if length(forest(g, &in_neighbors/3, l)) == map_size(vs) do
      Enum.map(l, &Map.get(vs, &1))
    else
      false
    end
  end

  def preorder(%Graph{vertices: vs} = g) do
    g
    |> reverse_preorder()
    |> Stream.map(fn id -> Map.get(vs, id) end)
    |> Enum.reverse()
  end

  def postorder(%Graph{vertices: vs} = g) do
    g
    |> reverse_postorder()
    |> Stream.map(fn id -> Map.get(vs, id) end)
    |> Enum.reverse()
  end

  def is_arborescence?(%Graph{} = g) do
    arborescence_root(g) != nil
  end

  def arborescence_root(%Graph{vertices: vs, out_edges: oe} = g) do
    num_edges = Enum.reduce(oe, 0, fn {_, out}, sum -> sum + MapSet.size(out) end)
    num_vertices = map_size(vs)

    if num_edges == num_vertices - 1 do
      [root] =
        Enum.reduce(vs, [], fn {v_id, v}, acc ->
          case length(in_neighbors(g, v_id)) do
            1 -> acc
            0 when acc == [] -> [v]
          end
        end)

      root
    else
      nil
    end
  catch
    _type, _err ->
      nil
  end

  def is_acyclic?(%Graph{} = g) do
    has_loops?(g) == false and topsort(g) != false
  end

  def has_loops?(%Graph{vertices: vs} = g) do
    for {v_id, _} <- vs do
      if is_reflexive_vertex(g, v_id) do
        throw(:has_loop)
      end
    end

    false
  catch
    _, :has_loop ->
      true
  end

  def loop_vertices(%Graph{vertices: vs} = g) do
    for {v_id, v} <- vs, is_reflexive_vertex(g, v_id), do: v
  end

  def components(%Graph{vertices: vs} = g) do
    for component <- forest(g, &inout/3) do
      for id <- component, do: Map.get(vs, id)
    end
  end

  def strong_components(%Graph{vertices: vs} = g) do
    for component <- forest(g, &in_neighbors/3, reverse_postorder(g)) do
      for id <- component, do: Map.get(vs, id)
    end
  end

  def reachable(%Graph{vertices: vertices, vertex_identifier: vertex_identifier} = g, vs)
      when is_list(vs) do
    vs = Enum.map(vs, vertex_identifier)
    for id <- :lists.append(forest(g, &out_neighbors/3, vs, :first)), do: Map.get(vertices, id)
  end

  def reachable_neighbors(
        %Graph{vertices: vertices, vertex_identifier: vertex_identifier} = g,
        vs
      )
      when is_list(vs) do
    vs = Enum.map(vs, vertex_identifier)

    for id <- :lists.append(forest(g, &out_neighbors/3, vs, :not_first)),
        do: Map.get(vertices, id)
  end

  def reaching(%Graph{vertices: vertices, vertex_identifier: vertex_identifier} = g, vs)
      when is_list(vs) do
    vs = Enum.map(vs, vertex_identifier)
    for id <- :lists.append(forest(g, &in_neighbors/3, vs, :first)), do: Map.get(vertices, id)
  end

  def reaching_neighbors(%Graph{vertices: vertices, vertex_identifier: vertex_identifier} = g, vs)
      when is_list(vs) do
    vs = Enum.map(vs, vertex_identifier)
    for id <- :lists.append(forest(g, &in_neighbors/3, vs, :not_first)), do: Map.get(vertices, id)
  end

  def in_neighbors(%Graph{} = g, v, []) do
    in_neighbors(g, v)
  end

  def in_neighbors(%Graph{in_edges: ie}, v, vs) do
    case Map.get(ie, v) do
      nil -> vs
      v_in -> MapSet.to_list(v_in) ++ vs
    end
  end

  def in_neighbors(%Graph{in_edges: ie}, v) do
    case Map.get(ie, v) do
      nil -> []
      v_in -> MapSet.to_list(v_in)
    end
  end

  def out_neighbors(%Graph{} = g, v, []) do
    out_neighbors(g, v)
  end

  def out_neighbors(%Graph{out_edges: oe}, v, vs) do
    case Map.get(oe, v) do
      nil -> vs
      v_out -> MapSet.to_list(v_out) ++ vs
    end
  end

  def out_neighbors(%Graph{out_edges: oe}, v) do
    case Map.get(oe, v) do
      nil -> []
      v_out -> MapSet.to_list(v_out)
    end
  end

  ## Private

  defp is_reflexive_vertex(g, v) do
    Enum.member?(out_neighbors(g, v), v)
  end

  defp forest(%Graph{vertices: vs} = g, fun) do
    forest(g, fun, Map.keys(vs))
  end

  defp forest(g, fun, vs) do
    forest(g, fun, vs, :first)
  end

  defp forest(g, fun, vs, handle_first) do
    {_, acc} =
      List.foldl(vs, {MapSet.new(), []}, fn v, {visited, acc} ->
        pretraverse(handle_first, v, fun, g, visited, acc)
      end)

    acc
  end

  defp pretraverse(:first, v, fun, g, visited, acc) do
    ptraverse([v], fun, g, visited, [], acc)
  end

  defp pretraverse(:not_first, v, fun, g, visited, acc) do
    if MapSet.member?(visited, v) do
      {visited, acc}
    else
      ptraverse(fun.(g, v, []), fun, g, visited, [], acc)
    end
  end

  defp ptraverse([v | vs], fun, g, visited, results, acc) do
    if MapSet.member?(visited, v) do
      ptraverse(vs, fun, g, visited, results, acc)
    else
      visited = MapSet.put(visited, v)
      ptraverse(fun.(g, v, vs), fun, g, visited, [v | results], acc)
    end
  end

  defp ptraverse([], _fun, _g, visited, [], acc), do: {visited, acc}
  defp ptraverse([], _fun, _g, visited, results, acc), do: {visited, [results | acc]}

  defp reverse_preorder(g) do
    :lists.append(forest(g, &out_neighbors/3))
  end

  defp reverse_postorder(%Graph{vertices: vs} = g) do
    {_, l} = posttraverse(Map.keys(vs), g, MapSet.new(), [])
    l
  end

  defp posttraverse([v | vs], g, visited, acc) do
    {visited, acc} =
      if MapSet.member?(visited, v) do
        {visited, acc}
      else
        visited = MapSet.put(visited, v)
        {visited2, acc2} = posttraverse(out_neighbors(g, v, []), g, visited, acc)
        {visited2, [v | acc2]}
      end

    posttraverse(vs, g, visited, acc)
  end

  defp posttraverse([], _g, visited, acc), do: {visited, acc}

  defp inout(g, v, vs) do
    in_neighbors(g, v, out_neighbors(g, v, vs))
  end
end
