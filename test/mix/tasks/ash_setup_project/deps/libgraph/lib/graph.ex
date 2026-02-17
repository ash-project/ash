defmodule Graph do
  @moduledoc """
  This module defines a graph data structure, which supports directed and undirected graphs, in both acyclic and cyclic forms.
  It also defines the API for creating, manipulating, and querying that structure.

  As far as memory usage is concerned, `Graph` should be fairly compact in memory, but if you want to do a rough
  comparison between the memory usage for a graph between `libgraph` and `digraph`, use `:digraph.info/1` and
  `Graph.info/1` on the two graphs, and both results will contain memory usage information. Keep in mind we don't have a precise
  way to measure the memory usage of a term in memory, whereas ETS is able to give a more precise answer, but we do have
  a fairly good way to estimate the usage of a term, and we use that method within `libgraph`.

  The Graph struct is structured like so:

  - A map of vertex ids to vertices (`vertices`)
  - A map of vertex ids to their out neighbors (`out_edges`),
  - A map of vertex ids to their in neighbors (`in_edges`), effectively the transposition of `out_edges`
  - A map of vertex ids to vertex labels (`vertex_labels`), (labels are only stored if a non-nil label was provided)
  - A map of edge ids (where an edge id is simply a tuple of `{vertex_id, vertex_id}`) to a map of edge metadata (`edges`)
  - Edge metadata is a map of `label => weight`, and each entry in that map represents a distinct edge. This allows
    us to support multiple edges in the same direction between the same pair of vertices, but for many purposes simply
    treat them as a single logical edge.

  This structure is designed to be as efficient as possible once a graph is built, but it turned out that it is also
  quite efficient for manipulating the graph as well. For example, splitting an edge and introducing a new vertex on that
  edge can be done with very little effort. We use vertex ids everywhere because we can generate them without any lookups,
  we don't incur any copies of the vertex structure, and they are very efficient as keys in a map.
  """
  defstruct in_edges: %{},
            out_edges: %{},
            edges: %{},
            vertex_labels: %{},
            vertices: %{},
            type: :directed,
            vertex_identifier: &Graph.Utils.vertex_id/1

  alias Graph.{Edge, EdgeSpecificationError}

  @typedoc """
  Identifier of a vertex. By default a non_neg_integer from `Graph.Utils.vertex_id/1` utilizing `:erlang.phash2`.
  """
  @type vertex_id :: non_neg_integer() | term()
  @type vertex :: term
  @type label :: term
  @type edge_weight :: integer | float
  @type edge_key :: {vertex_id, vertex_id}
  @type edge_value :: %{label => edge_weight}
  @type graph_type :: :directed | :undirected
  @type vertices :: %{vertex_id => vertex}
  @type t :: %__MODULE__{
          in_edges: %{vertex_id => MapSet.t()},
          out_edges: %{vertex_id => MapSet.t()},
          edges: %{edge_key => edge_value},
          vertex_labels: %{vertex_id => term},
          vertices: %{vertex_id => vertex},
          type: graph_type,
          vertex_identifier: (vertex() -> term())
        }
  @type graph_info :: %{
          :num_edges => non_neg_integer(),
          :num_vertices => non_neg_integer(),
          :size_in_bytes => number(),
          :type => :directed | :undirected
        }

  @doc """
  Creates a new graph using the provided options.

  ## Options

  - `type: :directed | :undirected`, specifies what type of graph this is. Defaults to a `:directed` graph.
  - `vertex_identifier`: a function which accepts a vertex and returns a unique identifier of said vertex.
    Defaults to `Graph.Utils.vertex_id/1`, a hash of the whole vertex utilizing `:erlang.phash2/2`.

  ## Example

      iex> Graph.new()
      #Graph<type: directed, vertices: [], edges: []>

      iex> g = Graph.new(type: :undirected) |> Graph.add_edges([{:a, :b}, {:b, :a}])
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b}]

      iex> g = Graph.new(type: :directed) |> Graph.add_edges([{:a, :b}, {:b, :a}])
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b}, %Graph.Edge{v1: :b, v2: :a}]

      iex> g = Graph.new(vertex_identifier: fn v -> :erlang.phash2(v) end) |> Graph.add_edges([{:a, :b}, {:b, :a}])
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b}, %Graph.Edge{v1: :b, v2: :a}]
  """
  def new(opts \\ []) do
    type = Keyword.get(opts, :type) || :directed
    vertex_identifier = Keyword.get(opts, :vertex_identifier) || (&Graph.Utils.vertex_id/1)
    %__MODULE__{type: type, vertex_identifier: vertex_identifier}
  end

  @doc """
  Returns a map of summary information about this graph.

  NOTE: The `size_in_bytes` value is an estimate, not a perfectly precise value, but
  should be close enough to be useful.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = g |> Graph.add_edges([{:a, :b}, {:b, :c}])
      ...> match?(%{type: :directed, num_vertices: 4, num_edges: 2}, Graph.info(g))
      true
  """
  @spec info(t) :: graph_info()
  def info(%__MODULE__{type: type} = g) do
    %{
      type: type,
      num_edges: num_edges(g),
      num_vertices: num_vertices(g),
      size_in_bytes: Graph.Utils.sizeof(g)
    }
  end

  @doc """
  Converts the given Graph to DOT format, which can then be converted to
  a number of other formats via Graphviz, e.g. `dot -Tpng out.dot > out.png`.

  If labels are set on a vertex, then those labels are used in the DOT output
  in place of the vertex itself. If no labels were set, then the vertex is
  stringified if it's a primitive type and inspected if it's not, in which
  case the inspect output will be quoted and used as the vertex label in the DOT file.

  Edge labels and weights will be shown as attributes on the edge definitions, otherwise
  they use the same labelling scheme for the involved vertices as described above.

  NOTE: Currently this function assumes graphs are directed graphs, but in the future
  it will support undirected graphs as well.

  ## Example

      > g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      > g = Graph.add_edges([{:a, :b}, {:b, :c}, {:b, :d}, {:c, :d}])
      > g = Graph.label_vertex(g, :a, :start)
      > g = Graph.label_vertex(g, :d, :finish)
      > g = Graph.update_edge(g, :b, :d, weight: 3)
      > IO.puts(Graph.to_dot(g))
      strict digraph {
          start
          b
          c
          finish
          start -> b [weight=1]
          b -> c [weight=1]
          b -> finish [weight=3]
          c -> finish [weight=1]
      }
  """
  @spec to_dot(t) :: {:ok, binary} | {:error, term}
  def to_dot(%__MODULE__{} = g) do
    Graph.Serializers.DOT.serialize(g)
  end

  @spec to_edgelist(t) :: {:ok, binary} | {:error, term}
  def to_edgelist(%__MODULE__{} = g) do
    Graph.Serializers.Edgelist.serialize(g)
  end

  @doc """
  Returns the number of edges in the graph.

  Pseudo-edges (label/weight pairs applied to an edge) are not counted, only distinct
  vertex pairs where an edge exists between them are counted.

  ## Example

      iex> g = Graph.add_edges(Graph.new, [{:a, :b}, {:b, :c}, {:a, :a}])
      ...> Graph.num_edges(g)
      3
  """
  @spec num_edges(t) :: non_neg_integer
  def num_edges(%__MODULE__{out_edges: oe, edges: meta}) do
    Enum.reduce(oe, 0, fn {from, tos}, sum ->
      Enum.reduce(tos, sum, fn to, s ->
        s + map_size(Map.get(meta, {from, to}))
      end)
    end)
  end

  @doc """
  Returns the number of vertices in the graph

  ## Example

      iex> g = Graph.add_vertices(Graph.new, [:a, :b, :c])
      ...> Graph.num_vertices(g)
      3
  """
  @spec num_vertices(t) :: non_neg_integer
  def num_vertices(%__MODULE__{vertices: vs}) do
    map_size(vs)
  end

  @doc """
  Returns true if and only if the graph `g` is a tree.

  This function always returns false for undirected graphs.

  NOTE: Multiple edges between the same pair of vertices in the same direction are
  considered a single edge when determining if the provided graph is a tree.
  """
  @spec is_tree?(t) :: boolean
  def is_tree?(%__MODULE__{type: :undirected}), do: false

  def is_tree?(%__MODULE__{out_edges: es, vertices: vs} = g) do
    num_edges = Enum.reduce(es, 0, fn {_, out}, sum -> sum + MapSet.size(out) end)

    if num_edges == map_size(vs) - 1 do
      length(components(g)) == 1
    else
      false
    end
  end

  @doc """
  Returns true if the graph is an aborescence, a directed acyclic graph,
  where the *root*, a vertex, of the arborescence has a unique path from itself
  to every other vertex in the graph.
  """
  @spec is_arborescence?(t) :: boolean
  def is_arborescence?(%__MODULE__{type: :undirected}), do: false
  def is_arborescence?(%__MODULE__{} = g), do: Graph.Directed.is_arborescence?(g)

  @doc """
  Returns the root vertex of the arborescence, if one exists, otherwise nil.
  """
  @spec arborescence_root(t) :: vertex | nil
  def arborescence_root(%__MODULE__{type: :undirected}), do: nil
  def arborescence_root(%__MODULE__{} = g), do: Graph.Directed.arborescence_root(g)

  @doc """
  Returns true if and only if the graph `g` is acyclic.
  """
  @spec is_acyclic?(t) :: boolean
  defdelegate is_acyclic?(g), to: Graph.Directed

  @doc """
  Returns true if the graph `g` is not acyclic.
  """
  @spec is_cyclic?(t) :: boolean
  def is_cyclic?(%__MODULE__{} = g), do: not is_acyclic?(g)

  @doc """
  Returns true if graph `g1` is a subgraph of `g2`.

  A graph is a subgraph of another graph if it's vertices and edges
  are a subset of that graph's vertices and edges.

  ## Example

      iex> g1 = Graph.new |> Graph.add_vertices([:a, :b, :c, :d]) |> Graph.add_edge(:a, :b) |> Graph.add_edge(:b, :c)
      ...> g2 = Graph.new |> Graph.add_vertices([:b, :c]) |> Graph.add_edge(:b, :c)
      ...> Graph.is_subgraph?(g2, g1)
      true

      iex> g1 = Graph.new |> Graph.add_vertices([:a, :b, :c, :d]) |> Graph.add_edges([{:a, :b}, {:b, :c}])
      ...> g2 = Graph.new |> Graph.add_vertices([:b, :c, :e]) |> Graph.add_edges([{:b, :c}, {:c, :e}])
      ...> Graph.is_subgraph?(g2, g1)
      false
  """
  @spec is_subgraph?(t, t) :: boolean
  def is_subgraph?(%__MODULE__{} = a, %__MODULE__{} = b) do
    meta1 = a.edges
    vs1 = a.vertices
    meta2 = b.edges
    vs2 = b.vertices

    for {v, _} <- vs1 do
      unless Map.has_key?(vs2, v), do: throw(:not_subgraph)
    end

    for {edge_key, g1_edge_meta} <- meta1 do
      case Map.fetch(meta2, edge_key) do
        {:ok, g2_edge_meta} ->
          unless MapSet.subset?(MapSet.new(g1_edge_meta), MapSet.new(g2_edge_meta)) do
            throw(:not_subgraph)
          end

        _ ->
          throw(:not_subgraph)
      end
    end

    true
  catch
    :throw, :not_subgraph ->
      false
  end

  @doc """
  See `dijkstra/3`.
  """
  @spec get_shortest_path(t, vertex, vertex) :: [vertex] | nil
  defdelegate get_shortest_path(g, a, b), to: Graph.Pathfinding, as: :dijkstra

  @doc """
  Gets the shortest path between `a` and `b`.

  As indicated by the name, this uses Dijkstra's algorithm for locating the shortest path, which
  means that edge weights are taken into account when determining which vertices to search next. By
  default, all edges have a weight of 1, so vertices are inspected at random; which causes this algorithm
  to perform a naive depth-first search of the graph until a path is found. If your edges are weighted however,
  this will allow the algorithm to more intelligently navigate the graph.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:b, :c}, {:c, :d}, {:b, :d}])
      ...> Graph.dijkstra(g, :a, :d)
      [:a, :b, :d]

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :c}, {:b, :c}, {:b, :d}])
      ...> Graph.dijkstra(g, :a, :d)
      nil
  """
  @spec dijkstra(t, vertex, vertex) :: [vertex]
  defdelegate dijkstra(g, a, b), to: Graph.Pathfinding

  @doc """
  ## Example

      iex> g = Graph.new |> Graph.add_edges([
      ...>   {:b, :c, weight: -2}, {:a, :b, weight: 1},
      ...>   {:c, :d, weight: 3}, {:b, :d, weight: 4}])
      ...> Graph.bellman_ford(g, :a)
      %{97 => 0, 98 => 1, 99 => -1, 100 => 2}

      iex> g = Graph.new |> Graph.add_edges([
      ...>   {:b, :c, weight: -2}, {:a, :b, weight: -1},
      ...>   {:c, :d, weight: -3}, {:d, :a, weight: -5}])
      ...> Graph.bellman_ford(g, :a)
      nil
  """
  @spec bellman_ford(t, vertex) :: [vertex]
  defdelegate bellman_ford(g, a), to: Graph.Pathfinding

  @doc """
  Gets the shortest path between `a` and `b`.

  The A* algorithm is very much like Dijkstra's algorithm, except in addition to edge weights, A*
  also considers a heuristic function for determining the lower bound of the cost to go from vertex
  `v` to `b`. The lower bound *must* be less than the cost of the shortest path from `v` to `b`, otherwise
  it will do more harm than good. Dijkstra's algorithm can be reframed as A* where `lower_bound(v)` is always 0.

  This function puts the heuristics in your hands, so you must provide the heuristic function, which should take
  a single parameter, `v`, which is the vertex being currently examined. Your heuristic should then determine what the
  lower bound for the cost to reach `b` from `v` is, and return that value.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:b, :c}, {:c, :d}, {:b, :d}])
      ...> Graph.a_star(g, :a, :d, fn _ -> 0 end)
      [:a, :b, :d]

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :c}, {:b, :c}, {:b, :d}])
      ...> Graph.a_star(g, :a, :d, fn _ -> 0 end)
      nil
  """
  @spec a_star(t, vertex, vertex, (vertex, vertex -> integer)) :: [vertex]
  defdelegate a_star(g, a, b, hfun), to: Graph.Pathfinding

  @doc """
  Builds a list of paths between vertex `a` and vertex `b`.

  The algorithm used here is a depth-first search, which evaluates the whole
  graph until all paths are found. Order is guaranteed to be deterministic,
  but not guaranteed to be in any meaningful order (i.e. shortest to longest).

  ## Example
      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:b, :c}, {:c, :d}, {:b, :d}, {:c, :a}])
      ...> Graph.get_paths(g, :a, :d)
      [[:a, :b, :c, :d], [:a, :b, :d]]

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :c}, {:b, :c}, {:b, :d}])
      ...> Graph.get_paths(g, :a, :d)
      []
  """
  @spec get_paths(t, vertex, vertex) :: [[vertex]]
  defdelegate get_paths(g, a, b), to: Graph.Pathfinding, as: :all

  @doc """
  Return a list of all the edges, where each edge is expressed as a tuple
  of `{A, B}`, where the elements are the vertices involved, and implying the
  direction of the edge to be from `A` to `B`.

  NOTE: You should be careful when using this on dense graphs, as it produces
  lists with whatever you've provided as vertices, with likely many copies of
  each. I'm not sure if those copies are shared in-memory as they are unchanged,
  so it *should* be fairly compact in memory, but I have not verified that to be sure.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.add_vertex(:b) |> Graph.add_vertex(:c)
      ...> g = g |> Graph.add_edge(:a, :c) |> Graph.add_edge(:b, :c)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :c}, %Graph.Edge{v1: :b, v2: :c}]

  """
  @spec edges(t) :: [Edge.t()]
  def edges(%__MODULE__{out_edges: edges, edges: meta, vertices: vs}) do
    edges
    |> Enum.flat_map(fn {source_id, out_neighbors} ->
      source = Map.get(vs, source_id)

      out_neighbors
      |> Enum.flat_map(fn out_neighbor ->
        target = Map.get(vs, out_neighbor)
        meta = Map.get(meta, {source_id, out_neighbor})

        Enum.map(meta, fn {label, weight} ->
          Edge.new(source, target, label: label, weight: weight)
        end)
      end)
    end)
  end

  @doc """
  Returns a list of all edges inbound or outbound from vertex `v`.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:b, :c}])
      ...> Graph.edges(g, :b)
      [%Graph.Edge{v1: :a, v2: :b}, %Graph.Edge{v1: :b, v2: :c}]

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:b, :c}])
      ...> Graph.edges(g, :d)
      []
  """
  @spec edges(t, vertex) :: [Edge.t()]
  def edges(
        %__MODULE__{
          in_edges: ie,
          out_edges: oe,
          edges: meta,
          vertices: vs,
          vertex_identifier: vertex_identifier
        },
        v
      ) do
    v_id = vertex_identifier.(v)
    v_in = Map.get(ie, v_id) || MapSet.new()
    v_out = Map.get(oe, v_id) || MapSet.new()
    v_all = MapSet.union(v_in, v_out)

    e_in =
      Enum.flat_map(v_all, fn v2_id ->
        case Map.get(meta, {v2_id, v_id}) do
          nil ->
            []

          edge_meta when is_map(edge_meta) ->
            v2 = Map.get(vs, v2_id)

            for {label, weight} <- edge_meta do
              Edge.new(v2, v, label: label, weight: weight)
            end
        end
      end)

    e_out =
      Enum.flat_map(v_all, fn v2_id ->
        case Map.get(meta, {v_id, v2_id}) do
          nil ->
            []

          edge_meta when is_map(edge_meta) ->
            v2 = Map.get(vs, v2_id)

            for {label, weight} <- edge_meta do
              Edge.new(v, v2, label: label, weight: weight)
            end
        end
      end)

    e_in ++ e_out
  end

  @doc """
  Returns a list of all edges between `v1` and `v2`.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(:a, :b, label: :uses)
      ...> g = Graph.add_edge(g, :a, :b, label: :contains)
      ...> Graph.edges(g, :a, :b)
      [%Graph.Edge{v1: :a, v2: :b, label: :contains}, %Graph.Edge{v1: :a, v2: :b, label: :uses}]

      iex> g = Graph.new(type: :undirected) |> Graph.add_edge(:a, :b, label: :uses)
      ...> g = Graph.add_edge(g, :a, :b, label: :contains)
      ...> Graph.edges(g, :a, :b)
      [%Graph.Edge{v1: :a, v2: :b, label: :contains}, %Graph.Edge{v1: :a, v2: :b, label: :uses}]
  """
  @spec edges(t, vertex, vertex) :: [Edge.t()]
  def edges(%__MODULE__{type: type, edges: meta, vertex_identifier: vertex_identifier}, v1, v2) do
    with v1_id <- vertex_identifier.(v1),
         v2_id <- vertex_identifier.(v2),
         edge_key <- {v1_id, v2_id},
         edge_meta <- Map.get(meta, edge_key, %{}) do
      case type do
        :directed ->
          edge_list(v1, v2, edge_meta, type)

        :undirected ->
          edge_meta2 = Map.get(meta, {v2_id, v1_id}, %{})
          merged_meta = Map.merge(edge_meta, edge_meta2)

          edge_list(v1, v2, merged_meta, type)
      end
    end
  end

  defp edge_list(v1, v2, edge_meta, :undirected) do
    for {label, weight} <- edge_meta do
      if v1 > v2 do
        Edge.new(v2, v1, label: label, weight: weight)
      else
        Edge.new(v1, v2, label: label, weight: weight)
      end
    end
  end

  defp edge_list(v1, v2, edge_meta, _) do
    for {label, weight} <- edge_meta do
      Edge.new(v1, v2, label: label, weight: weight)
    end
  end

  @doc """
  Get an Edge struct for a specific vertex pair, or vertex pair + label.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :contains}, {:a, :b, label: :uses}])
      ...> Graph.edge(g, :b, :a)
      nil

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :contains}, {:a, :b, label: :uses}])
      ...> Graph.edge(g, :a, :b)
      %Graph.Edge{v1: :a, v2: :b}

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :contains}, {:a, :b, label: :uses}])
      ...> Graph.edge(g, :a, :b, :contains)
      %Graph.Edge{v1: :a, v2: :b, label: :contains}

      iex> g = Graph.new(type: :undirected) |> Graph.add_edges([{:a, :b}, {:a, :b, label: :contains}, {:a, :b, label: :uses}])
      ...> Graph.edge(g, :a, :b, :contains)
      %Graph.Edge{v1: :a, v2: :b, label: :contains}
  """
  @spec edge(t, vertex, vertex) :: Edge.t() | nil
  @spec edge(t, vertex, vertex, label) :: Edge.t() | nil
  def edge(%__MODULE__{} = g, v1, v2) do
    edge(g, v1, v2, nil)
  end

  def edge(%__MODULE__{type: :undirected} = g, v1, v2, label) do
    if v1 > v2 do
      do_edge(g, v2, v1, label)
    else
      do_edge(g, v1, v2, label)
    end
  end

  def edge(%__MODULE__{} = g, v1, v2, label) do
    do_edge(g, v1, v2, label)
  end

  defp do_edge(%__MODULE__{edges: meta, vertex_identifier: vertex_identifier}, v1, v2, label) do
    with v1_id <- vertex_identifier.(v1),
         v2_id <- vertex_identifier.(v2),
         edge_key <- {v1_id, v2_id},
         {:ok, edge_meta} <- Map.fetch(meta, edge_key),
         {:ok, weight} <- Map.fetch(edge_meta, label) do
      Edge.new(v1, v2, label: label, weight: weight)
    else
      _ ->
        nil
    end
  end

  @doc """
  Returns a list of all the vertices in the graph.

  NOTE: You should be careful when using this on large graphs, as the list it produces
  contains every vertex on the graph. I have not yet verified whether Erlang ensures that
  they are a shared reference with the original, or copies, but if the latter it could result
  in running out of memory if the graph is too large.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.add_vertex(:b)
      ...> Graph.vertices(g)
      [:a, :b]
  """
  @spec vertices(t) :: vertex
  def vertices(%__MODULE__{vertices: vs}) do
    Map.values(vs)
  end

  @doc """
  Returns true if the given vertex exists in the graph. Otherwise false.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b])
      ...> Graph.has_vertex?(g, :a)
      true

      iex> g = Graph.new |> Graph.add_vertices([:a, :b])
      ...> Graph.has_vertex?(g, :c)
      false
  """
  @spec has_vertex?(t, vertex) :: boolean
  def has_vertex?(%__MODULE__{vertices: vs, vertex_identifier: vertex_identifier}, v) do
    v_id = vertex_identifier.(v)
    Map.has_key?(vs, v_id)
  end

  @doc """
  Returns the label for the given vertex.
  If no label was assigned, it returns [].

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.label_vertex(:a, :my_label)
      ...> Graph.vertex_labels(g, :a)
      [:my_label]
  """
  @spec vertex_labels(t, vertex) :: term | []
  def vertex_labels(%__MODULE__{vertex_labels: labels, vertex_identifier: vertex_identifier}, v) do
    with v1_id <- vertex_identifier.(v),
         true <- Map.has_key?(labels, v1_id) do
      Map.get(labels, v1_id)
    else
      _ -> []
    end
  end

  @doc """
  Adds a new vertex to the graph. If the vertex is already present in the graph, the add is a no-op.

  You can provide optional labels for the vertex, aside from the variety of uses this has for working
  with graphs, labels will also be used when exporting a graph in DOT format.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a, :mylabel) |> Graph.add_vertex(:a)
      ...> [:a] = Graph.vertices(g)
      ...> Graph.vertex_labels(g, :a)
      [:mylabel]

      iex> g = Graph.new |> Graph.add_vertex(:a, [:mylabel, :other])
      ...> Graph.vertex_labels(g, :a)
      [:mylabel, :other]
  """
  @spec add_vertex(t, vertex, label) :: t
  def add_vertex(g, v, labels \\ [])

  def add_vertex(
        %__MODULE__{vertices: vs, vertex_labels: vl, vertex_identifier: vertex_identifier} = g,
        v,
        labels
      )
      when is_list(labels) do
    id = vertex_identifier.(v)

    case Map.get(vs, id) do
      nil ->
        %__MODULE__{g | vertices: Map.put(vs, id, v), vertex_labels: Map.put(vl, id, labels)}

      _ ->
        g
    end
  end

  def add_vertex(g, v, label) do
    add_vertex(g, v, [label])
  end

  @doc """
  Like `add_vertex/2`, but takes a list of vertices to add to the graph.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :a])
      ...> Graph.vertices(g)
      [:a, :b]
  """
  @spec add_vertices(t, [vertex]) :: t
  def add_vertices(%__MODULE__{} = g, vs) when is_list(vs) do
    Enum.reduce(vs, g, &add_vertex(&2, &1))
  end

  @doc """
  Updates the labels for the given vertex.

  If no such vertex exists in the graph, `{:error, {:invalid_vertex, v}}` is returned.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a, :foo)
      ...> [:foo] = Graph.vertex_labels(g, :a)
      ...> g = Graph.label_vertex(g, :a, :bar)
      ...> Graph.vertex_labels(g, :a)
      [:foo, :bar]

      iex> g = Graph.new |> Graph.add_vertex(:a)
      ...> g = Graph.label_vertex(g, :a, [:foo, :bar])
      ...> Graph.vertex_labels(g, :a)
      [:foo, :bar]
  """
  @spec label_vertex(t, vertex, term) :: t | {:error, {:invalid_vertex, vertex}}
  def label_vertex(
        %__MODULE__{vertices: vs, vertex_labels: labels, vertex_identifier: vertex_identifier} =
          g,
        v,
        vlabels
      )
      when is_list(vlabels) do
    with v_id <- vertex_identifier.(v),
         true <- Map.has_key?(vs, v_id),
         old_vlabels <- Map.get(labels, v_id),
         new_vlabels <- old_vlabels ++ vlabels,
         labels <- Map.put(labels, v_id, new_vlabels) do
      %__MODULE__{g | vertex_labels: labels}
    else
      _ -> {:error, {:invalid_vertex, v}}
    end
  end

  def label_vertex(g, v, vlabel) do
    label_vertex(g, v, [vlabel])
  end

  @doc """
    iex> graph = Graph.new |> Graph.add_vertex(:a, [:foo, :bar])
    ...> [:foo, :bar] = Graph.vertex_labels(graph, :a)
    ...> graph = Graph.remove_vertex_labels(graph, :a)
    ...> Graph.vertex_labels(graph, :a)
    []

    iex> graph = Graph.new |> Graph.add_vertex(:a, [:foo, :bar])
    ...> [:foo, :bar] = Graph.vertex_labels(graph, :a)
    ...> Graph.remove_vertex_labels(graph, :b)
    {:error, {:invalid_vertex, :b}}
  """
  @spec remove_vertex_labels(t, vertex) :: t | {:error, {:invalid_vertex, vertex}}
  def remove_vertex_labels(
        %__MODULE__{
          vertices: vertices,
          vertex_labels: vertex_labels,
          vertex_identifier: vertex_identifier
        } = graph,
        vertex
      ) do
    graph.vertex_labels
    |> Map.put(vertex, [])

    with vertex_id <- vertex_identifier.(vertex),
         true <- Map.has_key?(vertices, vertex_id),
         labels <- Map.put(vertex_labels, vertex_id, []) do
      %__MODULE__{graph | vertex_labels: labels}
    else
      _ -> {:error, {:invalid_vertex, vertex}}
    end
  end

  @doc """
  Replaces `vertex` with `new_vertex` in the graph.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:b, :c}, {:c, :a}, {:c, :d}])
      ...> [:a, :b, :c, :d] = Graph.vertices(g)
      ...> g = Graph.replace_vertex(g, :a, :e)
      ...> [:b, :c, :d, :e] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :b, v2: :c}, %Graph.Edge{v1: :c, v2: :d}, %Graph.Edge{v1: :c, v2: :e}, %Graph.Edge{v1: :e, v2: :b}]
  """
  @spec replace_vertex(t, vertex, vertex) :: t | {:error, :no_such_vertex}
  def replace_vertex(
        %__MODULE__{out_edges: oe, in_edges: ie, edges: em, vertex_identifier: vertex_identifier} =
          g,
        v,
        rv
      ) do
    vs = g.vertices
    labels = g.vertex_labels

    with v_id <- vertex_identifier.(v),
         true <- Map.has_key?(vs, v_id),
         rv_id <- vertex_identifier.(rv),
         vs <- Map.put(Map.delete(vs, v_id), rv_id, rv) do
      oe =
        for {from_id, to} = e <- oe, into: %{} do
          fid = if from_id == v_id, do: rv_id, else: from_id

          cond do
            MapSet.member?(to, v_id) ->
              {fid, MapSet.put(MapSet.delete(to, v_id), rv_id)}

            from_id != fid ->
              {fid, to}

            :else ->
              e
          end
        end

      ie =
        for {to_id, from} = e <- ie, into: %{} do
          tid = if to_id == v_id, do: rv_id, else: to_id

          cond do
            MapSet.member?(from, v_id) ->
              {tid, MapSet.put(MapSet.delete(from, v_id), rv_id)}

            to_id != tid ->
              {tid, from}

            :else ->
              e
          end
        end

      meta =
        em
        |> Stream.map(fn
          {{^v_id, ^v_id}, meta} -> {{rv_id, rv_id}, meta}
          {{^v_id, v2_id}, meta} -> {{rv_id, v2_id}, meta}
          {{v1_id, ^v_id}, meta} -> {{v1_id, rv_id}, meta}
          edge -> edge
        end)
        |> Enum.into(%{})

      labels =
        case Map.get(labels, v_id) do
          nil -> labels
          label -> Map.put(Map.delete(labels, v_id), rv_id, label)
        end

      %__MODULE__{
        g
        | vertices: vs,
          out_edges: oe,
          in_edges: ie,
          edges: meta,
          vertex_labels: labels
      }
    else
      _ -> {:error, :no_such_vertex}
    end
  end

  @doc """
  Removes a vertex from the graph, as well as any edges which refer to that vertex. If the vertex does
  not exist in the graph, it is a no-op.

  ## Example

      iex> g = Graph.new |> Graph.add_vertex(:a) |> Graph.add_vertex(:b) |> Graph.add_edge(:a, :b)
      ...> [:a, :b] = Graph.vertices(g)
      ...> [%Graph.Edge{v1: :a, v2: :b}] = Graph.edges(g)
      ...> g = Graph.delete_vertex(g, :b)
      ...> [:a] = Graph.vertices(g)
      ...> Graph.edges(g)
      []
  """
  @spec delete_vertex(t, vertex) :: t
  def delete_vertex(
        %__MODULE__{out_edges: oe, in_edges: ie, edges: em, vertex_identifier: vertex_identifier} =
          g,
        v
      ) do
    vs = g.vertices
    ls = g.vertex_labels

    with v_id <- vertex_identifier.(v),
         true <- Map.has_key?(vs, v_id),
         oe <- Map.delete(oe, v_id),
         ie <- Map.delete(ie, v_id),
         vs <- Map.delete(vs, v_id),
         ls <- Map.delete(ls, v_id) do
      oe = for {id, ns} <- oe, do: {id, MapSet.delete(ns, v_id)}, into: %{}
      ie = for {id, ns} <- ie, do: {id, MapSet.delete(ns, v_id)}, into: %{}
      em = for {{id1, id2}, _} = e <- em, v_id != id1 && v_id != id2, do: e, into: %{}
      %__MODULE__{g | vertices: vs, vertex_labels: ls, out_edges: oe, in_edges: ie, edges: em}
    else
      _ -> g
    end
  end

  @doc """
  Like `delete_vertex/2`, but takes a list of vertices to delete from the graph.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.delete_vertices([:a, :b])
      ...> Graph.vertices(g)
      [:c]
  """
  @spec delete_vertices(t, [vertex]) :: t
  def delete_vertices(%__MODULE__{} = g, vs) when is_list(vs) do
    Enum.reduce(vs, g, &delete_vertex(&2, &1))
  end

  @doc """
  Like `add_edge/3` or `add_edge/4`, but takes a `Graph.Edge` struct created with
  `Graph.Edge.new/2` or `Graph.Edge.new/3`.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(Graph.Edge.new(:a, :b))
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b}]
  """
  @spec add_edge(t, Edge.t()) :: t
  def add_edge(%__MODULE__{} = g, %Edge{v1: v1, v2: v2, label: label, weight: weight}) do
    add_edge(g, v1, v2, label: label, weight: weight)
  end

  @doc """
  Adds an edge connecting `v1` to `v2`. If either `v1` or `v2` do not exist in the graph,
  they are automatically added. Adding the same edge more than once does not create multiple edges,
  each edge is only ever stored once.

  Edges have a default weight of 1, and an empty (nil) label. You can change this by passing options
  to this function, as shown below.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(:a, :b)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: nil, weight: 1}]

      iex> g = Graph.new |> Graph.add_edge(:a, :b, label: :foo, weight: 2)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo, weight: 2}]
  """
  @spec add_edge(t, vertex, vertex) :: t
  @spec add_edge(t, vertex, vertex, Edge.edge_opts()) :: t | no_return
  def add_edge(g, v1, v2, opts \\ [])

  def add_edge(%__MODULE__{type: :undirected} = g, v1, v2, opts) when is_list(opts) do
    if v1 > v2 do
      do_add_edge(g, v2, v1, opts)
    else
      do_add_edge(g, v1, v2, opts)
    end
  end

  def add_edge(%__MODULE__{} = g, v1, v2, opts) when is_list(opts) do
    do_add_edge(g, v1, v2, opts)
  end

  defp do_add_edge(%__MODULE__{vertex_identifier: vertex_identifier} = g, v1, v2, opts) do
    v1_id = vertex_identifier.(v1)
    v2_id = vertex_identifier.(v2)

    %__MODULE__{in_edges: ie, out_edges: oe, edges: meta} =
      g = g |> add_vertex(v1) |> add_vertex(v2)

    out_neighbors =
      case Map.get(oe, v1_id) do
        nil -> MapSet.new([v2_id])
        ms -> MapSet.put(ms, v2_id)
      end

    in_neighbors =
      case Map.get(ie, v2_id) do
        nil -> MapSet.new([v1_id])
        ms -> MapSet.put(ms, v1_id)
      end

    edge_meta = Map.get(meta, {v1_id, v2_id}, %{})
    {label, weight} = Edge.options_to_meta(opts)
    edge_meta = Map.put(edge_meta, label, weight)

    %__MODULE__{
      g
      | in_edges: Map.put(ie, v2_id, in_neighbors),
        out_edges: Map.put(oe, v1_id, out_neighbors),
        edges: Map.put(meta, {v1_id, v2_id}, edge_meta)
    }
  end

  @doc """
  This function is like `add_edge/3`, but for multiple edges at once, it also accepts edge specifications
  in a few different ways to make it easy to generate graphs succinctly.

  Edges must be provided as a list of `Edge` structs, `{vertex, vertex}` pairs, or
  `{vertex, vertex, edge_opts :: [label: term, weight: integer]}`.

  See the docs for `Graph.Edge.new/2` or `Graph.Edge.new/3` for more info on creating Edge structs, and
  `add_edge/3` for information on edge options.

  If an invalid edge specification is provided, raises `Graph.EdgeSpecificationError`.

  ## Examples

      iex> alias Graph.Edge
      ...> edges = [Edge.new(:a, :b), Edge.new(:b, :c, weight: 2)]
      ...> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edges(edges)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b}, %Graph.Edge{v1: :b, v2: :c, weight: 2}]

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:a, :b, label: :foo, weight: 2}])
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo, weight: 2}, %Graph.Edge{v1: :a, v2: :b}]

      iex> Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edges([:a, :b])
      ** (Graph.EdgeSpecificationError) Expected a valid edge specification, but got: :a
  """
  @spec add_edges(t, [Edge.t()] | Enumerable.t()) :: t | no_return
  def add_edges(%__MODULE__{} = g, es) do
    Enum.reduce(es, g, fn
      %Edge{} = edge, acc ->
        add_edge(acc, edge)

      {v1, v2}, acc ->
        add_edge(acc, v1, v2)

      {v1, v2, opts}, acc when is_list(opts) ->
        add_edge(acc, v1, v2, opts)

      bad_edge, _acc ->
        raise Graph.EdgeSpecificationError, bad_edge
    end)
  end

  @doc """
  Splits the edges between `v1` and `v2` by inserting a new vertex, `v3`, deleting
  the edges between `v1` and `v2`, and inserting new edges from `v1` to `v3` and from
  `v3` to `v2`.

  The resulting edges from the split will share the same weight and label as the old edges.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :c]) |> Graph.add_edge(:a, :c, weight: 2)
      ...> g = Graph.split_edge(g, :a, :c, :b)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, weight: 2}, %Graph.Edge{v1: :b, v2: :c, weight: 2}]

      iex> g = Graph.new(type: :undirected) |> Graph.add_vertices([:a, :c]) |> Graph.add_edge(:a, :c, weight: 2)
      ...> g = Graph.split_edge(g, :a, :c, :b)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, weight: 2}, %Graph.Edge{v1: :b, v2: :c, weight: 2}]
  """
  @spec split_edge(t, vertex, vertex, vertex) :: t | {:error, :no_such_edge}
  def split_edge(%__MODULE__{type: :undirected} = g, v1, v2, v3) do
    if v1 > v2 do
      do_split_edge(g, v2, v1, v3)
    else
      do_split_edge(g, v1, v2, v3)
    end
  end

  def split_edge(%__MODULE__{} = g, v1, v2, v3) do
    do_split_edge(g, v1, v2, v3)
  end

  defp do_split_edge(
         %__MODULE__{in_edges: ie, out_edges: oe, edges: em, vertex_identifier: vertex_identifier} =
           g,
         v1,
         v2,
         v3
       ) do
    with v1_id <- vertex_identifier.(v1),
         v2_id <- vertex_identifier.(v2),
         {:ok, v1_out} <- Map.fetch(oe, v1_id),
         {:ok, v2_in} <- Map.fetch(ie, v2_id),
         true <- MapSet.member?(v1_out, v2_id),
         meta <- Map.get(em, {v1_id, v2_id}),
         v1_out <- MapSet.delete(v1_out, v2_id),
         v2_in <- MapSet.delete(v2_in, v1_id) do
      g = %__MODULE__{
        g
        | in_edges: Map.put(ie, v2_id, v2_in),
          out_edges: Map.put(oe, v1_id, v1_out)
      }

      g = add_vertex(g, v3)

      Enum.reduce(meta, g, fn {label, weight}, acc ->
        acc
        |> add_edge(v1, v3, label: label, weight: weight)
        |> add_edge(v3, v2, label: label, weight: weight)
      end)
    else
      _ -> {:error, :no_such_edge}
    end
  end

  @doc """
  Given two vertices, this function updates the metadata (weight/label) for the unlabelled
  edge between those two vertices. If no unlabelled edge exists between them, an error
  tuple is returned. If you set a label, the unlabelled edge will be replaced with a new labelled
  edge.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(:a, :b) |> Graph.add_edge(:a, :b, label: :bar)
      ...> %Graph{} = g = Graph.update_edge(g, :a, :b, weight: 2, label: :foo)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :bar}, %Graph.Edge{v1: :a, v2: :b, label: :foo, weight: 2}]
  """
  @spec update_edge(t, vertex, vertex, Edge.edge_opts()) :: t | {:error, :no_such_edge}
  def update_edge(%__MODULE__{} = g, v1, v2, opts) when is_list(opts) do
    update_labelled_edge(g, v1, v2, nil, opts)
  end

  @doc """
  Like `update_edge/4`, but requires you to specify the labelled edge to update.

  Th implementation of `update_edge/4` is actually `update_edge(g, v1, v2, nil, opts)`.

  ## Example

      iex> g = Graph.new |> Graph.add_edge(:a, :b) |> Graph.add_edge(:a, :b, label: :bar)
      ...> %Graph{} = g = Graph.update_labelled_edge(g, :a, :b, :bar, weight: 2, label: :foo)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo, weight: 2}, %Graph.Edge{v1: :a, v2: :b}]

      iex> g = Graph.new(type: :undirected) |> Graph.add_edge(:a, :b) |> Graph.add_edge(:a, :b, label: :bar)
      ...> %Graph{} = g = Graph.update_labelled_edge(g, :a, :b, :bar, weight: 2, label: :foo)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo, weight: 2}, %Graph.Edge{v1: :a, v2: :b}]
  """
  @spec update_labelled_edge(t, vertex, vertex, label, Edge.edge_opts()) ::
          t | {:error, :no_such_edge}
  def update_labelled_edge(%__MODULE__{type: :undirected} = g, v1, v2, old_label, opts)
      when is_list(opts) do
    if v1 > v2 do
      do_update_labelled_edge(g, v2, v1, old_label, opts)
    else
      do_update_labelled_edge(g, v1, v2, old_label, opts)
    end
  end

  def update_labelled_edge(%__MODULE__{} = g, v1, v2, old_label, opts) when is_list(opts) do
    do_update_labelled_edge(g, v1, v2, old_label, opts)
  end

  defp do_update_labelled_edge(
         %__MODULE__{edges: em, vertex_identifier: vertex_identifier} = g,
         v1,
         v2,
         old_label,
         opts
       ) do
    with v1_id <- vertex_identifier.(v1),
         v2_id <- vertex_identifier.(v2),
         edge_key <- {v1_id, v2_id},
         {:ok, meta} <- Map.fetch(em, edge_key),
         {:ok, _} <- Map.fetch(meta, old_label),
         {new_label, new_weight} <- Edge.options_to_meta(opts) do
      case new_label do
        ^old_label ->
          new_meta = Map.put(meta, old_label, new_weight)
          %__MODULE__{g | edges: Map.put(em, edge_key, new_meta)}

        nil ->
          new_meta = Map.put(meta, old_label, new_weight)
          %__MODULE__{g | edges: Map.put(em, edge_key, new_meta)}

        _ ->
          new_meta = Map.put(Map.delete(meta, old_label), new_label, new_weight)
          %__MODULE__{g | edges: Map.put(em, edge_key, new_meta)}
      end
    else
      _ ->
        {:error, :no_such_edge}
    end
  end

  @doc """
  Removes all edges connecting `v1` to `v2`, regardless of label.

  If no such edge exists, the graph is returned unmodified.

  ## Example

    iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}])
    ...> g = Graph.delete_edge(g, :a, :b)
    ...> [:a, :b] = Graph.vertices(g)
    ...> Graph.edges(g)
    []

    iex> g = Graph.new(type: :undirected) |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}])
    ...> g = Graph.delete_edge(g, :a, :b)
    ...> [:a, :b] = Graph.vertices(g)
    ...> Graph.edges(g)
    []
  """
  @spec delete_edge(t, vertex, vertex) :: t
  def delete_edge(%__MODULE__{type: :undirected} = g, v1, v2) do
    if v1 > v2 do
      do_delete_edge(g, v2, v1)
    else
      do_delete_edge(g, v1, v2)
    end
  end

  def delete_edge(%__MODULE__{} = g, v1, v2) do
    do_delete_edge(g, v1, v2)
  end

  defp do_delete_edge(
         %__MODULE__{
           in_edges: ie,
           out_edges: oe,
           edges: meta,
           vertex_identifier: vertex_identifier
         } = g,
         v1,
         v2
       ) do
    with v1_id <- vertex_identifier.(v1),
         v2_id <- vertex_identifier.(v2),
         edge_key <- {v1_id, v2_id},
         {:ok, v1_out} <- Map.fetch(oe, v1_id),
         {:ok, v2_in} <- Map.fetch(ie, v2_id) do
      v1_out = MapSet.delete(v1_out, v2_id)
      v2_in = MapSet.delete(v2_in, v1_id)
      meta = Map.delete(meta, edge_key)

      %__MODULE__{
        g
        | in_edges: Map.put(ie, v2_id, v2_in),
          out_edges: Map.put(oe, v1_id, v1_out),
          edges: meta
      }
    else
      _ -> g
    end
  end

  @doc """
  Removes an edge connecting `v1` to `v2`. A label can be specified to disambiguate the
  specific edge you wish to delete, if not provided, the unlabelled edge, if one exists,
  will be removed.

  If no such edge exists, the graph is returned unmodified.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}])
      ...> g = Graph.delete_edge(g, :a, :b, nil)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo}]

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}])
      ...> g = Graph.delete_edge(g, :a, :b, :foo)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: nil}]

      iex> g = Graph.new(type: :undirected) |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}])
      ...> g = Graph.delete_edge(g, :a, :b, :foo)
      ...> [:a, :b] = Graph.vertices(g)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: nil}]
  """
  @spec delete_edge(t, vertex, vertex, label) :: t
  def delete_edge(%__MODULE__{type: :undirected} = g, v1, v2, label) do
    if v1 > v2 do
      do_delete_edge(g, v2, v1, label)
    else
      do_delete_edge(g, v1, v2, label)
    end
  end

  def delete_edge(%__MODULE__{} = g, v1, v2, label) do
    do_delete_edge(g, v1, v2, label)
  end

  defp do_delete_edge(
         %__MODULE__{
           in_edges: ie,
           out_edges: oe,
           edges: meta,
           vertex_identifier: vertex_identifier
         } = g,
         v1,
         v2,
         label
       ) do
    with v1_id <- vertex_identifier.(v1),
         v2_id <- vertex_identifier.(v2),
         edge_key <- {v1_id, v2_id},
         {:ok, v1_out} <- Map.fetch(oe, v1_id),
         {:ok, v2_in} <- Map.fetch(ie, v2_id),
         {:ok, edge_meta} <- Map.fetch(meta, edge_key),
         {:ok, _} <- Map.fetch(edge_meta, label) do
      edge_meta = Map.delete(edge_meta, label)

      case map_size(edge_meta) do
        0 ->
          v1_out = MapSet.delete(v1_out, v2_id)
          v2_in = MapSet.delete(v2_in, v1_id)
          meta = Map.delete(meta, edge_key)

          %__MODULE__{
            g
            | in_edges: Map.put(ie, v2_id, v2_in),
              out_edges: Map.put(oe, v1_id, v1_out),
              edges: meta
          }

        _ ->
          meta = Map.put(meta, edge_key, edge_meta)
          %__MODULE__{g | edges: meta}
      end
    else
      _ -> g
    end
  end

  @doc """
  Like `delete_edge/3`, but takes a list of edge specifications, and deletes the corresponding
  edges from the graph, if they exist.

  Edge specifications can be `Edge` structs, `{vertex, vertex}` pairs, or `{vertex, vertex, label: label}`
  triplets. An invalid specification will cause `Graph.EdgeSpecificationError` to be raised.

  ## Examples

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b)
      ...> g = Graph.delete_edges(g, [{:a, :b}])
      ...> Graph.edges(g)
      []

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b, label: :foo)
      ...> g = Graph.delete_edges(g, [{:a, :b}])
      ...> Graph.edges(g)
      []

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b, label: :foo)
      ...> g = Graph.delete_edges(g, [{:a, :b, label: :bar}])
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo}]

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b, label: :foo)
      ...> g = Graph.delete_edges(g, [{:a, :b, label: :foo}])
      ...> Graph.edges(g)
      []

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b)
      ...> Graph.delete_edges(g, [:a])
      ** (Graph.EdgeSpecificationError) Expected a valid edge specification, but got: :a
  """
  @spec delete_edges(t, [{vertex, vertex}]) :: t | no_return
  def delete_edges(%__MODULE__{} = g, es) when is_list(es) do
    Enum.reduce(es, g, fn
      {v1, v2}, acc ->
        delete_edge(acc, v1, v2)

      {v1, v2, [{:label, label}]}, acc ->
        delete_edge(acc, v1, v2, label)

      %Edge{v1: v1, v2: v2, label: label}, acc ->
        delete_edge(acc, v1, v2, label)

      bad_edge, _acc ->
        raise EdgeSpecificationError, bad_edge
    end)
  end

  @doc """
  This function can be used to remove all edges between `v1` and `v2`. This is useful if
  you are defining multiple edges between vertices to represent different relationships, but
  want to remove them all as if they are a single unit.

  ## Examples

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:b, :a}])
      ...> g = Graph.delete_edges(g, :a, :b)
      ...> Graph.edges(g)
      [%Graph.Edge{v1: :b, v2: :a}]

      iex> g = Graph.new(type: :undirected) |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:b, :a}])
      ...> g = Graph.delete_edges(g, :a, :b)
      ...> Graph.edges(g)
      []
  """
  @spec delete_edges(t, vertex, vertex) :: t
  def delete_edges(%__MODULE__{type: :undirected} = g, v1, v2) do
    if v1 > v2 do
      do_delete_edges(g, v2, v1)
    else
      do_delete_edges(g, v1, v2)
    end
  end

  def delete_edges(%__MODULE__{} = g, v1, v2) do
    do_delete_edges(g, v1, v2)
  end

  defp do_delete_edges(
         %__MODULE__{
           in_edges: ie,
           out_edges: oe,
           edges: meta,
           vertex_identifier: vertex_identifier
         } = g,
         v1,
         v2
       ) do
    with v1_id <- vertex_identifier.(v1),
         v2_id <- vertex_identifier.(v2),
         edge_key <- {v1_id, v2_id},
         true <- Map.has_key?(meta, edge_key),
         v1_out <- Map.get(oe, v1_id),
         v2_in <- Map.get(ie, v2_id) do
      meta = Map.delete(meta, edge_key)
      v1_out = MapSet.delete(v1_out, v2_id)
      v2_in = MapSet.delete(v2_in, v1_id)

      %__MODULE__{
        g
        | out_edges: Map.put(oe, v1_id, v1_out),
          in_edges: Map.put(ie, v2_id, v2_in),
          edges: meta
      }
    else
      _ -> g
    end
  end

  @doc """
  The transposition of a graph is another graph with the direction of all the edges reversed.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b) |> Graph.add_edge(:b, :c)
      ...> g |> Graph.transpose |> Graph.edges
      [%Graph.Edge{v1: :b, v2: :a}, %Graph.Edge{v1: :c, v2: :b}]
  """
  @spec transpose(t) :: t
  def transpose(%__MODULE__{in_edges: ie, out_edges: oe, edges: meta} = g) do
    meta2 =
      meta
      |> Enum.reduce(%{}, fn {{v1, v2}, meta}, acc -> Map.put(acc, {v2, v1}, meta) end)

    %__MODULE__{g | in_edges: oe, out_edges: ie, edges: meta2}
  end

  @doc """
  Returns a topological ordering of the vertices of graph `g`, if such an ordering exists, otherwise it returns false.
  For each vertex in the returned list, no out-neighbors occur earlier in the list.

  Multiple edges between two vertices are considered a single edge for purposes of this sort.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:a, :c}, {:b, :c}, {:c, :d}])
      ...> Graph.topsort(g)
      [:a, :b, :c, :d]

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:a, :c}, {:b, :c}, {:c, :d}, {:c, :a}])
      ...> Graph.topsort(g)
      false
  """
  @spec topsort(t) :: [vertex] | false
  def topsort(%__MODULE__{type: :undirected}), do: false
  def topsort(%__MODULE__{} = g), do: Graph.Directed.topsort(g)

  @doc """
  Returns a list of connected components, where each component is a list of vertices.

  A *connected component* is a maximal subgraph such that there is a path between each pair of vertices,
  considering all edges undirected.

  A *subgraph* is a graph whose vertices and edges are a subset of the vertices and edges of the source graph.

  A *maximal subgraph* is a subgraph with property `P` where all other subgraphs which contain the same vertices
  do not have that same property `P`.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:a, :c}, {:b, :c}, {:c, :d}, {:c, :a}])
      ...> Graph.components(g)
      [[:d, :b, :c, :a]]
  """
  @spec components(t) :: [[vertex]]
  defdelegate components(g), to: Graph.Directed

  @doc """
  Returns a list of strongly connected components, where each component is a list of vertices.

  A *strongly connected component* is a maximal subgraph such that there is a path between each pair of vertices.

  See `components/1` for the definitions of *subgraph* and *maximal subgraph* if you are unfamiliar with the
  terminology.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:a, :c}, {:b, :c}, {:c, :d}, {:c, :a}])
      ...> Graph.strong_components(g)
      [[:d], [:b, :c, :a]]
  """
  @spec strong_components(t) :: [[vertex]]
  defdelegate strong_components(g), to: Graph.Directed

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path in the graph from some vertex of `vs` to `v`.

  As paths of length zero are allowed, the vertices of `vs` are also included in the returned list.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:a, :c}, {:b, :c}, {:c, :d}])
      ...> Graph.reachable(g, [:a])
      [:d, :c, :b, :a]
  """
  @spec reachable(t, [vertex]) :: [[vertex]]
  defdelegate reachable(g, vs), to: Graph.Directed

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path in the graph of length one or more from some vertex of `vs` to `v`.

  As a consequence, only those vertices of `vs` that are included in some cycle are returned.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:a, :c}, {:b, :c}, {:c, :d}])
      ...> Graph.reachable_neighbors(g, [:a])
      [:d, :c, :b]
  """
  @spec reachable_neighbors(t, [vertex]) :: [[vertex]]
  defdelegate reachable_neighbors(g, vs), to: Graph.Directed

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path from `v` to some vertex of `vs`.

  As paths of length zero are allowed, the vertices of `vs` are also included in the returned list.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:a, :c}, {:b, :c}, {:c, :d}])
      ...> Graph.reaching(g, [:d])
      [:b, :a, :c, :d]
  """
  @spec reaching(t, [vertex]) :: [[vertex]]
  defdelegate reaching(g, vs), to: Graph.Directed

  @doc """
  Returns an unsorted list of vertices from the graph, such that for each vertex in the list (call it `v`),
  there is a path of length one or more from `v` to some vertex of `vs`.

  As a consequence, only those vertices of `vs` that are included in some cycle are returned.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:a, :c}, {:b, :c}, {:c, :a}, {:b, :d}])
      ...> Graph.reaching_neighbors(g, [:b])
      [:b, :c, :a]
  """
  @spec reaching_neighbors(t, [vertex]) :: [[vertex]]
  defdelegate reaching_neighbors(g, vs), to: Graph.Directed

  @doc """
  Returns all vertices of graph `g`. The order is given by a depth-first traversal of the graph,
  collecting visited vertices in preorder.

  ## Example

  Our example code constructs a graph which looks like so:

           :a
             \
              :b
             /  \
           :c   :d
           /
         :e

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d, :e])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:b, :c}, {:b, :d}, {:c, :e}])
      ...> Graph.preorder(g)
      [:a, :b, :c, :e, :d]
  """
  @spec preorder(t) :: [vertex]
  defdelegate preorder(g), to: Graph.Directed

  @doc """
  Returns all vertices of graph `g`. The order is given by a depth-first traversal of the graph,
  collecting visited vertices in postorder. More precisely, the vertices visited while searching from an
  arbitrarily chosen vertex are collected in postorder, and all those collected vertices are placed before
  the subsequently visited vertices.

  ## Example

  Our example code constructs a graph which looks like so:

          :a
            \
             :b
            /  \
           :c   :d
          /
         :e

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c, :d, :e])
      ...> g = Graph.add_edges(g, [{:a, :b}, {:b, :c}, {:b, :d}, {:c, :e}])
      ...> Graph.postorder(g)
      [:e, :c, :d, :b, :a]
  """
  @spec postorder(t) :: [vertex]
  defdelegate postorder(g), to: Graph.Directed

  @doc """
  Returns a list of vertices from graph `g` which are included in a loop, where a loop is a cycle of length 1.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :a)
      ...> Graph.loop_vertices(g)
      [:a]
  """
  @spec loop_vertices(t) :: [vertex]
  defdelegate loop_vertices(g), to: Graph.Directed

  @doc """
  Detects all maximal cliques in the provided graph.

  Returns a list of cliques, where each clique is a list of vertices in the clique.

  A clique is a subset `vs` of the vertices in the given graph, which together form a complete graph;
  or put another way, every vertex in `vs` is connected to all other vertices in `vs`.
  """
  @spec cliques(t) :: [[vertex]]
  def cliques(%__MODULE__{type: :directed}) do
    raise "cliques/1 can not be called on a directed graph"
  end

  def cliques(%__MODULE__{vertex_identifier: vertex_identifier} = g) do
    # We do vertex ordering as described in Bron-Kerbosch
    # to improve the worst-case performance of the algorithm
    p =
      g
      |> k_core_components()
      |> Enum.sort_by(fn {k, _} -> k end, fn a, b -> a >= b end)
      |> Stream.flat_map(fn {_, vs} -> vs end)
      |> Enum.map(&vertex_identifier.(&1))

    g
    |> detect_cliques(_r = [], p, _x = [], _acc = [])
    |> Enum.reverse()
  end

  @doc """
  Detects all maximal cliques of degree `k`.

  Returns a list of cliques, where each clique is a list of vertices in the clique.
  """
  @spec k_cliques(t, non_neg_integer) :: [[vertex]]
  def k_cliques(%__MODULE__{type: :directed}, _k) do
    raise "k_cliques/2 can not be called on a directed graph"
  end

  def k_cliques(%__MODULE__{} = g, k) when is_integer(k) and k >= 0 do
    g
    |> cliques()
    |> Enum.filter(fn clique -> length(clique) == k end)
  end

  # r is a maximal clique
  defp detect_cliques(%__MODULE__{vertices: vs}, r, [], [], acc) do
    mapped =
      r
      |> Stream.map(&Map.get(vs, &1))
      |> Enum.reverse()

    [mapped | acc]
  end

  # r is a subset of another clique
  defp detect_cliques(_g, _r, [], _x, acc), do: acc

  defp detect_cliques(%__MODULE__{in_edges: ie, out_edges: oe} = g, r, [pivot | p], x, acc) do
    n = MapSet.union(Map.get(ie, pivot, MapSet.new()), Map.get(oe, pivot, MapSet.new()))
    p2 = Enum.filter(p, &Enum.member?(n, &1))
    x2 = Enum.filter(x, &Enum.member?(n, &1))
    acc2 = detect_cliques(g, [pivot | r], p2, x2, acc)
    detect_cliques(g, r, p, [pivot | x], acc2)
  end

  @doc """
  Calculates the k-core for a given graph and value of `k`.

  A k-core of the graph is a maximal subgraph of `g` which contains vertices of which all
  have a degree of at least `k`. This function returns a new `Graph` which is a subgraph
  of `g` containing all vertices which have a coreness >= the desired value of `k`.

  If there is no k-core in the graph for the provided value of `k`, an empty `Graph` is returned.

  If a negative integer is provided for `k`, a RuntimeError will be raised.

  NOTE: For performance reasons, k-core calculations make use of ETS. If you are
  sensitive to the number of concurrent ETS tables running in your system, you should
  be aware of it's usage here. 2 tables are used, and they are automatically cleaned
  up when this function returns.
  """
  @spec k_core(t, k :: non_neg_integer) :: t
  def k_core(%__MODULE__{} = g, k) when is_integer(k) and k >= 0 do
    vs =
      g
      |> decompose_cores()
      |> Stream.filter(fn {_, vk} -> vk >= k end)
      |> Enum.map(fn {v, _k} -> v end)

    Graph.subgraph(g, vs)
  end

  def k_core(%__MODULE__{}, k) do
    raise "`k` must be a positive number, got `#{inspect(k)}`"
  end

  @doc """
  Groups all vertices by their k-coreness into a single map.

  More commonly you will want a specific k-core, in particular the degeneracy core,
  for which there are other functions in the API you can use. However if you have
  a need to determine which k-core each vertex belongs to, this function can be used
  to do just that.

  As an example, you can construct the k-core for a given graph like so:

      k_core_vertices =
        g
        |> Graph.k_core_components()
        |> Stream.filter(fn {k, _} -> k >= desired_k end)
        |> Enum.flat_map(fn {_, vs} -> vs end)
      Graph.subgraph(g, k_core_vertices)
  """
  @spec k_core_components(t) :: %{(k :: non_neg_integer) => [vertex]}
  def k_core_components(%__MODULE__{} = g) do
    res =
      g
      |> decompose_cores()
      |> Enum.group_by(fn {_, k} -> k end, fn {v, _} -> v end)

    if map_size(res) > 0 do
      res
    else
      %{0 => []}
    end
  end

  @doc """
  Determines the k-degeneracy of the given graph.

  The degeneracy of graph `g` is the maximum value of `k` for which a k-core
  exists in graph `g`.
  """
  @spec degeneracy(t) :: non_neg_integer
  def degeneracy(%__MODULE__{} = g) do
    {_, k} =
      g
      |> decompose_cores()
      |> Enum.max_by(fn {_, k} -> k end, fn -> {nil, 0} end)

    k
  end

  @doc """
  Calculates the degeneracy core of a given graph.

  The degeneracy core of a graph is the k-core of the graph where the
  value of `k` is the degeneracy of the graph. The degeneracy of a graph
  is the highest value of `k` which has a non-empty k-core in the graph.
  """
  @spec degeneracy_core(t) :: t
  def degeneracy_core(%__MODULE__{} = g) do
    {_, core} =
      g
      |> decompose_cores()
      |> Enum.group_by(fn {_, k} -> k end, fn {v, _} -> v end)
      |> Enum.max_by(fn {k, _} -> k end, fn -> {0, []} end)

    Graph.subgraph(g, core)
  end

  @doc """
  Calculates the k-coreness of vertex `v` in graph `g`.

  The k-coreness of a vertex is defined as the maximum value of `k`
  for which `v` is found in the corresponding k-core of graph `g`.

  NOTE: This function decomposes all k-core components to determine the coreness
  of a vertex - if you will be trying to determine the coreness of many vertices,
  it is recommended to use `k_core_components/1` and then lookup the coreness of a vertex
  by querying the resulting map.
  """
  @spec coreness(t, vertex) :: non_neg_integer
  def coreness(%__MODULE__{} = g, v) do
    res =
      g
      |> decompose_cores()
      |> Enum.find(fn
        {^v, _} -> true
        _ -> false
      end)

    case res do
      {_, k} -> k
      _ -> 0
    end
  end

  # This produces a list of {v, k} where k is the largest k-core this vertex belongs to
  defp decompose_cores(%__MODULE__{vertices: vs} = g) do
    # Rules to remember
    # - a k-core of a graph is a subgraph where each vertex has at least `k` neighbors in the subgraph
    # - A k-core is not necessarily connected.
    # - The core number for each vertex is the highest k-core it is a member of
    # - A vertex in a k-core will be, by definition, in a (k-1)-core (cores are nested)
    degrees = :ets.new(:k_cores, [:set, keypos: 1])
    l = :ets.new(:k_cores_l, [:set, keypos: 1])

    try do
      # Since we are making many modifications to the graph as we work on it,
      # it is more performant to store the list of vertices and their degree in ETS
      # and work on it there. This is not strictly necessary, but makes the algorithm
      # easier to read and is faster, so unless there is good reason to avoid ETS here
      # I think it's a fair compromise.
      for {_id, v} <- vs do
        :ets.insert(degrees, {v, out_degree(g, v)})
      end

      decompose_cores(degrees, l, g, 1)
    after
      :ets.delete(degrees)
      :ets.delete(l)
    end
  end

  defp decompose_cores(degrees, l, g, k) do
    case :ets.info(degrees, :size) do
      0 ->
        Enum.reverse(:ets.tab2list(l))

      _ ->
        # Select all v that have a degree less than `k`
        case :ets.select(degrees, [{{:"$1", :"$2"}, [{:<, :"$2", k}], [:"$1"]}]) do
          [] ->
            decompose_cores(degrees, l, g, k + 1)

          matches ->
            for v <- matches do
              :ets.delete(degrees, v)

              for neighbor <- out_neighbors(g, v),
                  not :ets.member(l, neighbor) and v != neighbor do
                :ets.update_counter(degrees, neighbor, {2, -1})
              end

              :ets.insert(l, {v, k - 1})
            end

            decompose_cores(degrees, l, g, k)
        end
    end
  end

  @doc """
  Returns the degree of vertex `v` of graph `g`.

  The degree of a vertex is the total number of edges containing that vertex.

  For directed graphs this is the same as the sum of the in-degree and out-degree
  of the given vertex. For undirected graphs, the in-degree and out-degree are always
  the same.

  ## Example

      iex> g = Graph.new(type: :undirected) |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b)
      ...> Graph.degree(g, :b)
      1

      iex> g = Graph.new() |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b)
      ...> Graph.degree(g, :b)
      1
  """
  @spec degree(t, vertex) :: non_neg_integer
  def degree(%__MODULE__{type: :undirected} = g, v) do
    in_degree(g, v)
  end

  def degree(%__MODULE__{} = g, v) do
    in_degree(g, v) + out_degree(g, v)
  end

  @doc """
  Returns the in-degree of vertex `v` of graph `g`.

  The *in-degree* of a vertex is the number of edges directed inbound towards that vertex.

  For undirected graphs, the in-degree and out-degree are always the same - the sum total
  of all edges inbound or outbound from the vertex.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b)
      ...> Graph.in_degree(g, :b)
      1
  """
  def in_degree(
        %__MODULE__{
          type: :undirected,
          in_edges: ie,
          out_edges: oe,
          edges: meta,
          vertex_identifier: vertex_identifier
        },
        v
      ) do
    v_id = vertex_identifier.(v)
    v_in = Map.get(ie, v_id, MapSet.new())
    v_out = Map.get(oe, v_id, MapSet.new())
    v_all = MapSet.union(v_in, v_out)

    Enum.reduce(v_all, 0, fn v1_id, sum ->
      case Map.fetch(meta, {v1_id, v_id}) do
        {:ok, edge_meta} ->
          sum + map_size(edge_meta)

        _ ->
          case Map.fetch(meta, {v_id, v1_id}) do
            {:ok, edge_meta} -> sum + map_size(edge_meta)
            _ -> sum
          end
      end
    end)
  end

  def in_degree(%__MODULE__{in_edges: ie, edges: meta, vertex_identifier: vertex_identifier}, v) do
    with v_id <- vertex_identifier.(v),
         {:ok, v_in} <- Map.fetch(ie, v_id) do
      Enum.reduce(v_in, 0, fn v1_id, sum ->
        sum + map_size(Map.get(meta, {v1_id, v_id}))
      end)
    else
      _ -> 0
    end
  end

  @doc """
  Returns the out-degree of vertex `v` of graph `g`.

  The *out-degree* of a vertex is the number of edges directed outbound from that vertex.

  For undirected graphs, the in-degree and out-degree are always the same - the sum total
  of all edges inbound or outbound from the vertex.

  ## Example

      iex> g = Graph.new |> Graph.add_vertices([:a, :b, :c]) |> Graph.add_edge(:a, :b)
      ...> Graph.out_degree(g, :a)
      1
  """
  @spec out_degree(t, vertex) :: non_neg_integer
  def out_degree(%__MODULE__{type: :undirected} = g, v) do
    # Take advantage of the fact that in_degree and out_degree
    # are the same for undirected graphs
    in_degree(g, v)
  end

  def out_degree(%__MODULE__{out_edges: oe, edges: meta, vertex_identifier: vertex_identifier}, v) do
    with v_id <- vertex_identifier.(v),
         {:ok, v_out} <- Map.fetch(oe, v_id) do
      Enum.reduce(v_out, 0, fn v2_id, sum ->
        sum + map_size(Map.get(meta, {v_id, v2_id}))
      end)
    else
      _ -> 0
    end
  end

  @doc """
  Return all neighboring vertices of the given vertex.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:b, :a}, {:b, :c}, {:c, :a}])
      ...> Graph.neighbors(g, :a)
      [:b, :c]

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:b, :a}, {:b, :c}, {:c, :a}])
      ...> Graph.neighbors(g, :d)
      []
  """
  @spec neighbors(t, vertex) :: [vertex]
  def neighbors(
        %__MODULE__{
          in_edges: ie,
          out_edges: oe,
          vertices: vs,
          vertex_identifier: vertex_identifier
        },
        v
      ) do
    v_id = vertex_identifier.(v)
    v_in = Map.get(ie, v_id, MapSet.new())
    v_out = Map.get(oe, v_id, MapSet.new())
    v_all = MapSet.union(v_in, v_out)
    Enum.map(v_all, &Map.get(vs, &1))
  end

  @doc """
  Returns a list of vertices which all have edges coming in to the given vertex `v`.

  In the case of undirected graphs, it delegates to `neighbors/2`.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:b, :c}])
      ...> Graph.in_neighbors(g, :b)
      [:a]
  """
  @spec in_neighbors(t, vertex) :: [vertex]
  def in_neighbors(%__MODULE__{type: :undirected} = g, v) do
    neighbors(g, v)
  end

  def in_neighbors(
        %__MODULE__{in_edges: ie, vertices: vs, vertex_identifier: vertex_identifier},
        v
      ) do
    with v_id <- vertex_identifier.(v),
         {:ok, v_in} <- Map.fetch(ie, v_id) do
      Enum.map(v_in, &Map.get(vs, &1))
    else
      _ -> []
    end
  end

  @doc """
  Returns a list of `Graph.Edge` structs representing the in edges to vertex `v`.

  In the case of undirected graphs, it delegates to `edges/2`.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:b, :c}])
      ...> Graph.in_edges(g, :b)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo}, %Graph.Edge{v1: :a, v2: :b}]
  """
  @spec in_edges(t, vertex) :: Edge.t()
  def in_edges(%__MODULE__{type: :undirected} = g, v) do
    edges(g, v)
  end

  def in_edges(
        %__MODULE__{
          vertices: vs,
          in_edges: ie,
          edges: meta,
          vertex_identifier: vertex_identifier
        },
        v
      ) do
    with v_id <- vertex_identifier.(v),
         {:ok, v_in} <- Map.fetch(ie, v_id) do
      Enum.flat_map(v_in, fn v1_id ->
        v1 = Map.get(vs, v1_id)

        Enum.map(Map.get(meta, {v1_id, v_id}), fn {label, weight} ->
          Edge.new(v1, v, label: label, weight: weight)
        end)
      end)
    else
      _ -> []
    end
  end

  @doc """
  Returns a list of vertices which the given vertex `v` has edges going to.

  In the case of undirected graphs, it delegates to `neighbors/2`.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:b, :c}])
      ...> Graph.out_neighbors(g, :a)
      [:b]
  """
  @spec out_neighbors(t, vertex) :: [vertex]
  def out_neighbors(%__MODULE__{type: :undirected} = g, v) do
    neighbors(g, v)
  end

  def out_neighbors(
        %__MODULE__{vertices: vs, out_edges: oe, vertex_identifier: vertex_identifier},
        v
      ) do
    with v_id <- vertex_identifier.(v),
         {:ok, v_out} <- Map.fetch(oe, v_id) do
      Enum.map(v_out, &Map.get(vs, &1))
    else
      _ -> []
    end
  end

  @doc """
  Returns a list of `Graph.Edge` structs representing the out edges from vertex `v`.

  In the case of undirected graphs, it delegates to `edges/2`.

  ## Example

      iex> g = Graph.new |> Graph.add_edges([{:a, :b}, {:a, :b, label: :foo}, {:b, :c}])
      ...> Graph.out_edges(g, :a)
      [%Graph.Edge{v1: :a, v2: :b, label: :foo}, %Graph.Edge{v1: :a, v2: :b}]
  """
  @spec out_edges(t, vertex) :: Edge.t()
  def out_edges(%__MODULE__{type: :undirected} = g, v) do
    edges(g, v)
  end

  def out_edges(
        %__MODULE__{
          vertices: vs,
          out_edges: oe,
          edges: meta,
          vertex_identifier: vertex_identifier
        },
        v
      ) do
    with v_id <- vertex_identifier.(v),
         {:ok, v_out} <- Map.fetch(oe, v_id) do
      Enum.flat_map(v_out, fn v2_id ->
        v2 = Map.get(vs, v2_id)

        Enum.map(Map.get(meta, {v_id, v2_id}), fn {label, weight} ->
          Edge.new(v, v2, label: label, weight: weight)
        end)
      end)
    else
      _ ->
        []
    end
  end

  @doc """
  Builds a maximal subgraph of `g` which includes all of the vertices in `vs` and the edges which connect them.

  See the test suite for example usage.
  """
  @spec subgraph(t, [vertex]) :: t
  def subgraph(
        %__MODULE__{
          type: type,
          vertices: vertices,
          out_edges: oe,
          edges: meta,
          vertex_identifier: vertex_identifier
        } = graph,
        vs
      ) do
    allowed =
      vs
      |> Enum.map(&vertex_identifier.(&1))
      |> Enum.filter(&Map.has_key?(vertices, &1))
      |> MapSet.new()

    Enum.reduce(allowed, Graph.new(type: type), fn v_id, sg ->
      v = Map.get(vertices, v_id)

      sg =
        sg
        |> Graph.add_vertex(v)
        |> Graph.label_vertex(v, Graph.vertex_labels(graph, v))

      oe
      |> Map.get(v_id, MapSet.new())
      |> MapSet.intersection(allowed)
      |> Enum.reduce(sg, fn v2_id, sg ->
        v2 = Map.get(vertices, v2_id)

        Enum.reduce(Map.get(meta, {v_id, v2_id}), sg, fn {label, weight}, sg ->
          Graph.add_edge(sg, v, v2, label: label, weight: weight)
        end)
      end)
    end)
  end
end
