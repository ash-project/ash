defmodule Ash.Query.Aggregate do
  @moduledoc "Represents an aggregated association value"
  defstruct [
    :name,
    :relationship_path,
    :default_value,
    :resource,
    :query,
    :field,
    :kind,
    :type,
    :constraints,
    :implementation,
    :load,
    :read_action,
    :agg_name,
    join_filters: %{},
    context: %{},
    authorize?: true,
    include_nil?: false,
    uniq?: false,
    filterable?: true,
    sortable?: true,
    sensitive?: false
  ]

  @type t :: %__MODULE__{}

  @kinds [:count, :first, :sum, :list, :max, :min, :avg, :exists, :custom]
  @type kind :: unquote(Enum.reduce(@kinds, &{:|, [], [&1, &2]}))

  alias Ash.Error.Query.{NoReadAction, NoSuchRelationship}

  require Ash.Query

  @doc false
  def kinds, do: @kinds

  def new!(resource, name, kind, opts \\ []) do
    case new(resource, name, kind, opts) do
      {:ok, aggregate} ->
        aggregate

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  @doc false
  def subpaths([]), do: []

  def subpaths([first | rest]) do
    [[first] | Enum.map(subpaths(rest), &[first | &1])]
  end

  @schema [
    path: [
      type: {:list, :atom},
      doc: "The relationship path to aggregate over. Only used when adding aggregates to a query."
    ],
    query: [
      type: :any,
      doc:
        "A base query to use for the aggregate, or a keyword list to be passed to `Ash.Query.build/2`"
    ],
    field: [
      type: :atom,
      doc: "The field to use for the aggregate. Not necessary for all aggregate types."
    ],
    default: [
      type: :any,
      doc: "A default value to use for the aggregate if it returns `nil`."
    ],
    filterable?: [
      type: :boolean,
      doc: "Whether or not this aggregate may be used in filters.",
      default: true
    ],
    sortable?: [
      type: :boolean,
      doc: "Whether or not this aggregate may be used in sorts.",
      default: true
    ],
    type: [
      type: :any,
      doc: "A type to use for the aggregate."
    ],
    constraints: [
      type: :any,
      doc: "Type constraints to use for the aggregate."
    ],
    implementation: [
      type: :any,
      doc: "The implementation for any custom aggregates."
    ],
    read_action: [
      type: :atom,
      doc: "The read action to use for the aggregate, defaults to the primary read action."
    ],
    uniq?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not to only consider unique values. Only relevant for `count` and `list` aggregates."
    ],
    include_nil?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not to include `nil` values in the aggregate. Only relevant for `list` and `first` aggregates."
    ],
    join_filters: [
      type: {:map, {:wrap_list, :atom}, :any},
      default: %{},
      doc: """
      A map of relationship paths (an atom or list of atoms), to an expression to apply when fetching the aggregate data. See the aggregates guide for more.
      """
    ],
    sensitive?: [
      type: :boolean,
      doc: "Whether or not references to this aggregate will be considered sensitive",
      default: false
    ],
    authorize?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the aggregate query should authorize based on the target action.

      See `d:Ash.Resource.Dsl.aggregates|count` for more information.
      """
    ]
  ]

  @keys Keyword.keys(@schema)

  @doc false
  def opt_keys do
    @keys
  end

  @doc """
  Create a new aggregate, used with `Query.aggregate` or `Ash.aggregate`

  Options:

  #{Spark.Options.docs(@schema)}
  """
  def new(resource, name, kind, opts \\ []) do
    opts =
      Enum.reject(opts, fn
        {_key, nil} ->
          true

        _ ->
          false
      end)

    with {:ok, opts} <- Spark.Options.validate(opts, @schema) do
      related = Ash.Resource.Info.related(resource, opts[:path] || [])

      query =
        case opts[:query] || Ash.Query.new(related) do
          %Ash.Query{} = query -> query
          build_opts -> build_query(related, build_opts)
        end

      opts[:join_filters]
      |> Kernel.||(%{})
      |> Enum.reduce_while({:ok, %{}}, fn {path, filter}, {:ok, acc} ->
        case parse_join_filter(resource, path, filter) do
          {:ok, filter} ->
            {:cont, {:ok, Map.put(acc, path, filter)}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
      |> case do
        {:ok, join_filters} ->
          relationship = opts[:path] || []
          field = opts[:field]
          default = opts[:default]
          filterable? = opts[:filterable?]
          sortable? = opts[:sortable?]
          sensitive? = opts[:sensitive?]
          type = opts[:type]
          constraints = Keyword.get(opts, :constraints, [])
          implementation = opts[:implementation]
          uniq? = opts[:uniq?]
          read_action = opts[:read_action]
          authorize? = opts[:authorize?]

          if kind == :custom && !type do
            raise ArgumentError, "Must supply type when building a `custom` aggregate"
          end

          if kind == :custom && !implementation do
            raise ArgumentError, "Must supply implementation when building a `custom` aggregate"
          end

          related = Ash.Resource.Info.related(resource, relationship)

          attribute_type =
            if field do
              case Ash.Resource.Info.field(related, field) do
                %{type: type, constraints: constraints} ->
                  {:ok, type, constraints}

                _ ->
                  {:error, "No such field for #{inspect(related)}: #{inspect(field)}"}
              end
            else
              {:ok, nil, constraints}
            end

          default =
            if is_function(default) do
              default.()
            else
              default
            end

          with :ok <- validate_uniq(uniq?, kind),
               {:ok, attribute_type, attribute_constraints} <- attribute_type,
               :ok <- validate_path(resource, List.wrap(relationship)),
               {:ok, type, constraints} <-
                 get_type(kind, type, attribute_type, attribute_constraints, constraints),
               %{valid?: true} = query <- build_query(related, query) do
            {:ok,
             %__MODULE__{
               name: name,
               agg_name: name,
               resource: resource,
               constraints: constraints,
               default_value: default || default_value(kind),
               relationship_path: List.wrap(relationship),
               implementation: implementation,
               field: field,
               kind: kind,
               type: type,
               uniq?: uniq?,
               query: query,
               filterable?: filterable?,
               sortable?: sortable?,
               sensitive?: sensitive?,
               authorize?: authorize?,
               read_action: read_action,
               join_filters: Map.new(join_filters, fn {key, value} -> {List.wrap(key), value} end)
             }}
          else
            %{valid?: false} = query ->
              {:error, query.errors}

            {:error, error} ->
              {:error, error}
          end

        {:error, error} ->
          {:error, error}
      end
    end
  end

  defp parse_join_filter(resource, path, filter) do
    [last_relationship | relationships] =
      path_to_reversed_relationships(resource, path)

    top_parent_resource = (List.last(relationships) || last_relationship).source

    parent_resources =
      relationships |> Enum.map(& &1.destination) |> Enum.concat([top_parent_resource])

    Ash.Filter.parse(last_relationship.destination, filter, %{
      parent_stack: parent_resources
    })
  end

  defp path_to_reversed_relationships(resource, path, acc \\ [])
  defp path_to_reversed_relationships(_resource, [], acc), do: acc

  defp path_to_reversed_relationships(resource, [first | rest], acc) do
    relationship = Ash.Resource.Info.relationship(resource, first)

    if !relationship do
      raise ArgumentError, "No such relationship: #{inspect(resource)}.#{first} in join_filter"
    end

    path_to_reversed_relationships(relationship.destination, rest, [
      relationship | acc
    ])
  end

  defp validate_uniq(true, kind) when kind in [:count, :list], do: :ok

  defp validate_uniq(true, kind),
    do:
      {:error,
       "#{kind} aggregates do not support the `uniq?` option. Only count and list are supported currently."}

  defp validate_uniq(_, _), do: :ok

  defp get_type(:custom, type, _, _attribute_constraints, provided_constraints),
    do: {:ok, type, provided_constraints || []}

  defp get_type(kind, _, attribute_type, attribute_constraints, provided_constraints) do
    kind_to_type(kind, attribute_type, attribute_constraints || provided_constraints)
  end

  defp validate_path(_, []), do: :ok

  defp validate_path(resource, [relationship | rest]) do
    case Ash.Resource.Info.relationship(resource, relationship) do
      nil ->
        {:error, NoSuchRelationship.exception(resource: resource, relationship: relationship)}

      %{type: :many_to_many, through: through, destination: destination} ->
        cond do
          !Ash.Resource.Info.primary_action(through, :read) ->
            {:error, NoReadAction.exception(resource: through, when: "aggregating")}

          !Ash.Resource.Info.primary_action(destination, :read) ->
            {:error, NoReadAction.exception(resource: destination, when: "aggregating")}

          !Ash.DataLayer.data_layer(through) == Ash.DataLayer.data_layer(resource) ->
            {:error, "Cannot cross data layer boundaries when building an aggregate"}

          true ->
            validate_path(destination, rest)
        end

      relationship ->
        cond do
          !Ash.Resource.Info.primary_action(relationship.destination, :read) ->
            NoReadAction.exception(resource: relationship.destination, when: "aggregating")

          !Ash.DataLayer.data_layer(relationship.destination) ==
              Ash.DataLayer.data_layer(resource) ->
            {:error, "Cannot cross data layer boundaries when building an aggregate"}

          true ->
            validate_path(relationship.destination, rest)
        end
    end
  end

  @doc false
  def split_aggregate_opts(opts) do
    {left, right} = Keyword.split(opts, opt_keys())

    right =
      case Keyword.fetch(left, :authorize?) do
        {:ok, value} ->
          Keyword.put(right, :authorize?, value)

        :error ->
          right
      end

    case Keyword.fetch(right, :action) do
      {:ok, action} ->
        {Keyword.put(left, :read_action, action), right}

      :error ->
        {left, right}
    end
  end

  def default_value(:count), do: 0
  def default_value(:first), do: nil
  def default_value(:sum), do: nil
  def default_value(:max), do: nil
  def default_value(:min), do: nil
  def default_value(:avg), do: nil
  def default_value(:exists), do: nil
  def default_value(:list), do: []
  def default_value(:custom), do: nil

  @doc false
  def build_query(resource, nil), do: Ash.Query.new(resource)

  def build_query(resource, build_opts) when is_list(build_opts) do
    cond do
      build_opts[:limit] ->
        Ash.Query.add_error(resource, "Cannot set limit on aggregate query")

      build_opts[:offset] && build_opts[:offset] != 0 ->
        Ash.Query.add_error(resource, "Cannot set offset on aggregate query")

      true ->
        case Ash.Query.build(resource, build_opts) do
          %{valid?: true} = query ->
            build_query(resource, query)

          %{valid?: false} = query ->
            query
        end
    end
  end

  def build_query(_resource, %Ash.Query{} = query) do
    cond do
      query.limit ->
        Ash.Query.add_error(query, "Cannot set limit on aggregate query")

      query.offset && query.offset != 0 ->
        Ash.Query.add_error(query, "Cannot set offset on aggregate query")

      true ->
        Ash.Query.unset(query, [:load, :limit, :offset])
    end
  end

  @doc false
  def kind_to_type({:custom, type}, _attribute_type, _attribute_constraints), do: {:ok, type, []}

  def kind_to_type(kind, attribute_type, attribute_constraints)
      when kind in [:first, :sum, :max, :min],
      do: {:ok, attribute_type, attribute_constraints || []}

  def kind_to_type(:list, attribute_type, attribute_constraints),
    do: {:ok, {:array, attribute_type}, [items: attribute_constraints || []]}

  def kind_to_type(kind, attribute_type, _attribute_constraints) do
    with {:ok, type} <- kind_to_type(kind, attribute_type) do
      {:ok, type, []}
    end
  end

  defp kind_to_type({:custom, type}, _attribute_type), do: {:ok, type}
  defp kind_to_type(:count, _attribute_type), do: {:ok, Ash.Type.Integer}
  defp kind_to_type(:exists, _attribute_type), do: {:ok, Ash.Type.Boolean}
  defp kind_to_type(kind, nil), do: {:error, "Must provide field type for #{kind}"}
  defp kind_to_type(:avg, _attribute_type), do: {:ok, :float}

  defp kind_to_type(kind, attribute_type) when kind in [:first, :sum, :max, :min],
    do: {:ok, attribute_type}

  defp kind_to_type(:list, attribute_type), do: {:ok, {:array, attribute_type}}
  defp kind_to_type(kind, _attribute_type), do: {:error, "Invalid aggregate kind: #{kind}"}

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{query: nil} = aggregate, opts) do
      container_doc(
        "#" <> to_string(aggregate.kind) <> "<",
        [Enum.join(aggregate.relationship_path, ".")],
        ">",
        opts,
        fn str, _ -> str end,
        separator: ""
      )
    end

    def inspect(%{query: query} = aggregate, opts) do
      field =
        if aggregate.field do
          if is_atom(aggregate.field) do
            [to_string(aggregate.field)]
          else
            case aggregate.field do
              %{agg_name: agg_name} ->
                [to_string(agg_name)]

              %{calc_name: calc_name} ->
                [to_string(calc_name)]

              _ ->
                [inspect(aggregate.field)]
            end
          end
        else
          []
        end

      container_doc(
        "#" <> to_string(aggregate.kind) <> "<",
        [
          concat([
            Enum.join(aggregate.relationship_path ++ field, "."),
            concat(" from ", to_doc(query, opts))
          ])
        ],
        ">",
        opts,
        fn str, _ -> str end
      )
    end
  end
end
