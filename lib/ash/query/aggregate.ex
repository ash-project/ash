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
    context: %{},
    authorize?: true,
    uniq?: false,
    filterable?: true
  ]

  @type t :: %__MODULE__{}

  @kinds [:count, :first, :sum, :list, :max, :min, :avg, :exists, :custom]
  @type kind :: unquote(Enum.reduce(@kinds, &{:|, [], [&1, &2]}))

  alias Ash.Engine.Request
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
      doc: "Whether or not this aggregate may be used in filters."
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
  Create a new aggregate, used with `Query.aggregate` or `Api.aggregate`

  Options:

  #{Spark.OptionsHelpers.docs(@schema)}
  """
  def new(resource, name, kind, opts \\ []) do
    opts =
      Enum.reject(opts, fn
        {_key, nil} ->
          true

        _ ->
          false
      end)

    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @schema) do
      query =
        opts[:query] || Ash.Query.new(Ash.Resource.Info.related(resource, opts[:path] || []))

      query =
        if opts[:read_action] && query.__validated_for_action__ != opts[:read_action] do
          Ash.Query.for_read(query, opts[:read_action], %{})
        else
          query
        end

      new(
        resource,
        name,
        kind,
        opts[:path] || [],
        query,
        opts[:field],
        opts[:default],
        Keyword.get(opts, :filterable?, true),
        opts[:type],
        Keyword.get(opts, :constraints, []),
        opts[:implementation],
        opts[:uniq?],
        opts[:read_action],
        Keyword.get(opts, :authorize?, true)
      )
    end
  end

  @deprecated "Use `new/4` instead."
  def new(
        resource,
        name,
        kind,
        relationship,
        query,
        field,
        default \\ nil,
        filterable? \\ true,
        type \\ nil,
        constraints \\ [],
        implementation \\ nil,
        uniq? \\ false,
        read_action \\ nil,
        authorize? \\ true
      ) do
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
         {:ok, query} <- build_query(related, query) do
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
         authorize?: authorize?,
         read_action: read_action
       }}
    end
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
        {:error, NoSuchRelationship.exception(resource: resource, name: relationship)}

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

  defp build_query(_resource, nil), do: {:ok, nil}

  defp build_query(resource, build_opts) when is_list(build_opts) do
    case Ash.Query.build(resource, build_opts) do
      %{valid?: true} = query ->
        build_query(resource, query)

      %{valid?: false} = query ->
        {:error, query.errors}
    end
  end

  defp build_query(_resource, %Ash.Query{} = query) do
    {:ok, Ash.Query.unset(query, [:load, :limit, :offset])}
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

  @deprecated "use kind to type/3 instead"
  def kind_to_type({:custom, type}, _attribute_type), do: {:ok, type}
  def kind_to_type(:count, _attribute_type), do: {:ok, Ash.Type.Integer}
  def kind_to_type(:exists, _attribute_type), do: {:ok, Ash.Type.Boolean}
  def kind_to_type(kind, nil), do: {:error, "Must provide field type for #{kind}"}
  def kind_to_type(:avg, _attribute_type), do: {:ok, :float}

  def kind_to_type(kind, attribute_type) when kind in [:first, :sum, :max, :min],
    do: {:ok, attribute_type}

  def kind_to_type(:list, attribute_type), do: {:ok, {:array, attribute_type}}
  def kind_to_type(kind, _attribute_type), do: {:error, "Invalid aggregate kind: #{kind}"}

  def requests(
        initial_query,
        can_be_in_query?,
        authorizing?,
        _calculations_in_query,
        request_path
      ) do
    initial_query.aggregates
    |> Map.values()
    |> Enum.map(&{{&1.resource, &1.relationship_path, []}, &1})
    |> Enum.group_by(fn {{resource, relationship_path, ref_path}, aggregate} ->
      action =
        aggregate.read_action ||
          Ash.Resource.Info.primary_action(
            Ash.Resource.Info.related(resource, relationship_path),
            :read
          ).name

      {{resource, relationship_path, ref_path}, aggregate.authorize? && authorizing?, action}
    end)
    |> Enum.map(fn {key, value} ->
      {key, Enum.uniq_by(Enum.map(value, &elem(&1, 1)), &{&1.name, &1.load})}
    end)
    |> Enum.uniq()
    |> Enum.reduce({[], [], []}, fn {{{aggregate_resource, relationship_path, ref_path},
                                      authorize?, read_action}, aggregates},
                                    {auth_requests, value_requests, aggregates_in_query} ->
      related = Ash.Resource.Info.related(aggregate_resource, relationship_path)

      last_relationship =
        Enum.reduce(relationship_path, nil, fn
          relationship, nil ->
            Ash.Resource.Info.relationship(initial_query.resource, relationship)

          relationship, acc ->
            Ash.Resource.Info.relationship(acc.destination, relationship)
        end)

      can_be_in_query? = can_be_in_query? && ref_path == []

      opts =
        initial_query.context
        |> Map.get(:private, %{})
        |> Map.take([:tracer, :actor, :query])
        |> Map.to_list()

      related_query =
        related
        |> then(fn query ->
          if last_relationship do
            Ash.Query.set_context(query, %{
              accessing_from: %{source: last_relationship.source, name: last_relationship.name}
            })
          else
            query
          end
        end)
        |> Ash.Query.for_read(read_action, opts)

      auth_request =
        if authorize? do
          auth_request(
            related,
            initial_query,
            relationship_path,
            request_path,
            related_query,
            authorize?,
            read_action
          )
        else
          nil
        end

      new_auth_requests =
        if auth_request do
          [auth_request | auth_requests]
        else
          auth_requests
        end

      if can_be_in_query? do
        {new_auth_requests, value_requests, aggregates_in_query ++ aggregates}
      else
        if ref_path == [] do
          request =
            value_request(
              initial_query,
              relationship_path,
              aggregates,
              auth_request,
              aggregate_resource,
              request_path,
              related_query,
              authorize?,
              read_action
            )

          {new_auth_requests, [request | value_requests], aggregates_in_query}
        else
          {new_auth_requests, value_requests, aggregates_in_query}
        end
      end
    end)
  end

  defp auth_request(
         related,
         initial_query,
         relationship_path,
         request_path,
         related_query,
         authorize?,
         read_action
       ) do
    auth_filter_path =
      request_path ++
        [:aggregate, relationship_path, {authorize?, read_action}, :authorization_filter]

    action =
      if read_action do
        Ash.Resource.Info.action(related, read_action)
      else
        Ash.Resource.Info.primary_action!(related, :read)
      end

    Request.new(
      resource: related,
      api: initial_query.api,
      async?: false,
      query: related_query,
      path: request_path ++ [:aggregate, relationship_path, {authorize?, read_action}],
      strict_check_only?: true,
      action: action,
      name: "authorize aggregate: #{Enum.join(relationship_path, ".")}",
      data:
        Request.resolve([auth_filter_path], fn data ->
          {:ok, get_in(data, auth_filter_path)}
        end)
    )
  end

  defp value_request(
         initial_query,
         relationship_path,
         aggregates,
         auth_request,
         aggregate_resource,
         request_path,
         related_query,
         authorize?,
         read_action
       ) do
    pkey = Ash.Resource.Info.primary_key(aggregate_resource)

    deps =
      if auth_request do
        [auth_request.path ++ [:authorization_filter], request_path ++ [:fetch, :data]]
      else
        [request_path ++ [:fetch, :data]]
      end

    Request.new(
      resource: aggregate_resource,
      api: initial_query.api,
      query: related_query,
      authorize?: false,
      path: request_path ++ [:aggregate_values, relationship_path, {authorize?, read_action}],
      action: Ash.Resource.Info.primary_action(aggregate_resource, :read),
      name: "fetch aggregate: #{Enum.join(relationship_path, ".")}",
      data:
        Request.resolve(
          deps,
          fn data ->
            records = get_in(data, request_path ++ [:fetch, :data, :results])

            if records == [] do
              {:ok, %{}}
            else
              initial_query =
                initial_query
                |> Ash.Query.unset([:filter, :sort, :aggregates, :limit, :offset, :select])
                |> Ash.Query.select([])

              query =
                case records do
                  [record] ->
                    filter = record |> Map.take(pkey) |> Enum.to_list()

                    Ash.Query.filter(
                      initial_query,
                      ^filter
                    )

                  records ->
                    filter = [or: Enum.map(records, &Map.take(&1, pkey))]

                    Ash.Query.filter(
                      initial_query,
                      ^filter
                    )
                end

              aggregates =
                if auth_request do
                  case get_in(data, auth_request.path ++ [:authorization_filter]) do
                    nil ->
                      aggregates

                    filter ->
                      Enum.map(aggregates, fn aggregate ->
                        %{
                          aggregate
                          | query: Ash.Query.filter(aggregate.query, ^filter)
                        }
                      end)
                  end
                else
                  aggregates
                end

              with {:ok, data_layer_query} <- Ash.Query.data_layer_query(query),
                   {:ok, data_layer_query} <-
                     add_data_layer_aggregates(
                       data_layer_query,
                       aggregates,
                       initial_query.resource
                     ),
                   {:ok, results} <-
                     Ash.DataLayer.run_query(
                       data_layer_query,
                       query.resource
                     )
                     |> Ash.Actions.Helpers.rollback_if_in_transaction(query.resource, query) do
                loaded_aggregates =
                  aggregates
                  |> Enum.map(& &1.load)
                  |> Enum.reject(&is_nil/1)

                all_aggregates = Enum.map(aggregates, & &1.name)

                aggregate_values =
                  Enum.reduce(results, %{}, fn result, acc ->
                    loaded_aggregate_values = Map.take(result, loaded_aggregates)

                    all_aggregate_values =
                      result.aggregates
                      |> Kernel.||(%{})
                      |> Map.take(all_aggregates)
                      |> Map.merge(loaded_aggregate_values)

                    Map.put(
                      acc,
                      Map.take(result, pkey),
                      all_aggregate_values
                    )
                  end)

                {:ok, aggregate_values}
              else
                {:error, error} ->
                  {:error, error}
              end
            end
          end
        )
    )
  end

  defp add_data_layer_aggregates(data_layer_query, aggregates, aggregate_resource) do
    Ash.DataLayer.add_aggregates(data_layer_query, aggregates, aggregate_resource)
  end

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
          [aggregate.field]
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
