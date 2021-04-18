defmodule Ash.Actions.Read do
  @moduledoc false

  alias Ash.Actions.{Helpers, SideLoad}
  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Error.Invalid.{LimitRequired, PaginationRequired}
  alias Ash.Error.Query.NoReadAction
  alias Ash.Filter
  alias Ash.Query.Aggregate

  require Logger
  require Ash.Query

  def unpaginated_read(query, action \\ nil, opts \\ []) do
    action = action || Ash.Resource.Info.primary_action(query.resource, :read)

    cond do
      !action ->
        {:error, NoReadAction.exception(resource: query.resource, when: "reading")}

      action.pagination ->
        opts = Keyword.put(opts, :page, false)
        run(query, %{action | pagination: %{action.pagination | required?: false}}, opts)

      true ->
        run(query, action, opts)
    end
  end

  @spec run(Ash.Query.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Page.page() | list(Ash.Resource.record())}
          | {:ok, Ash.Page.page() | list(Ash.Resource.record()), Ash.Query.t()}
          | {:error, term}
  def run(query, action, opts \\ []) do
    authorize? =
      if opts[:authorize?] == false do
        false
      else
        opts[:authorize?] || Keyword.has_key?(opts, :actor)
      end

    opts = Keyword.put(opts, :authorize?, authorize?)
    opts = Keyword.merge(opts, Map.get(query.context, :override_api_params) || [])

    engine_opts = Keyword.take(opts, [:verbose?, :actor, :authorize?])
    query = %{query | action: action}

    original_query = query

    initial_offset = query.offset
    initial_limit = query.limit

    query =
      if query.__validated_for_action__ == action.name do
        query
      else
        Ash.Query.for_read(query, action.name, %{}, actor: engine_opts[:actor])
      end

    with %{valid?: true} <- query,
         :ok <- validate_multitenancy(query, opts),
         %{errors: []} = query <- query_with_initial_data(query, opts),
         {:ok, filter_requests} <- filter_requests(query, opts),
         {:ok, query, page_opts, count_request} <-
           paginate(query, action, filter_requests, initial_offset, initial_limit, opts),
         page_opts <- page_opts && Keyword.delete(page_opts, :filter),
         {:ok, requests} <- requests(query, action, filter_requests, opts),
         {query, side_load_requests} <- SideLoad.requests(query),
         {:ok, %{data: %{data: data} = all_data}} <-
           Engine.run(
             requests ++ side_load_requests ++ List.wrap(count_request),
             query.api,
             engine_opts
           ),
         data_with_side_loads <- SideLoad.attach_side_loads(data, all_data),
         data_with_aggregates <-
           add_aggregate_values(
             data_with_side_loads,
             query.aggregates,
             query.resource,
             Map.get(all_data, :aggregate_values, %{})
           ) do
      data_with_aggregates
      |> add_tenant(query)
      |> add_page(
        action,
        Map.get(all_data, :count),
        query.sort,
        original_query,
        Keyword.put(opts, :page, page_opts)
      )
      |> add_query(Map.get(all_data, :ultimate_query), opts)
    else
      %Ash.Query{errors: errors} = query ->
        {:error, Ash.Error.to_error_class(errors, query: query)}

      {:error, %Ash.Engine.Runner{errors: errors}} ->
        {:error, Ash.Error.to_error_class(errors, query: query)}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error, query: query)}
    end
  end

  defp validate_multitenancy(query, opts) do
    if is_nil(Ash.Resource.Info.multitenancy_strategy(query.resource)) ||
         Ash.Resource.Info.multitenancy_global?(query.resource) || query.tenant ||
         opts[:initial_data] do
      :ok
    else
      {:error,
       "Queries against the #{inspect(query.resource)} resource require a tenant to be specified"}
    end
  end

  defp add_tenant(data, query) do
    if Ash.Resource.Info.multitenancy_strategy(query.resource) do
      Enum.map(data, fn item ->
        %{item | __metadata__: Map.put(item.__metadata__, :tenant, query.tenant)}
      end)
    else
      data
    end
  end

  defp add_query(result, query, opts) do
    if opts[:return_query?] do
      {:ok, result, query}
    else
      {:ok, result}
    end
  end

  defp add_page(data, action, count, sort, original_query, opts) do
    if opts[:page] && action.pagination do
      to_page(data, action, count, sort, original_query, opts)
    else
      data
    end
  end

  defp to_page(data, action, count, sort, original_query, opts) do
    page_opts = opts[:page]

    {data, rest} =
      if opts[:page][:limit] do
        Enum.split(data, opts[:page][:limit])
      else
        {data, []}
      end

    more? = not Enum.empty?(rest)

    if page_opts[:offset] do
      if action.pagination.keyset? do
        data
        |> Ash.Page.Keyset.data_with_keyset(sort)
        |> Ash.Page.Offset.new(count, original_query, more?, opts)
      else
        Ash.Page.Offset.new(data, count, original_query, more?, opts)
      end
    else
      cond do
        action.pagination.offset? && action.pagination.keyset? ->
          data
          |> Ash.Page.Keyset.data_with_keyset(sort)
          |> Ash.Page.Offset.new(count, original_query, more?, opts)

        action.pagination.offset? ->
          Ash.Page.Offset.new(data, count, original_query, more?, opts)

        true ->
          Ash.Page.Keyset.new(data, count, sort, original_query, more?, opts)
      end
    end
  end

  defp filter_requests(query, opts) do
    authorizing? =
      if opts[:authorize?] == false do
        false
      else
        Keyword.has_key?(opts, :actor) || opts[:authorize?]
      end

    if not Keyword.has_key?(opts, :initial_data) &&
         authorizing? do
      Filter.read_requests(query.api, query.filter)
    else
      {:ok, []}
    end
  end

  defp query_with_initial_data(query, opts) do
    case Keyword.fetch(opts, :initial_data) do
      :error ->
        query

      {:ok, nil} ->
        Ash.Query.filter(query, false)

      {:ok, []} ->
        Ash.Query.filter(query, false)

      {:ok, [record]} ->
        pkey_value =
          record |> Map.take(Ash.Resource.Info.primary_key(query.resource)) |> Map.to_list()

        Ash.Query.filter(query, ^pkey_value)

      {:ok, %{} = record} ->
        pkey_value =
          record |> Map.take(Ash.Resource.Info.primary_key(query.resource)) |> Map.to_list()

        Ash.Query.filter(query, ^pkey_value)

      {:ok, records} when is_list(records) ->
        pkey = Ash.Resource.Info.primary_key(query.resource)
        pkey_value = Enum.map(records, fn record -> record |> Map.take(pkey) |> Map.to_list() end)

        filter = [or: pkey_value]
        Ash.Query.filter(query, ^filter)
    end
  end

  defp requests(query, action, filter_requests, opts) do
    authorizing? =
      if opts[:authorize?] == false do
        false
      else
        Keyword.has_key?(opts, :actor) || opts[:authorize?]
      end

    can_be_in_query? = not Keyword.has_key?(opts, :initial_data)

    {aggregate_auth_requests, aggregate_value_requests, aggregates_in_query} =
      Aggregate.requests(query, can_be_in_query?, authorizing?)

    request =
      Request.new(
        resource: query.resource,
        api: query.api,
        query:
          Request.resolve([], fn _ ->
            case Filter.run_other_data_layer_filters(
                   query.api,
                   query.resource,
                   query.filter
                 ) do
              {:ok, filter} ->
                {:ok, %{query | filter: filter}}

              {:error, error} ->
                {:error, error}
            end
          end),
        action: action,
        authorize?: not Keyword.has_key?(opts, :initial_data),
        data:
          data_field(
            opts,
            filter_requests,
            aggregate_auth_requests,
            aggregates_in_query,
            query
          ),
        path: [:data],
        name: "#{action.type} - `#{action.name}`"
      )

    {:ok, [request | filter_requests] ++ aggregate_auth_requests ++ aggregate_value_requests}
  end

  defp data_field(
         params,
         filter_requests,
         aggregate_auth_requests,
         aggregates_in_query,
         initial_query
       ) do
    if params[:initial_data] do
      Request.resolve([], fn _ ->
        add_calculation_values(initial_query, params[:initial_data], initial_query.calculations)
      end)
    else
      relationship_filter_paths =
        Enum.map(filter_requests, fn request ->
          request.path ++ [:authorization_filter]
        end)

      aggregate_auth_paths =
        Enum.map(aggregate_auth_requests, fn request ->
          request.path ++ [:authorization_filter]
        end)

      deps = [
        [:data, :query]
        | relationship_filter_paths ++ aggregate_auth_paths
      ]

      Request.resolve(
        deps,
        fn %{data: %{query: ash_query}} = data ->
          multitenancy_attribute = Ash.Resource.Info.multitenancy_attribute(ash_query.resource)

          ash_query =
            if multitenancy_attribute && ash_query.tenant do
              {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(ash_query.resource)
              attribute_value = apply(m, f, [ash_query.tenant | a])
              Ash.Query.filter(ash_query, [{multitenancy_attribute, attribute_value}])
            else
              ash_query
            end
            |> run_before_action()

          query =
            initial_query
            |> Ash.Query.unset([:filter, :aggregates, :sort])
            |> Ash.Query.data_layer_query(only_validate_filter?: true)

          with %{valid?: true} <- ash_query,
               {:ok, query} <- query,
               {:ok, filter} <-
                 filter_with_related(relationship_filter_paths, ash_query, data),
               {:ok, query} <-
                 Ash.DataLayer.select(
                   query,
                   Helpers.attributes_to_select(ash_query),
                   ash_query.resource
                 ),
               {:ok, query} <-
                 add_aggregates(
                   query,
                   ash_query,
                   aggregates_in_query,
                   Map.get(data, :aggregate, %{})
                 ),
               {:ok, query} <-
                 Ash.DataLayer.filter(query, filter, ash_query.resource),
               {:ok, query} <-
                 Ash.DataLayer.sort(query, ash_query.sort, ash_query.resource),
               {:ok, query} <-
                 Ash.DataLayer.distinct(query, ash_query.distinct, ash_query.resource),
               {:ok, query} <-
                 Ash.DataLayer.set_context(ash_query.resource, query, ash_query.context),
               {:ok, query} <- set_tenant(query, ash_query),
               {:ok, results} <- run_query(ash_query, query),
               {:ok, results} <- run_after_action(initial_query, results),
               {:ok, with_calculations} <-
                 add_calculation_values(ash_query, results, ash_query.calculations) do
            if params[:return_query?] do
              ultimate_query =
                ash_query
                |> Ash.Query.unset(:filter)
                |> Ash.Query.filter(filter)

              {:ok, with_calculations, %{extra_data: %{ultimate_query: ultimate_query}}}
            else
              {:ok, with_calculations}
            end
          else
            %{valid?: false} = query ->
              {:error, query}

            other ->
              other
          end
        end
      )
    end
  end

  defp run_before_action(query) do
    Enum.reduce(query.before_action, query, & &1.(&2))
  end

  defp run_after_action(query, results) do
    query.after_action
    |> Enum.reduce_while({query, {:ok, results}}, fn after_action, {query, {:ok, results}} ->
      case after_action.(query, results) do
        {:ok, results} -> {:cont, {query, {:ok, results}}}
        {:error, error} -> {:halt, {query, {:error, error}}}
      end
    end)
    |> elem(1)
  end

  defp set_tenant(query, ash_query) do
    if Ash.Resource.Info.multitenancy_strategy(ash_query.resource) == :context && ash_query.tenant do
      Ash.DataLayer.set_tenant(ash_query.resource, query, ash_query.tenant)
    else
      {:ok, query}
    end
  end

  defp page_opts(action, opts) do
    cond do
      !(action.pagination && action.pagination.default_limit) ->
        opts[:page]

      Keyword.keyword?(opts[:page]) && !Keyword.has_key?(opts[:page], :limit) ->
        Keyword.put(opts[:page], :limit, action.pagination.default_limit)

      is_nil(opts[:page]) ->
        [limit: action.pagination.default_limit]

      true ->
        opts[:page]
    end
  end

  defp paginate(starting_query, action, filter_requests, initial_offset, initial_limit, opts) do
    page_opts = page_opts(action, opts)

    cond do
      action.pagination == false && page_opts ->
        {:error, "Pagination is not supported"}

      action.pagination == false ->
        {:ok, starting_query, opts[:page], nil}

      page_opts == false ->
        if action.pagination.required? do
          {:error, PaginationRequired.exception([])}
        else
          {:ok, starting_query, false, nil}
        end

      page_opts[:limit] || is_nil(page_opts) || page_opts == [] ->
        case do_paginate(starting_query, action.pagination, opts) do
          {:ok, query} ->
            count_request =
              count_request(
                starting_query,
                action,
                filter_requests,
                initial_offset,
                initial_limit,
                opts
              )

            {:ok, query, page_opts, count_request}

          {:error, error} ->
            {:error, error}
        end

      action.pagination.required? ->
        {:error, LimitRequired.exception([])}

      true ->
        {:error, LimitRequired.exception([])}
    end
  end

  defp do_paginate(query, pagination, opts) do
    paginated =
      cond do
        opts[:page][:before] || opts[:page][:after] ->
          keyset_pagination(query, opts[:page])

        opts[:page][:offset] ->
          limit_offset_pagination(query, opts[:page])

        pagination.offset? && pagination.keyset? ->
          keyset_pagination(query, opts[:page])

        pagination.offset? ->
          limit_offset_pagination(query, opts[:page])

        true ->
          keyset_pagination(query, opts[:page])
      end

    case paginated do
      {:ok, paginated} ->
        if opts[:page][:filter] do
          {:ok, Ash.Query.filter(paginated, ^opts[:page][:filter])}
        else
          {:ok, paginated}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp keyset_pagination(query, opts) do
    sorted =
      if Ash.Actions.Sort.sorting_on_identity?(query) do
        query
      else
        Ash.Query.sort(query, Ash.Resource.Info.primary_key(query.resource))
      end

    limited =
      cond do
        opts[:limit] && sorted.limit ->
          Ash.Query.limit(sorted, min(opts[:limit], sorted.limit) + 1)

        opts[:limit] ->
          Ash.Query.limit(sorted, opts[:limit] + 1)

        true ->
          sorted
      end

    if opts[:before] || opts[:after] do
      after_or_before =
        if opts[:before] do
          :before
        else
          :after
        end

      case Ash.Page.Keyset.filter(
             opts[:before] || opts[:after],
             sorted.sort,
             after_or_before
           ) do
        {:ok, filter} ->
          {:ok, Ash.Query.filter(limited, ^filter)}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, limited}
    end
  end

  defp limit_offset_pagination(query, opts) do
    limited =
      cond do
        opts[:limit] && query.limit ->
          Ash.Query.limit(query, min(opts[:limit], query.limit) + 1)

        opts[:limit] ->
          Ash.Query.limit(query, opts[:limit] + 1)

        true ->
          query
      end

    with_offset =
      cond do
        opts[:offset] && query.offset ->
          Ash.Query.offset(limited, max(opts[:offset], query.offset))

        opts[:offset] ->
          Ash.Query.offset(limited, opts[:offset])

        true ->
          limited
      end

    {:ok, with_offset}
  end

  defp count_request(_, %{pagination: %{countable: false}}, _, _, _, _), do: nil

  defp count_request(initial_query, action, filter_requests, initial_offset, initial_limit, opts) do
    if opts[:page][:count] == true ||
         (opts[:page][:count] != false and action.pagination.countable == :by_default) do
      relationship_filter_paths =
        Enum.map(filter_requests, fn request ->
          request.path ++ [:authorization_filter]
        end)

      Request.new(
        resource: initial_query.resource,
        api: initial_query.api,
        query: initial_query,
        action: action,
        authorize?: false,
        data:
          Request.resolve(
            [[:data, :authorization_filter]] ++ relationship_filter_paths,
            fn %{
                 data: %{
                   authorization_filter: auth_filter
                 }
               } = data ->
              query =
                initial_query
                |> Ash.Query.unset([:filter, :aggregates, :sort, :limit, :offset, :select])
                |> Ash.Query.limit(initial_limit)
                |> Ash.Query.offset(initial_offset)
                |> Ash.Query.filter(^auth_filter)
                |> Ash.Query.data_layer_query(only_validate_filter?: true)

              with {:ok, query} <- query,
                   {:ok, filter} <-
                     filter_with_related(relationship_filter_paths, initial_query, data),
                   {:ok, query} <-
                     Ash.DataLayer.filter(query, filter, initial_query.resource),
                   {:ok, query} <-
                     Ash.DataLayer.sort(query, initial_query.sort, initial_query.resource),
                   {:ok, query} <-
                     Ash.DataLayer.distinct(query, initial_query.distinct, initial_query.resource),
                   {:ok, %{count: count}} <- run_count_query(initial_query, query) do
                {:ok, count}
              end
            end
          ),
        path: [:count],
        name: "#{action.type} - `#{action.name}`"
      )
    end
  end

  defp run_query(
         %{
           resource: destination_resource,
           context: %{
             data_layer: %{
               lateral_join_source: {root_data, resource, source, destination}
             }
           }
         } = ash_query,
         query
       ) do
    query
    |> Ash.DataLayer.run_query_with_lateral_join(
      root_data,
      resource,
      destination_resource,
      source,
      destination
    )
    |> Helpers.select(ash_query)
  end

  defp run_query(%{resource: resource} = ash_query, query) do
    query
    |> Ash.DataLayer.run_query(resource)
    |> Helpers.select(ash_query)
  end

  defp run_count_query(
         %{
           resource: destination_resource,
           context: %{
             data_layer: %{lateral_join_source: {root_data, resource, source, destination}}
           }
         },
         query
       ) do
    case Ash.Query.Aggregate.new(destination_resource, :count, :count, [], nil, nil) do
      {:ok, aggregate} ->
        Ash.DataLayer.run_aggregate_query_with_lateral_join(
          query,
          [aggregate],
          root_data,
          resource,
          destination_resource,
          source,
          destination
        )

      {:error, error} ->
        {:error, error}
    end
  end

  defp run_count_query(ash_query, query) do
    case Ash.Query.Aggregate.new(ash_query.resource, :count, :count, [], nil, nil) do
      {:ok, aggregate} ->
        Ash.DataLayer.run_aggregate_query(query, [aggregate], ash_query.resource)

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_calculation_values(query, results, calculations) do
    calculations
    |> Enum.reduce_while({:ok, %{}}, fn {_name, calculation}, {:ok, calculation_results} ->
      context = Map.put(calculation.context, :context, query.context)

      case calculation.module.calculate(results, calculation.opts, context) do
        results when is_list(results) ->
          {:cont, {:ok, Map.put(calculation_results, calculation, results)}}

        {:ok, results} ->
          {:cont, {:ok, Map.put(calculation_results, calculation, results)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, calculation_results} ->
        {:ok,
         Enum.reduce(calculation_results, results, fn {calculation, values}, records ->
           if calculation.load do
             :lists.zipwith(
               fn record, value -> Map.put(record, calculation.name, value) end,
               records,
               values
             )
           else
             :lists.zipwith(
               fn record, value ->
                 %{record | calculations: Map.put(record.calculations, calculation.name, value)}
               end,
               records,
               values
             )
           end
         end)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_aggregate_values(results, _aggregates, _resource, aggregate_values)
       when aggregate_values == %{},
       do: Enum.map(results, &Map.update!(&1, :aggregates, fn agg -> agg || %{} end))

  defp add_aggregate_values(results, aggregates, resource, aggregate_values) do
    keys_to_aggregates =
      Enum.reduce(aggregate_values, %{}, fn {_name, keys_to_values}, acc ->
        Enum.reduce(keys_to_values, acc, fn {pkey, values}, acc ->
          Map.update(acc, pkey, values, &Map.merge(&1, values))
        end)
      end)

    pkey = Ash.Resource.Info.primary_key(resource)

    loaded =
      aggregates
      |> Enum.map(fn {_, aggregate} -> aggregate.load end)
      |> Enum.reject(&is_nil/1)

    Enum.map(results, fn result ->
      aggregate_values = Map.get(keys_to_aggregates, Map.take(result, pkey), %{})

      {top_level, nested} = Map.split(aggregate_values || %{}, loaded)

      Map.merge(%{result | aggregates: Map.merge(result.aggregates, nested)}, top_level)
    end)
  end

  defp add_aggregates(data_layer_query, query, aggregates_to_add, aggregate_filters) do
    aggregates_to_add =
      Enum.into(aggregates_to_add, %{}, fn aggregate ->
        {aggregate.name, aggregate}
      end)

    aggregates_to_add
    |> Enum.reduce({aggregates_to_add, aggregate_filters}, fn {name, aggregate},
                                                              {aggregates, aggregate_filters} ->
      case Map.fetch(aggregate_filters, aggregate.relationship_path) do
        {:ok, %{authorization_filter: filter}} ->
          {Map.put(aggregates, name, %{aggregate | authorization_filter: filter}),
           Map.delete(aggregates, aggregate.relationship_path)}

        :error ->
          {aggregates, aggregate_filters}
      end
    end)
    |> elem(0)
    |> Enum.reduce_while({:ok, data_layer_query}, fn {_name, aggregate},
                                                     {:ok, data_layer_query} ->
      case Ash.DataLayer.add_aggregate(data_layer_query, aggregate, query.resource) do
        {:ok, new_query} -> {:cont, {:ok, new_query}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp filter_with_related(relationship_filter_paths, ash_query, data) do
    Enum.reduce_while(relationship_filter_paths, {:ok, ash_query.filter}, fn path,
                                                                             {:ok, filter} ->
      case get_in(data, path) do
        nil ->
          {:cont, {:ok, filter}}

        authorization_filter ->
          add_authorization_filter(filter, authorization_filter)
      end
    end)
  end

  defp add_authorization_filter(filter, authorization_filter) do
    case Ash.Filter.add_to_filter(filter, authorization_filter) do
      {:ok, new_filter} ->
        {:cont, {:ok, new_filter}}

      {:error, error} ->
        {:halt, {:error, error}}
    end
  end
end
