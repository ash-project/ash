defmodule Ash.Actions.Read do
  @moduledoc false

  alias Ash.Actions.{Helpers, Load}
  alias Ash.Engine.Request
  alias Ash.Error.Invalid.{LimitRequired, PaginationRequired}
  alias Ash.Error.Query.NoReadAction
  alias Ash.Filter
  alias Ash.Query.Aggregate

  require Logger
  require Ash.Query
  require Ash.Tracer

  def unpaginated_read(query, action \\ nil, opts \\ []) do
    action =
      cond do
        action && is_atom(action) ->
          Ash.Resource.Info.action(query.resource, action)

        action ->
          action

        true ->
          Ash.Resource.Info.primary_action!(query.resource, :read)
      end

    cond do
      !action ->
        {:error, NoReadAction.exception(resource: query.resource, when: "reading")}

      action.pagination ->
        opts = Keyword.put(opts, :page, false)
        run(query, %{action | pagination: false}, opts)

      true ->
        run(query, action, opts)
    end
  end

  def unpaginated_read_request(query, action \\ nil, opts \\ []) do
    action =
      cond do
        action && is_atom(action) ->
          Ash.Resource.Info.action(query.resource, action)

        action ->
          action

        true ->
          Ash.Resource.Info.primary_action!(query.resource, :read)
      end

    cond do
      !action ->
        {:error, NoReadAction.exception(resource: query.resource, when: "reading")}

      action.pagination ->
        opts = Keyword.put(opts, :page, false)
        run(query, %{action | pagination: false}, opts)

      true ->
        run(query, action, opts)
    end
  end

  @spec run(Ash.Query.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Page.page() | list(Ash.Resource.record())}
          | {:ok, Ash.Page.page() | list(Ash.Resource.record()), Ash.Query.t()}
          | {:error, term}
  def run(query, action, opts \\ []) do
    {query, opts} = Ash.Actions.Helpers.add_process_context(query.api, query, opts)

    Ash.Tracer.span :action,
                    Ash.Api.Info.span_name(query.api, query.resource, action.name),
                    opts[:tracer] do
      metadata = %{
        api: query.api,
        resource: query.resource,
        resource_short_name: Ash.Resource.Info.short_name(query.resource),
        actor: opts[:actor],
        tenant: opts[:tenant],
        action: action.name,
        authorize?: opts[:authorize?]
      }

      Ash.Tracer.telemetry_span [:ash, Ash.Api.Info.short_name(query.api), :read], metadata do
        Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

        case do_run(query, action, opts) do
          {:error, error} ->
            if opts[:tracer] do
              opts[:tracer].set_error(Ash.Error.to_error_class(error))
            end

            {:error, error}

          other ->
            other
        end
      end
    end
  end

  defp do_run(query, action, opts) do
    {query, opts} =
      if opts[:unsafe_no_authorize?] do
        {Ash.Query.set_context(query, %{private: %{authorize?: false}}),
         Keyword.put(opts, :authorize?, false)}
      else
        {query, opts}
      end

    authorize? = authorize?(opts)
    opts = sanitize_opts(opts, authorize?, query)
    query = set_tenant_opt(query, opts)
    action = get_action(query.resource, action)

    engine_opts =
      opts
      |> Keyword.put(:authorize?, authorize?)
      |> engine_opts(action, query.api, query.resource, opts[:tracer])

    query =
      if opts[:load] do
        Ash.Query.load(query, opts[:load])
      else
        query
      end

    query =
      for_read(
        query,
        action,
        actor: engine_opts[:actor],
        authorize?: engine_opts[:authorize?],
        timeout: opts[:timeout],
        tenant: opts[:tenant]
      )

    request_opts =
      Keyword.merge(engine_opts,
        query: query,
        page: opts[:page],
        return_query?: opts[:return_query?],
        timeout: opts[:timeout]
      )

    request_opts =
      if Keyword.has_key?(opts, :initial_data) do
        Keyword.put(request_opts, :initial_data, opts[:initial_data])
      else
        request_opts
      end

    request_opts =
      request_opts
      |> Keyword.put(:lazy?, opts[:lazy?] || false)
      |> Keyword.put(:tracer, opts[:tracer])

    requests =
      as_requests(
        [:data],
        query.resource,
        query.api,
        action,
        request_opts
      )

    case Ash.Engine.run(requests, engine_opts) do
      {:ok, %{data: %{data: %{data: data} = all_data}}} ->
        add_query(data, all_data[:fetch][:ultimate_query], request_opts)

      {:error, %Ash.Engine{errors: errors, requests: requests}} ->
        case Enum.find_value(requests, fn request ->
               if request.path == [:fetch] && match?(%Ash.Query{}, request.query) do
                 request.changeset
               end
             end) do
          nil ->
            {:error, Ash.Error.to_error_class(errors, query: query)}

          query ->
            {:error, Ash.Error.to_error_class(errors, query: query)}
        end

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error, query: query)}
    end
  end

  def as_requests(path, resource, api, action, request_opts) do
    query_dependencies = request_opts[:query_dependencies] || []
    query_input = request_opts[:query_input] || fn _ -> %{} end
    modify_query = request_opts[:modify_query] || fn query, _ -> query end
    initial_data = request_opts[:initial_data]
    query = request_opts[:query]
    get? = !!request_opts[:get?]
    tenant = request_opts[:tenant]
    timeout = request_opts[:timeout]
    error_path = request_opts[:error_path]
    lazy? = request_opts[:lazy?]
    tracer = request_opts[:tracer]

    query =
      if initial_data && query && lazy? do
        new_load =
          query.load
          |> List.wrap()
          |> Enum.reject(fn load ->
            case load do
              {key, _value} ->
                calculation_or_aggregate?(resource, key) &&
                  Ash.Resource.loaded?(resource, key)

              key ->
                calculation_or_aggregate?(resource, key) &&
                  Ash.Resource.loaded?(resource, key)
            end
          end)

        %{query | load: new_load}
      else
        query
      end

    fetch =
      Request.new(
        resource: resource,
        api: api,
        action: action,
        error_path: error_path,
        query:
          Request.resolve(
            query_dependencies,
            fn %{authorize?: authorize?, actor: actor} = context ->
              query_opts = query_opts(request_opts, context)

              input = query_input.(context) || %{}

              tenant =
                case tenant do
                  nil ->
                    nil

                  tenant when is_function(tenant) ->
                    tenant.(context)

                  tenant ->
                    tenant
                end

              query =
                case query do
                  nil ->
                    Ash.Query.for_read(resource, action.name, input,
                      tenant: tenant,
                      actor: actor,
                      tracer: tracer,
                      authorize?: authorize?,
                      timeout: timeout
                    )

                  query ->
                    for_read(
                      query,
                      action,
                      actor: actor,
                      tenant: tenant,
                      authorize?: authorize?,
                      timeout: timeout,
                      tracer: tracer
                    )
                end

              query = %{
                query
                | api: api,
                  timeout: timeout || query.timeout || Ash.Api.Info.timeout(api)
              }

              query =
                if tenant do
                  Ash.Query.set_tenant(query, tenant)
                else
                  query
                end

              with %{valid?: true} = query <- modify_query.(query, context),
                   %{limit: initial_limit, offset: initial_offset} <- query,
                   %{valid?: true} = query <-
                     handle_attribute_multitenancy(query),
                   :ok <- validate_multitenancy(query, initial_data),
                   %{valid?: true} = query <-
                     query_with_initial_data(query, request_opts),
                   {:ok, initial_query, query, page_opts} <-
                     paginate(query, action, page: request_opts[:page]),
                   page_opts <- page_opts && Keyword.delete(page_opts, :filter),
                   {%{valid?: true} = query, before_notifications} <-
                     run_before_action(query),
                   {:ok, filter} <-
                     Filter.run_other_data_layer_filters(
                       query.api,
                       query.resource,
                       query.filter,
                       {path, query.tenant, context}
                     ),
                   {query, load_requests} <-
                     Load.requests(
                       query,
                       lazy?,
                       [actor: actor, authorize?: authorize?, tracer: request_opts[:tracer]],
                       path ++ [:fetch]
                     ),
                   {:ok, sort} <-
                     Ash.Actions.Sort.process(
                       query.resource,
                       query.sort,
                       query.aggregates,
                       query.context
                     ) do
                load_paths = Enum.map(load_requests, & &1.path)

                {:ok,
                 query
                 |> Map.put(:filter, filter)
                 |> Map.put(:sort, sort)
                 |> Ash.Query.set_context(%{
                   load_paths: load_paths,
                   initial_limit: initial_limit,
                   initial_offset: initial_offset,
                   page_opts: page_opts,
                   initial_query: initial_query,
                   query_opts: query_opts
                 }),
                 %{
                   requests: load_requests,
                   notifications: before_notifications
                 }}
              else
                {:filter_requests, requests} ->
                  {:requests, requests}

                %{valid?: false} = query ->
                  {:error, query.errors}

                other ->
                  other
              end
            end
          ),
        authorize?: true,
        data: data_field(request_opts, path),
        path: path ++ [:fetch],
        async?: !Keyword.has_key?(request_opts, :initial_data),
        name: "fetch #{inspect(resource)}.#{action.name}"
      )

    process =
      Request.new(
        resource: resource,
        api: api,
        path: path ++ [:data],
        action: action,
        authorize?: false,
        async?: false,
        name: "process #{inspect(resource)}.#{action.name}",
        error_path: error_path,
        data:
          Request.resolve([path ++ [:fetch, :data], path ++ [:fetch, :query]], fn context ->
            query_opts = query_opts(request_opts, context)
            query = get_in(context, path ++ [:fetch, :query])
            fetched_data = get_in(context, path ++ [:fetch, :data])
            data = fetched_data[:results]
            load_paths = query.context.load_paths
            initial_query = query.context.initial_query
            aggregate_value_request_paths = fetched_data[:aggregate_value_request_paths] || []

            if !Enum.empty?(aggregate_value_request_paths) &&
                 !get_in(context, path ++ [:aggregate_values]) do
              {:new_deps, aggregate_value_request_paths}
            else
              case Enum.filter(load_paths, fn path ->
                     !get_in(context, path ++ [:data])
                   end) do
                [] ->
                  data
                  |> Load.attach_loads(get_in(context, path ++ [:fetch, :load]) || %{})
                  |> add_aggregate_values(
                    query.aggregates,
                    query.resource,
                    get_in(context, path ++ [:aggregate_values]) || %{},
                    Map.get(fetched_data, :aggregates_in_query) || []
                  )
                  |> add_calculation_values(
                    query.resource,
                    api,
                    action,
                    error_path,
                    path,
                    Map.get(fetched_data, :ultimate_query) || query,
                    Map.get(fetched_data, :calculations_at_runtime) || [],
                    get_in(context, path ++ [:calculation_results]) || :error,
                    lazy?
                  )
                  |> case do
                    {:ok, values} ->
                      values
                      |> add_tenant(query)
                      |> add_page(
                        action,
                        Map.get(fetched_data, :count),
                        query.sort,
                        initial_query,
                        Keyword.put(query_opts, :page, query.context[:page_opts])
                      )
                      |> add_query(Map.get(fetched_data, :ultimate_query), request_opts)
                      |> unwrap_for_get(get?, query.resource)

                    {:requests, requests} ->
                      {:requests, Enum.map(requests, &{&1, :data})}
                  end

                deps ->
                  {:new_deps, Enum.map(deps, &(&1 ++ [:data]))}
              end
            end
          end)
      )

    [fetch, process]
  end

  defp unwrap_for_get({:ok, [value | _]}, true, _resource), do: {:ok, value}

  defp unwrap_for_get({:ok, []}, true, resource),
    do: {:error, Ash.Error.Query.NotFound.exception(resource: resource)}

  defp unwrap_for_get(other, false, _resource), do: other

  defp query_opts(request_opts, %{authorize?: authorize?, actor: actor, verbose?: verbose?}) do
    request_opts
    |> Keyword.take([
      :page,
      :return_query?,
      :action,
      :tenant,
      :stacktraces?
    ])
    |> Keyword.put(:authorize?, authorize?)
    |> Keyword.put(:actor, actor)
    |> Keyword.put(:verbose?, verbose?)
  end

  defp calculation_or_aggregate?(resource, field) do
    !!(Ash.Resource.Info.aggregate(resource, field) ||
         Ash.Resource.Info.calculation(resource, field))
  end

  defp handle_attribute_multitenancy(query) do
    multitenancy_attribute = Ash.Resource.Info.multitenancy_attribute(query.resource)

    if multitenancy_attribute && query.tenant do
      {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(query.resource)
      attribute_value = apply(m, f, [query.tenant | a])
      Ash.Query.filter(query, [{multitenancy_attribute, attribute_value}])
    else
      query
    end
  end

  defp authorize?(opts) do
    if opts[:authorize?] == false do
      false
    else
      opts[:authorize?] || Keyword.has_key?(opts, :actor)
    end
  end

  defp set_tenant_opt(query, opts) do
    if opts[:tenant] do
      Ash.Query.set_tenant(query, opts[:tenant])
    else
      query
    end
  end

  defp get_action(resource, action) do
    cond do
      action && is_atom(action) ->
        Ash.Resource.Info.action(resource, action)

      action ->
        action

      true ->
        Ash.Resource.Info.primary_action!(resource, :read)
    end
  end

  defp sanitize_opts(opts, authorize?, query) do
    opts
    |> Keyword.put(:authorize?, authorize?)
    |> Keyword.merge(Map.get(query.context, :override_api_params) || [])
  end

  defp engine_opts(opts, action, api, resource, tracer) do
    opts
    |> Keyword.take([:verbose?, :actor, :authorize?, :timeout])
    |> Keyword.put(:transaction?, action.transaction? || opts[:transaction?])
    |> Keyword.put(:default_timeout, Ash.Api.Info.timeout(api))
    |> Keyword.put(:resource, resource)
    |> Keyword.put(:name, "#{inspect(resource)}.#{action.name}")
    |> Keyword.put(:tracer, tracer)
  end

  defp for_read(query, action, opts) do
    if query.__validated_for_action__ == action.name do
      query
    else
      Ash.Query.for_read(query, action.name, %{}, opts)
    end
  end

  defp validate_multitenancy(query, initial_data) do
    if is_nil(Ash.Resource.Info.multitenancy_strategy(query.resource)) ||
         Ash.Resource.Info.multitenancy_global?(query.resource) || query.tenant ||
         initial_data do
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

  @doc false
  def add_page(data, action, count, sort, original_query, opts) do
    if opts[:page] && action.pagination && !opts[:initial_data] do
      to_page(data, action, count, sort, original_query, opts)
    else
      data
    end
  end

  @doc false
  def to_page(data, action, count, sort, original_query, opts) do
    page_opts = opts[:page]

    {data, rest} =
      if opts[:page][:limit] do
        Enum.split(data, opts[:page][:limit])
      else
        {data, []}
      end

    data =
      if page_opts[:before] do
        Enum.reverse(data)
      else
        data
      end

    more? = not Enum.empty?(rest)

    if page_opts[:offset] do
      if action.pagination.keyset? do
        data
        |> Ash.Page.Keyset.data_with_keyset(original_query.resource, sort)
        |> Ash.Page.Offset.new(count, original_query, more?, opts)
      else
        Ash.Page.Offset.new(data, count, original_query, more?, opts)
      end
    else
      cond do
        action.pagination.offset? && action.pagination.keyset? ->
          data
          |> Ash.Page.Keyset.data_with_keyset(original_query.resource, sort)
          |> Ash.Page.Offset.new(count, original_query, more?, opts)

        action.pagination.offset? ->
          Ash.Page.Offset.new(data, count, original_query, more?, opts)

        true ->
          Ash.Page.Keyset.new(data, count, sort, original_query, more?, opts)
      end
    end
  end

  defp filter_requests(query, request_path, opts) do
    authorizing? =
      if opts[:authorize?] == false do
        false
      else
        Keyword.has_key?(opts, :actor) || opts[:authorize?]
      end

    if not Keyword.has_key?(opts, :initial_data) &&
         authorizing? do
      Filter.read_requests(query.api, query.filter, request_path)
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

  defp data_field(request_opts, path) do
    Request.resolve(
      [path ++ [:fetch, :query]],
      fn data ->
        ash_query = get_in(data, path ++ [:fetch, :query])
        actor = data[:actor]
        authorize? = data[:authorize?]

        initial_query = ash_query.context[:initial_query]

        initial_limit = initial_query.context[:initial_limit]
        initial_offset = initial_query.context[:initial_offset]

        used_calculations =
          ash_query.filter
          |> Ash.Filter.used_calculations(
            ash_query.resource,
            [],
            ash_query.calculations,
            ash_query.aggregates
          )

        can_be_in_query? =
          not Keyword.has_key?(request_opts, :initial_data) && !ash_query.action.manual

        authorizing? =
          if request_opts[:authorize?] == false do
            false
          else
            request_opts[:authorize?] || Keyword.has_key?(request_opts, :actor)
          end

        filter_requests = filter_requests(ash_query, path, request_opts)

        {calculations_in_query, calculations_at_runtime} =
          if Ash.DataLayer.data_layer_can?(ash_query.resource, :expression_calculation) &&
               !request_opts[:initial_data] do
            ash_query.calculations
            |> Map.values()
            |> Enum.split_with(fn calculation ->
              Enum.find(used_calculations, &(&1.name == calculation.name)) ||
                calculation.name in Enum.map(ash_query.sort || [], &elem(&1, 0)) ||
                (:erlang.function_exported(calculation.module, :expression, 2) &&
                   !calculation.allow_async?)
            end)
          else
            {[], Map.values(ash_query.calculations)}
          end

        {aggregate_auth_requests, aggregate_value_requests, aggregates_in_query} =
          Aggregate.requests(
            ash_query,
            can_be_in_query?,
            authorizing?,
            calculations_in_query,
            path
          )

        cond do
          match?({:error, _error}, filter_requests) ->
            filter_requests

          # if aggregate auth requests is not empty but we have not received the data from
          # those requests, we should ask the engine to run the aggregate value requests
          !Enum.empty?(aggregate_auth_requests) && !get_in(data, path ++ [:aggregate]) ->
            {:requests, Enum.map(aggregate_auth_requests, &{&1, :authorization_filter})}

          !match?({:ok, []}, filter_requests) && !get_in(data, path ++ [:filter]) ->
            {:ok, filter_requests} = filter_requests
            {:requests, Enum.map(filter_requests, &{&1, :authorization_filter})}

          true ->
            if request_opts[:initial_data] do
              {:ok,
               %{
                 results: request_opts[:initial_data],
                 aggregates_in_query: aggregates_in_query,
                 calculations_at_runtime: calculations_at_runtime,
                 aggregate_value_request_paths:
                   Enum.map(aggregate_value_requests, &(&1.path ++ [:data]))
               },
               %{
                 requests: aggregate_value_requests
               }}
            else
              query =
                initial_query
                |> Ash.Query.unset([
                  :filter,
                  :aggregates,
                  :sort,
                  :limit,
                  :offset,
                  :distinct,
                  :select
                ])
                |> Ash.Query.data_layer_query(only_validate_filter?: true)

              ash_query =
                if ash_query.select || calculations_at_runtime == [] do
                  ash_query
                else
                  to_select =
                    ash_query.resource
                    |> Ash.Resource.Info.attributes()
                    |> Enum.map(& &1.name)

                  Ash.Query.select(ash_query, to_select)
                end

              ash_query =
                Enum.reduce(calculations_at_runtime, ash_query, fn calculation, ash_query ->
                  if calculation.select do
                    Ash.Query.select(ash_query, calculation.select || [])
                  else
                    ash_query
                  end
                end)

              {:ok, filter_requests} = filter_requests

              with %{valid?: true} <- ash_query,
                   {:ok, query} <- query,
                   {:ok, filter} <-
                     filter_with_related(
                       Enum.map(filter_requests, & &1.path),
                       ash_query.filter,
                       data
                     ),
                   filter <- update_aggregate_filters(filter, data, path),
                   {:ok, query} <-
                     Ash.DataLayer.set_context(
                       ash_query.resource,
                       query,
                       Map.put(ash_query.context, :action, ash_query.action)
                     ),
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
                     add_calculations(
                       query,
                       ash_query,
                       calculations_in_query
                     ),
                   {:ok, query} <-
                     Ash.DataLayer.filter(
                       query,
                       filter,
                       ash_query.resource
                     ),
                   {:ok, query} <-
                     Ash.DataLayer.sort(query, ash_query.sort, ash_query.resource),
                   {:ok, query} <-
                     Ash.DataLayer.distinct(query, ash_query.distinct, ash_query.resource),
                   {:ok, count} <-
                     fetch_count(
                       ash_query,
                       query,
                       ash_query.resource,
                       ash_query.action,
                       initial_limit,
                       initial_offset,
                       request_opts
                     ),
                   {:ok, query} <- apply_keyset_filter(query, ash_query),
                   {:ok, query} <-
                     Ash.DataLayer.limit(query, ash_query.limit, ash_query.resource),
                   {:ok, query} <-
                     Ash.DataLayer.offset(query, ash_query.offset, ash_query.resource),
                   {:ok, query} <- set_tenant(query, ash_query),
                   {:ok, results} <-
                     run_query(ash_query, query, %{
                       actor: actor,
                       tenant: ash_query.tenant,
                       authorize?: authorize?,
                       api: ash_query.api
                     }),
                   :ok <- validate_get(results, ash_query.action, ash_query),
                   {:ok, results, after_notifications} <-
                     run_after_action(initial_query, results),
                   {:ok, count} <- maybe_await(count) do
                if request_opts[:return_query?] do
                  ultimate_query =
                    ash_query
                    |> Ash.Query.unset(:filter)
                    |> Ash.Query.filter(filter)

                  {:ok,
                   %{
                     results: results,
                     ultimate_query: ultimate_query,
                     count: count,
                     calculations_at_runtime: calculations_at_runtime,
                     aggregates_in_query: aggregates_in_query,
                     aggregate_value_request_paths:
                       Enum.map(aggregate_value_requests, &(&1.path ++ [:data]))
                   },
                   %{
                     notifications: after_notifications,
                     requests: aggregate_value_requests
                   }}
                else
                  {:ok,
                   %{
                     results: results,
                     count: count,
                     calculations_at_runtime: calculations_at_runtime,
                     aggregates_in_query: aggregates_in_query,
                     aggregate_value_request_paths:
                       Enum.map(aggregate_value_requests, &(&1.path ++ [:data]))
                   },
                   %{
                     notifications: after_notifications,
                     requests: aggregate_value_requests
                   }}
                end
              else
                %{valid?: false} = query ->
                  {:error, query.errors}

                other ->
                  other
              end
            end
        end
      end
    )
  end

  defp apply_keyset_filter(data_layer_query, query) do
    case query.context[:private][:keyset_filter] do
      nil ->
        {:ok, data_layer_query}

      filter ->
        case Ash.Filter.parse(
               query.resource,
               filter,
               query.aggregates,
               query.calculations
             ) do
          {:ok, filter} ->
            Ash.DataLayer.filter(data_layer_query, filter, query.resource)

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp validate_get([_, _ | _] = results, %{get?: true}, query) do
    {:error,
     Ash.Error.Invalid.MultipleResults.exception(
       count: Enum.count(results),
       at_least?: true,
       query: query
     )}
  end

  defp validate_get(_, _, _), do: :ok

  defp update_aggregate_filters(filter, data, path) do
    Filter.update_aggregates(filter, fn aggregate, ref ->
      case get_in(
             data,
             path ++
               [:aggregate, ref.relationship_path] ++
               aggregate.relationship_path ++ [:authorization_filter]
           ) do
        nil ->
          aggregate

        authorization_filter ->
          %{aggregate | authorization_filter: authorization_filter}
      end
    end)
  end

  defp maybe_await(%Task{} = task) do
    case Task.await(task) do
      {:__exception__, e, stacktrace} ->
        reraise e, stacktrace

      other ->
        other
    end
  end

  defp maybe_await(other), do: other

  defp fetch_count(ash_query, query, resource, action, initial_limit, initial_offset, opts) do
    if action.pagination &&
         opts[:page] &&
         (opts[:page][:count] == true ||
            (opts[:page][:count] != false and action.pagination.countable == :by_default)) do
      if Ash.DataLayer.in_transaction?(resource) || !Ash.DataLayer.can?(:async_engine, resource) do
        case do_fetch_count(ash_query, query, initial_limit, initial_offset) do
          {:ok, count} -> {:ok, {:ok, count}}
          {:error, error} -> {:error, error}
        end
      else
        {:ok,
         Ash.Engine.async(
           fn ->
             {:ok, do_fetch_count(ash_query, query, initial_limit, initial_offset)}
           end,
           opts
         )}
      end
    else
      {:ok, {:ok, nil}}
    end
  end

  defp do_fetch_count(ash_query, query, initial_limit, initial_offset) do
    with {:ok, query} <- Ash.DataLayer.limit(query, initial_limit, ash_query.resource),
         {:ok, query} <- Ash.DataLayer.offset(query, initial_offset, ash_query.resource),
         {:ok, %{count: count}} <- run_count_query(ash_query, query) do
      {:ok, count}
    end
  end

  defp run_before_action(query) do
    query.before_action
    |> Enum.reduce({query, []}, fn before_action, {query, notifications} ->
      case before_action.(query) do
        {query, new_notifications} ->
          {query, notifications ++ new_notifications}

        query ->
          {query, notifications}
      end
    end)
  end

  defp run_after_action(query, results) do
    query.after_action
    |> Enum.reduce_while({query, {:ok, results, []}}, fn after_action,
                                                         {query, {:ok, results, notifications}} ->
      case after_action.(query, results) do
        {:ok, results} ->
          {:cont, {query, {:ok, results, notifications}}}

        {:ok, results, new_notifications} ->
          {:cont, {query, {:ok, results, notifications ++ new_notifications}}}

        {:error, error} ->
          {:halt, {query, {:error, error}}}
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

  def page_opts(action, opts) do
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

  defp paginate(starting_query, action, opts) do
    page_opts = page_opts(action, opts)

    cond do
      action.pagination == false && page_opts ->
        {:error, "Pagination is not supported"}

      action.pagination == false ->
        {:ok, starting_query, starting_query, opts[:page]}

      page_opts == false ->
        if action.pagination.required? do
          {:error, PaginationRequired.exception([])}
        else
          {:ok, starting_query, starting_query, false}
        end

      page_opts[:limit] || is_nil(page_opts) || page_opts == [] ->
        case do_paginate(starting_query, action.pagination, opts) do
          {:ok, initial_query, query} ->
            {:ok, initial_query, query, page_opts}

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
          keyset_pagination(query, pagination, opts[:page])

        opts[:page][:offset] ->
          limit_offset_pagination(query, pagination, opts[:page])

        pagination.offset? && pagination.keyset? ->
          keyset_pagination(query, pagination, opts[:page])

        pagination.offset? ->
          limit_offset_pagination(query, pagination, opts[:page])

        true ->
          keyset_pagination(query, pagination, opts[:page])
      end

    case paginated do
      {:ok, initial_query, query} ->
        if opts[:page][:filter] do
          {:ok, Ash.Query.filter(initial_query, ^opts[:page][:filter]),
           Ash.Query.filter(query, ^opts[:page][:filter])}
        else
          {:ok, initial_query, query}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp keyset_pagination(query, pagination, opts) do
    query =
      query.sort
      |> Enum.flat_map(fn
        {%Ash.Query.Calculation{} = calc, _} ->
          [calc]

        _ ->
          []
      end)
      |> Enum.reduce(query, fn
        %{load: nil} = calc, query ->
          Ash.Query.calculate(query, calc.name, {calc.module, calc.opts}, calc.type, calc.context)

        %{load: load}, query ->
          Ash.Query.load(query, load)
      end)

    sorted =
      if Ash.Actions.Sort.sorting_on_identity?(query) do
        query
      else
        Ash.Query.sort(query, Ash.Resource.Info.primary_key(query.resource))
      end

    limited = Ash.Query.limit(sorted, limit(opts[:limit], query.limit, pagination) + 1)

    if opts[:before] || opts[:after] do
      reversed =
        if opts[:before] do
          limited
          |> Ash.Query.unset(:sort)
          |> Ash.Query.sort(Ash.Sort.reverse(limited.sort))
        else
          limited
        end

      after_or_before =
        if opts[:before] do
          :before
        else
          :after
        end

      case Ash.Page.Keyset.filter(
             query.resource,
             opts[:before] || opts[:after],
             sorted.sort,
             after_or_before
           ) do
        {:ok, filter} ->
          {:ok, limited,
           reversed
           |> Ash.Query.set_context(%{
             private: %{
               keyset_filter: filter
             }
           })}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, limited, limited}
    end
  end

  defp limit(page_size, query_limit, pagination) do
    max_page_size = pagination && pagination.max_page_size

    [page_size, query_limit, max_page_size]
    |> Enum.filter(&is_integer/1)
    |> Enum.min()
  end

  defp limit_offset_pagination(query, pagination, opts) do
    limited = Ash.Query.limit(query, limit(opts[:limit], query.limit, pagination) + 1)

    with_offset =
      if opts[:offset] do
        Ash.Query.offset(limited, opts[:offset])
      else
        limited
      end

    {:ok, with_offset, with_offset}
  end

  defp run_query(
         %{
           resource: destination_resource,
           context: %{
             data_layer: %{
               lateral_join_source: {root_data, path}
             }
           }
         } = ash_query,
         query,
         _context
       ) do
    query
    |> Ash.DataLayer.run_query_with_lateral_join(
      root_data,
      destination_resource,
      path
    )
    |> Helpers.select(ash_query)
  end

  defp run_query(%{resource: resource, action: %{manual: nil}} = ash_query, query, _context) do
    query
    |> Ash.DataLayer.run_query(resource)
    |> Helpers.select(ash_query)
  end

  defp run_query(
         %{action: %{manual: {mod, opts}}} = ash_query,
         query,
         context
       ) do
    ash_query
    |> mod.read(query, opts, context)
    |> Helpers.select(ash_query)
  end

  @doc false
  def run_count_query(
        %{
          resource: destination_resource,
          context: %{
            data_layer: %{lateral_join_source: {root_data, path}}
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
          destination_resource,
          path
        )

      {:error, error} ->
        {:error, error}
    end
  end

  def run_count_query(ash_query, query) do
    case Ash.Query.Aggregate.new(ash_query.resource, :count, :count, [], nil, nil) do
      {:ok, aggregate} ->
        Ash.DataLayer.run_aggregate_query(query, [aggregate], ash_query.resource)

      {:error, error} ->
        {:error, error}
    end
  end

  defp calculation_request(
         all_calcs,
         resource,
         api,
         action,
         error_path,
         path,
         results,
         calculation,
         query,
         lazy?
       ) do
    all_calcs = Enum.map(all_calcs, & &1.name)

    resource_load =
      if resource_calculation = Ash.Resource.Info.calculation(resource, calculation.name) do
        List.wrap(resource_calculation.load)
      else
        []
      end

    dependencies =
      query
      |> calculation.module.load(calculation.opts, calculation.context)
      |> List.wrap()
      |> Enum.concat(resource_load)
      |> reject_loaded(results, lazy?)
      |> Ash.Actions.Helpers.validate_calculation_load!(calculation.module)
      |> Enum.map(fn
        {key, _} ->
          key

        key ->
          key
      end)
      |> Enum.uniq()
      |> Enum.filter(fn key ->
        key in all_calcs
      end)
      |> Enum.map(fn key ->
        path ++ [:calculation_results, key, :data]
      end)

    if function_exported?(calculation.module, :calculate, 3) do
      Request.new(
        resource: resource,
        api: api,
        action: action,
        error_path: error_path,
        query: query,
        authorize?: false,
        async?: true,
        data:
          Request.resolve(dependencies, fn data ->
            temp_results =
              Enum.reduce(get_in(data, path ++ [:calculation_results]) || %{}, results, fn {key,
                                                                                            config},
                                                                                           results ->
                add_calc_to_results(results, key, config[:data])
              end)

            case calculation.module.calculate(temp_results, calculation.opts, calculation.context) do
              :unknown ->
                case run_calculation_query(results, [calculation], query) do
                  {:ok, results_with_calc} ->
                    {:ok,
                     %{
                       values:
                         Enum.map(results_with_calc, fn record ->
                           if calculation.load do
                             Map.get(record, calculation.name)
                           else
                             Map.get(record.calculations, calculation.name)
                           end
                         end),
                       load?: not is_nil(calculation.load)
                     }}

                  other ->
                    other
                end

              {:ok, values} ->
                {:ok,
                 %{
                   values: values,
                   load?: not is_nil(calculation.load)
                 }}

              {:error, error} ->
                {:error, error}

              values ->
                {:ok,
                 %{
                   values: values,
                   load?: not is_nil(calculation.load)
                 }}
            end
          end),
        path: path ++ [:calculation_results, calculation.name],
        name: "calculate #{calculation.name}"
      )
    else
      Request.new(
        resource: resource,
        api: api,
        action: action,
        error_path: error_path,
        query: query,
        authorize?: false,
        async?: true,
        data:
          Request.resolve([], fn _ ->
            case run_calculation_query(results, [calculation], query) do
              {:ok, results_with_calc} ->
                {:ok,
                 %{
                   values:
                     Enum.map(results_with_calc, fn record ->
                       if calculation.load do
                         Map.get(record, calculation.name)
                       else
                         Map.get(record.calculations, calculation.name)
                       end
                     end),
                   load?: not is_nil(calculation.load)
                 }}

              other ->
                other
            end
          end),
        path: path ++ [:calculation_results, calculation.name],
        name: "calculate #{calculation.name}"
      )
    end
  end

  defp reject_loaded(loads, results, true) do
    loads
    |> List.wrap()
    |> Enum.reject(fn load ->
      Ash.Resource.loaded?(results, load)
    end)
  end

  defp reject_loaded(loads, _, _) do
    loads
  end

  defp add_calculation_values(
         results,
         resource,
         api,
         action,
         error_path,
         path,
         query,
         calculations,
         :error,
         lazy?
       )
       when calculations != [] do
    {:requests,
     Enum.map(
       calculations,
       &calculation_request(
         calculations,
         resource,
         api,
         action,
         error_path,
         path,
         results,
         &1,
         query,
         lazy?
       )
     )}
  end

  defp add_calculation_values(
         results,
         _resource,
         _api,
         _action,
         _error_path,
         _path,
         _query,
         _calculations,
         calculation_values,
         _lazy?
       ) do
    if calculation_values == :error do
      {:ok, results}
    else
      {:ok,
       Enum.reduce(calculation_values, results, fn {name, config}, results ->
         add_calc_to_results(results, name, config[:data])
       end)}
    end
  end

  defp add_calc_to_results(results, name, config) do
    if config[:load?] do
      :lists.zipwith(
        fn record, value -> Map.put(record, name, value) end,
        results,
        config[:values]
      )
    else
      :lists.zipwith(
        fn record, value ->
          %{record | calculations: Map.put(record.calculations, name, value)}
        end,
        results,
        config[:values]
      )
    end
  end

  defp run_calculation_query(results, calculations, query) do
    pkey = Ash.Resource.Info.primary_key(query.resource)

    pkey_filter =
      results
      |> List.wrap()
      |> Enum.map(fn result ->
        result
        |> Map.take(pkey)
        |> Map.to_list()
      end)

    with query <-
           Ash.Query.unset(query, [:filter, :aggregates, :sort, :limit, :offset, :load, :distinct]),
         query <- Ash.Query.filter(query, ^[or: pkey_filter]),
         {:ok, data_layer_query} <- Ash.Query.data_layer_query(query),
         {:ok, data_layer_query} <-
           add_calculations(data_layer_query, query, calculations) do
      Ash.DataLayer.run_query(
        data_layer_query,
        query.resource
      )
    end
  end

  defp add_aggregate_values(results, aggregates, resource, aggregate_values, aggregates_in_query) do
    keys_to_aggregates =
      Enum.reduce(aggregate_values, %{}, fn
        {_name, %{data: keys_to_values}}, acc ->
          Enum.reduce(keys_to_values, acc, fn {pkey, values}, acc ->
            Map.update(acc, pkey, values, &Map.merge(&1, values))
          end)

        _, acc ->
          acc
      end)

    pkey = Ash.Resource.Info.primary_key(resource)

    loaded =
      aggregates
      |> Enum.map(fn {_, aggregate} -> aggregate.load end)
      |> Enum.reject(&is_nil/1)

    Enum.map(results, fn result ->
      aggregate_values = Map.get(keys_to_aggregates, Map.take(result, pkey), %{})

      aggregate_values =
        aggregates
        |> Enum.reject(fn {name, _aggregate} ->
          Enum.find(aggregates_in_query, &(&1.name == name))
        end)
        |> Enum.reduce(aggregate_values, fn {_, aggregate}, aggregate_values ->
          Map.put_new(aggregate_values, aggregate.name, aggregate.default_value)
        end)

      {top_level, nested} = Map.split(aggregate_values || %{}, loaded)

      Map.merge(%{result | aggregates: Map.merge(result.aggregates, nested)}, top_level)
    end)
  end

  defp add_calculations(data_layer_query, query, calculations_to_add) do
    calculations =
      Enum.reduce_while(calculations_to_add, {:ok, []}, fn calculation, {:ok, calculations} ->
        if Ash.DataLayer.data_layer_can?(query.resource, :expression_calculation) do
          expression = calculation.module.expression(calculation.opts, calculation.context)

          case Ash.Filter.hydrate_refs(expression, %{
                 resource: query.resource,
                 aggregates: query.aggregates,
                 calculations: query.calculations,
                 public?: false
               }) do
            {:ok, expression} ->
              {:cont, {:ok, [{calculation, expression} | calculations]}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        else
          {:halt, {:error, "Expression calculations are not supported"}}
        end
      end)

    case calculations do
      {:ok, calculations} ->
        Ash.DataLayer.add_calculations(
          data_layer_query,
          calculations,
          query.resource
        )

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_aggregates(data_layer_query, query, aggregates_to_add, aggregate_filters) do
    aggregates_to_add =
      Enum.into(aggregates_to_add, %{}, fn aggregate ->
        {aggregate.name, aggregate}
      end)

    aggregates =
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

    Ash.DataLayer.add_aggregates(data_layer_query, Map.values(aggregates), query.resource)
  end

  defp filter_with_related(relationship_filter_paths, filter_expr, data, prefix \\ []) do
    paths_to_global_filter_on =
      filter_expr
      |> Ash.Filter.relationship_paths()
      |> Enum.filter(&((prefix ++ &1) in relationship_filter_paths))

    paths_to_global_filter_on
    |> Enum.reduce_while(
      {:ok, filter_expr},
      fn path, {:ok, filter} ->
        case get_in(data, prefix ++ path ++ [:authorization_filter]) do
          nil ->
            {:cont, {:ok, filter}}

          authorization_filter ->
            path = List.last(path)
            authorization_filter = Ash.Filter.put_at_path(authorization_filter, path)
            add_authorization_filter(filter, authorization_filter)
        end
      end
    )
    |> case do
      {:ok, filter} ->
        {:ok,
         filter
         |> Ash.Filter.map(fn
           %Ash.Query.Exists{path: exists_path, expr: exists_expr} = exists ->
             paths =
               Enum.filter(
                 relationship_filter_paths,
                 &List.starts_with?(&1, prefix ++ exists_path)
               )

             {:ok, new_expr} =
               filter_with_related(paths, exists_expr, data, prefix ++ exists_path)

             {:halt, %{exists | expr: new_expr}}

           other ->
             other
         end)}

      other ->
        other
    end
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
