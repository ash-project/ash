defmodule Ash.Actions.Read.Relationships do
  @moduledoc false
  require Ash.Query
  import Ash.Expr

  def load([], _query, _lazy?, _reuse_values?) do
    {:ok, []}
  end

  def load(record, query, lazy?, reuse_values?) when not is_list(record) do
    case load([record], query, lazy?, reuse_values?) do
      {:ok, [record]} -> {:ok, record}
      {:error, error} -> {:error, error}
    end
  end

  def load(records, %{load: load}, _lazy?, _reuse_values?) when load in [%{}, [], nil] do
    {:ok, records}
  end

  def load(records, query, lazy?, reuse_values?) do
    query.load
    |> with_related_queries(query, records, lazy?)
    |> fetch_related_records(records, reuse_values?)
    |> attach_related_records(records)
  end

  defp attach_related_records(relationships_queries_and_related_records, records) do
    Enum.reduce_while(relationships_queries_and_related_records, {:ok, records}, fn
      {relationship, related_query, {:ok, related_records}}, {:ok, records} ->
        {:cont,
         {:ok, do_attach_related_records(records, relationship, related_records, related_query)}}

      {relationship, _related_query, {:error, error}}, _ ->
        {:halt, {:error, Ash.Error.set_path(error, relationship.name)}}

      {:__exception__, error, stacktrace}, _ ->
        {:halt, {:error, Ash.Error.to_ash_error(error, stacktrace)}}
    end)
  end

  defp fetch_related_records(batch, records, reuse_values?, acc \\ [])

  defp fetch_related_records([], _records, _reuse_values?, acc) do
    Enum.map(acc, fn
      {a, b, %Task{} = task} ->
        {a, b, Task.await(task, :infinity)}

      %Task{} = task ->
        Task.await(task, :infinity)

      other ->
        other
    end)
  end

  defp fetch_related_records([first | rest], records, reuse_values?, acc) do
    result =
      case first do
        {relationship, {:lazy, query}} ->
          {relationship, {:lazy, query},
           lazy_related_records(records, relationship, query, Enum.empty?(rest), reuse_values?)}

        {relationship, %{valid?: true} = related_query} ->
          do_fetch_related_records(records, relationship, related_query, Enum.empty?(rest))

        {relationship, %{errors: errors} = related_query} ->
          {relationship, related_query, {:error, errors}}
      end

    fetch_related_records(rest, records, reuse_values?, [result | acc])
  end

  defp lazy_related_records(records, relationship, related_query, last?, reuse_values?) do
    primary_key = Ash.Resource.Info.primary_key(relationship.source)

    related_records_with_lazy_join_source =
      Enum.flat_map(records, fn record ->
        record_pkey = Map.take(record, primary_key)

        record
        |> Map.get(relationship.name)
        |> case do
          %Ash.NotLoaded{} ->
            []

          %Ash.ForbiddenField{} ->
            []

          record_or_records ->
            record_or_records
        end
        |> List.wrap()
        |> Enum.map(&Ash.Resource.set_metadata(&1, %{lazy_join_source: record_pkey}))
      end)

    Ash.Actions.Read.AsyncLimiter.async_or_inline(
      related_query,
      Ash.Context.to_opts(related_query.context),
      last?,
      fn ->
        Ash.load(related_records_with_lazy_join_source, related_query,
          lazy?: true,
          reuse_values?:
            reuse_values? || related_query.context[:private][:reuse_values?] || false,
          domain: related_query.domain,
          actor: related_query.context.private[:actor],
          tenant: related_query.tenant,
          authorize?: related_query.context.private[:authorize?]
        )
      end
    )
  end

  defp with_related_queries(load, query, records, lazy?) do
    Enum.map(load, fn {relationship_name, related_query} ->
      lazy? = lazy? || related_query.context[:private][:lazy?]

      if lazy? && Ash.Resource.loaded?(records, relationship_name, lists: :any) do
        relationship = Ash.Resource.Info.relationship(query.resource, relationship_name)

        related_query =
          case related_query do
            [] -> Ash.Query.new(relationship.destination)
            query -> query
          end

        domain = Ash.Domain.Info.related_domain(related_query, relationship, query.domain)

        {read_action_name, arguments} =
          case related_query do
            %Ash.Query{action: %{name: name}, arguments: arguments} ->
              {name, arguments}

            _ ->
              read_action_name =
                relationship.read_action ||
                  Ash.Resource.Info.primary_action!(relationship.destination, :read).name

              {read_action_name, %{}}
          end

        {relationship,
         {:lazy,
          related_query
          |> Map.put(
            :domain,
            Ash.Domain.Info.related_domain(related_query, relationship, query.domain)
          )
          |> Ash.Query.set_tenant(query.tenant)
          |> Ash.Query.set_context(%{
            private: %{async_limiter: query.context[:private][:async_limiter]}
          })
          |> Ash.Query.for_read(
            read_action_name,
            arguments,
            domain: domain,
            authorize?: query.context[:private][:authorize?],
            actor: query.context[:private][:actor],
            tracer: query.context[:private][:tracer]
          )}}
      else
        related_query(relationship_name, records, related_query, query)
      end
    end)
  end

  @doc false
  def related_query(relationship_name, records, related_query, query) do
    relationship = Ash.Resource.Info.relationship(query.resource, relationship_name)

    {read_action_name, arguments} =
      case related_query do
        %Ash.Query{action: %{name: name}, arguments: arguments} ->
          {name, arguments}

        _ ->
          read_action_name =
            relationship.read_action ||
              Ash.Resource.Info.primary_action!(relationship.destination, :read).name

          {read_action_name, %{}}
      end

    domain = Ash.Domain.Info.related_domain(related_query, relationship, query.domain)

    related_query =
      related_query
      |> Ash.Query.set_context(%{
        private: %{async_limiter: query.context[:private][:async_limiter]}
      })
      |> Ash.Query.set_tenant(query.tenant)
      |> Ash.Query.for_read(
        read_action_name,
        arguments,
        domain: domain,
        authorize?: query.context[:private][:authorize?],
        actor: query.context[:private][:actor],
        tracer: query.context[:private][:tracer]
      )
      |> Ash.Query.sort(relationship.sort)
      |> Ash.Query.do_filter(relationship.filter,
        parent_stack: List.wrap(query.context[:parent_stack]) ++ [query.resource]
      )
      |> Ash.Query.set_context(relationship.context)
      |> Ash.Query.set_context(%{private: %{loading_relationship?: true}})
      |> hydrate_refs(query.context[:private][:actor], relationship.source)
      |> with_lateral_join_query(query, relationship, records)

    if !related_query.context[:data_layer][:lateral_join_source] &&
         related_query.distinct not in [[], nil] do
      raise ArgumentError, message: "Cannot yet use `distinct` when loading related records"
    end

    {relationship, related_query}
  end

  defp with_lateral_join_query(related_query, source_query, relationship, records) do
    if lateral_join?(related_query, source_query, relationship, records) do
      lateral_join_source_path =
        if relationship.type == :many_to_many do
          join_relationship =
            Ash.Resource.Info.relationship(source_query.resource, relationship.join_relationship)

          through_query =
            relationship.through
            |> Ash.Query.for_read(
              join_relationship.read_action ||
                Ash.Resource.Info.primary_action!(relationship.through, :read).name,
              %{},
              authorize?: source_query.context[:private][:authorize?],
              actor: source_query.context[:private][:actor],
              tenant: source_query.tenant,
              tracer: source_query.context[:private][:tracer],
              domain: join_relationship.domain || related_query.domain
            )
            |> Ash.Query.do_filter(join_relationship.filter)
            |> Ash.Query.set_context(%{
              accessing_from: %{source: relationship.source, name: relationship.join_relationship}
            })
            |> Ash.Query.select([
              relationship.source_attribute_on_join_resource,
              relationship.destination_attribute_on_join_resource
            ])
            |> hydrate_refs(source_query.context[:private][:actor], relationship.source)

          if source_query.context[:private][:authorize?] do
            case Ash.can(
                   through_query,
                   source_query.context[:private][:actor],
                   return_forbidden_error?: true,
                   pre_flight?: false,
                   alter_source?: true,
                   run_queries?: false,
                   base_query: through_query
                 ) do
              {:ok, true} ->
                {:ok,
                 [
                   {clear_lateral_join_source(source_query), relationship.source_attribute,
                    relationship.source_attribute_on_join_resource, relationship},
                   {clear_lateral_join_source(through_query),
                    relationship.destination_attribute_on_join_resource,
                    relationship.destination_attribute, join_relationship}
                 ]}

              {:ok, true, authorized_through_query} ->
                {:ok,
                 [
                   {clear_lateral_join_source(source_query), relationship.source_attribute,
                    relationship.source_attribute_on_join_resource, relationship},
                   {clear_lateral_join_source(authorized_through_query),
                    relationship.destination_attribute_on_join_resource,
                    relationship.destination_attribute, join_relationship}
                 ]}

              {:ok, false, error} ->
                {:error, Ash.Error.set_path(error, join_relationship.name)}

              {:error, error} ->
                {:error, Ash.Error.set_path(error, join_relationship.name)}
            end
          else
            {:ok,
             [
               {clear_lateral_join_source(source_query), relationship.source_attribute,
                relationship.source_attribute_on_join_resource, relationship},
               {clear_lateral_join_source(through_query),
                relationship.destination_attribute_on_join_resource,
                relationship.destination_attribute, join_relationship}
             ]}
          end
        else
          {:ok,
           [
             {clear_lateral_join_source(source_query), relationship.source_attribute,
              relationship.destination_attribute, relationship}
           ]}
        end

      case lateral_join_source_path do
        {:ok, lateral_join_source_path} ->
          Ash.Query.set_context(related_query, %{
            data_layer: %{
              lateral_join_source: {records, lateral_join_source_path}
            }
          })

        {:error, error} ->
          Ash.Query.add_error(related_query, error)
      end
    else
      related_query
    end
  end

  defp hydrate_refs(query, actor, parent) do
    query.filter
    |> Ash.Expr.fill_template(actor, %{}, query.context)
    |> Ash.Filter.hydrate_refs(%{
      resource: query.resource,
      parent_stack: [parent],
      public?: false
    })
    |> case do
      {:ok, hydrated} ->
        %{query | filter: hydrated}

      {:error, error} ->
        Ash.Query.add_error(query, error)
    end
  end

  defp clear_lateral_join_source(source_query) do
    Ash.Query.unset(source_query, [:load, :select, :sort])
  end

  defp do_fetch_related_records(
         records,
         %{manual: {module, opts}} = relationship,
         related_query,
         last?
       ) do
    Ash.Actions.Read.AsyncLimiter.async_or_inline(
      related_query,
      Ash.Context.to_opts(related_query.context),
      last?,
      fn ->
        result =
          module.load(records, opts, %Ash.Resource.ManualRelationship.Context{
            relationship: relationship,
            query:
              related_query
              |> Ash.Query.sort(relationship.sort)
              |> Ash.Query.do_filter(relationship.filter)
              |> Map.put(:load, [])
              |> Ash.Query.set_context(%{
                accessing_from: %{source: relationship.source, name: relationship.name}
              }),
            actor: related_query.context[:private][:actor],
            authorize?: related_query.context[:private][:authorize?],
            domain: related_query.domain,
            tenant: related_query.tenant
          })
          |> case do
            {:ok, records} ->
              records
              |> Enum.flat_map(fn {key, value} ->
                value
                |> List.wrap()
                |> Enum.map(&Ash.Resource.put_metadata(&1, :manual_key, key))
              end)
              |> Ash.load(related_query,
                domain: related_query.domain,
                actor: related_query.context[:private][:actor],
                authorize?: related_query.context[:private][:authorize?],
                tenant: related_query.tenant
              )
              |> case do
                {:ok, results} ->
                  {:ok, regroup_manual_results(results, relationship)}

                {:error, error} ->
                  {:error, error}
              end

            {:error, error} ->
              {:error, error}
          end

        {relationship, related_query, result}
      end
    )
  end

  defp do_fetch_related_records(
         _records,
         %{no_attributes?: true} = relationship,
         related_query,
         last?
       ) do
    Ash.Actions.Read.AsyncLimiter.async_or_inline(
      related_query,
      Ash.Context.to_opts(related_query.context),
      last?,
      fn ->
        result =
          related_query
          |> select_destination_attribute(relationship)
          |> Ash.Query.set_context(%{
            accessing_from: %{source: relationship.source, name: relationship.name}
          })
          |> Ash.Actions.Read.unpaginated_read()

        {relationship, related_query, result}
      end
    )
  end

  defp do_fetch_related_records(
         _records,
         relationship,
         %{context: %{data_layer: %{lateral_join_source: {_, _}}}} = related_query,
         last?
       ) do
    Ash.Actions.Read.AsyncLimiter.async_or_inline(
      related_query,
      Ash.Context.to_opts(related_query.context),
      last?,
      fn ->
        result =
          related_query
          |> select_destination_attribute(relationship)
          |> Ash.Query.set_context(%{
            accessing_from: %{source: relationship.source, name: relationship.name}
          })
          |> Ash.Actions.Read.read_and_return_unpaged()

        {relationship, related_query, result}
      end
    )
  end

  defp do_fetch_related_records(
         records,
         %{type: :many_to_many} = relationship,
         related_query,
         last?
       ) do
    record_ids =
      Enum.map(records, fn record ->
        Map.get(record, relationship.source_attribute)
      end)

    join_relationship =
      Ash.Resource.Info.relationship(relationship.source, relationship.join_relationship)

    join_query =
      relationship.through
      |> Ash.Query.filter(^ref(relationship.source_attribute_on_join_resource) in ^record_ids)
      |> Ash.Query.do_filter(join_relationship.filter)
      |> Ash.Query.set_context(%{
        accessing_from: %{source: relationship.source, name: relationship.join_relationship}
      })
      |> Ash.Query.select(
        [
          relationship.source_attribute_on_join_resource,
          relationship.destination_attribute_on_join_resource
        ] ++ Ash.Resource.Info.primary_key(join_relationship.destination)
      )
      |> Ash.Query.for_read(
        join_relationship.read_action ||
          Ash.Resource.Info.primary_action!(relationship.through, :read).name,
        %{},
        authorize?: related_query.context[:private][:authorize?],
        actor: related_query.context[:private][:actor],
        tracer: related_query.context[:private][:tracer],
        tenant: related_query.tenant,
        domain:
          Ash.Domain.Info.related_domain(
            related_query,
            join_relationship,
            related_query.domain
          )
      )

    Ash.Actions.Read.AsyncLimiter.async_or_inline(
      related_query,
      Ash.Context.to_opts(related_query.context),
      last?,
      fn ->
        case Ash.Actions.Read.unpaginated_read(join_query, nil) do
          {:ok, join_records} ->
            {join_id_mapping, destination_ids} =
              Enum.reduce(join_records, {%{}, MapSet.new()}, fn join_record,
                                                                {mapping, destination_ids} ->
                destination_value =
                  Map.get(join_record, relationship.destination_attribute_on_join_resource)

                source_value =
                  Map.get(join_record, relationship.source_attribute_on_join_resource)

                new_destination_ids = MapSet.put(destination_ids, destination_value)

                new_mapping =
                  Map.update(
                    mapping,
                    destination_value,
                    [
                      source_value
                    ],
                    &[source_value | &1]
                  )

                {new_mapping, new_destination_ids}
              end)

            related_query =
              if related_query.page do
                if Ash.Actions.Sort.sorting_on_identity?(related_query) do
                  related_query
                else
                  Ash.Query.sort(
                    related_query,
                    Ash.Resource.Info.primary_key(related_query.resource)
                  )
                end
              else
                related_query
              end

            related_query
            |> select_destination_attribute(relationship)
            |> Ash.Query.sort(relationship.sort)
            |> Ash.Query.do_filter(relationship.filter)
            |> Ash.Query.filter(^ref(relationship.destination_attribute) in ^destination_ids)
            |> Ash.Query.set_context(%{
              accessing_from: %{source: relationship.source, name: relationship.name}
            })
            |> Map.put(:page, nil)
            |> Ash.Actions.Read.unpaginated_read()
            |> case do
              {:ok, records} ->
                {relationship, related_query,
                 {:ok,
                  Enum.flat_map(records, fn record ->
                    Enum.map(
                      join_id_mapping[Map.get(record, relationship.destination_attribute)] || [],
                      fn lateral_join_source ->
                        Map.put(
                          record,
                          :__lateral_join_source__,
                          lateral_join_source
                        )
                      end
                    )
                  end)}}

              {:error, error} ->
                {relationship, related_query, {:error, error}}
            end

          {:error, error} ->
            {relationship, related_query, {:error, error}}
        end
      end
    )
  end

  defp do_fetch_related_records(records, relationship, related_query, last?) do
    destination_attributes = Enum.map(records, &Map.get(&1, relationship.source_attribute))

    Ash.Actions.Read.AsyncLimiter.async_or_inline(
      related_query,
      Ash.Context.to_opts(related_query.context),
      last?,
      fn ->
        result =
          related_query
          |> select_destination_attribute(relationship)
          |> Ash.Query.filter(^ref(relationship.destination_attribute) in ^destination_attributes)
          |> Ash.Query.unset([:limit, :offset, :distinct, :distinct_sort, :page])
          |> Ash.Query.set_context(%{
            accessing_from: %{source: relationship.source, name: relationship.name}
          })
          |> Ash.Actions.Read.unpaginated_read()

        {relationship, related_query, result}
      end
    )
  end

  defp regroup_manual_results(records, %{cardinality: :many}) do
    Enum.group_by(records, & &1.__metadata__.manual_key, &delete_manual_key/1)
  end

  defp regroup_manual_results(records, %{cardinality: :one}) do
    Map.new(records, &{&1.__metadata__.manual_key, delete_manual_key(&1)})
  end

  defp delete_manual_key(record) do
    Map.update!(record, :__metadata__, &Map.delete(&1, :manual_key))
  end

  defp select_destination_attribute(related_query, relationship) do
    if Map.get(relationship, :no_attributes?) ||
         (Map.get(relationship, :manual) &&
            !Ash.Resource.Info.attribute(
              relationship.destination,
              relationship.destination_attribute
            )) do
      related_query
    else
      Ash.Query.ensure_selected(related_query, [relationship.destination_attribute])
    end
  end

  defp do_attach_related_records(
         [%resource{} | _] = records,
         relationship,
         related_records,
         {:lazy, _related_query}
       ) do
    pkey = Ash.Resource.Info.primary_key(resource)

    Enum.map(records, fn record ->
      record_pkey = Map.take(record, pkey)
      related = Enum.filter(related_records, &(&1.__metadata__.lazy_join_source == record_pkey))

      related =
        case relationship.cardinality do
          :many ->
            related

          :one ->
            Enum.at(related, 0)
        end

      case Map.get(record, relationship.name) do
        %Ash.ForbiddenField{} -> record
        %Ash.NotLoaded{} -> record
        _ -> Map.put(record, relationship.name, related)
      end
    end)
  end

  defp do_attach_related_records(
         [_ | _] = records,
         relationship,
         %Ash.Page.Unpaged{} = unpaged,
         %{context: %{data_layer: %{lateral_join_source: {_records, lateral_join_source_path}}}} =
           related_query
       ) do
    %Ash.Page.Unpaged{
      related_records: related_records,
      opts: opts
    } = unpaged

    attach_fun =
      if relationship.cardinality == :many do
        fn record, relationship_name, value ->
          count_key =
            Ash.Actions.Read.paginated_relationship_count_aggregate_name(relationship.name)

          # Retrieve the count (if present) while deleting it from the record aggregates
          {count, record} = pop_in(record.aggregates[count_key])

          # We scope the lateral join to the specific record, so that next runs of rerun
          # just fetch the entries related to this record
          related_query =
            Ash.Query.set_context(related_query, %{
              data_layer: %{lateral_join_source: {[record], lateral_join_source_path}}
            })

          page =
            Ash.Actions.Read.to_page(
              value,
              related_query.action,
              count,
              related_query.sort,
              related_query,
              opts
            )

          attach_related(record, relationship_name, page)
        end
      else
        &attach_related/3
      end

    attach_lateral_join_related_records(
      records,
      relationship,
      related_records,
      attach_fun
    )
  end

  defp do_attach_related_records(
         [_ | _] = records,
         relationship,
         related_records,
         %{context: %{data_layer: %{lateral_join_source: {_, _}}}}
       ) do
    attach_lateral_join_related_records(records, relationship, related_records)
  end

  defp do_attach_related_records(
         records,
         %{type: :many_to_many} = relationship,
         related_records,
         related_query
       ) do
    do_attach_related_records(
      records,
      relationship,
      related_records,
      Ash.Query.set_context(related_query, %{data_layer: %{lateral_join_source: {nil, nil}}})
    )
    |> Enum.map(fn row ->
      Map.update!(row, relationship.name, fn related_records ->
        apply_runtime_query_operations(
          row,
          relationship,
          related_records,
          related_query
        )
      end)
    end)
  end

  defp do_attach_related_records(
         [%resource{} | _] = records,
         %{manual: {_module, _opts}} = relationship,
         map,
         _related_query
       ) do
    default =
      case relationship.cardinality do
        :one ->
          nil

        :many ->
          []
      end

    if Ash.Resource.Info.primary_key_simple_equality?(resource) do
      pkey = Ash.Resource.Info.primary_key(resource)

      single_match? =
        case pkey do
          [_] -> true
          _ -> false
        end

      Enum.map(records, fn record ->
        value =
          if single_match? do
            case Map.fetch(map, Map.get(record, Enum.at(pkey, 0))) do
              {:ok, value} -> {:ok, value}
              :error -> Map.fetch(map, Map.take(record, pkey))
            end
          else
            Map.fetch(map, Map.take(record, pkey))
          end

        case value do
          {:ok, result} ->
            Map.put(record, relationship.name, result)

          :error ->
            Map.put(record, relationship.name, default)
        end
      end)
    else
      pkey = Ash.Resource.Info.primary_key(resource)

      case pkey do
        [pkey_key] ->
          Enum.map(records, fn record ->
            pkey_values = Map.take(record, pkey)

            value =
              Enum.find_value(map, fn {key, value} ->
                if is_map(key) do
                  if resource.primary_key_matches?(key, pkey_values) do
                    {:ok, value}
                  end
                else
                  if resource.primary_key_matches?(%{pkey_key => key}, pkey_values) do
                    {:ok, value}
                  end
                end
              end) || :error

            case value do
              {:ok, result} ->
                Map.put(record, relationship.name, result)

              :error ->
                Map.put(record, relationship.name, default)
            end
          end)

        _pkeys ->
          Enum.map(records, fn record ->
            pkey_values = Map.take(record, pkey)

            value =
              Enum.find_value(map, fn {key, value} ->
                if resource.primary_key_matches?(key, pkey_values) do
                  {:ok, value}
                end
              end) || :error

            case value do
              {:ok, result} ->
                Map.put(record, relationship.name, result)

              :error ->
                Map.put(record, relationship.name, default)
            end
          end)
      end
    end
  end

  defp do_attach_related_records(
         records,
         %{no_attributes?: true} = relationship,
         related_records,
         _related_query
       ) do
    Enum.map(records, fn record ->
      Map.put(record, relationship.name, related_records)
    end)
  end

  defp do_attach_related_records(records, relationship, related_records, related_query) do
    attribute = Ash.Resource.Info.attribute(relationship.source, relationship.source_attribute)
    simple_equality? = Ash.Type.simple_equality?(attribute.type)

    related =
      if simple_equality? do
        if relationship.cardinality == :one do
          Map.new(
            Enum.reverse(related_records),
            &{Map.get(&1, relationship.destination_attribute), &1}
          )
        else
          Enum.group_by(related_records, &Map.get(&1, relationship.destination_attribute))
        end
      else
        related_records
      end

    default =
      if relationship.cardinality == :one do
        nil
      else
        []
      end

    if simple_equality? do
      Enum.map(records, fn record ->
        value = Map.get(record, relationship.source_attribute)

        if relationship.cardinality == :many do
          Map.put(
            record,
            relationship.name,
            apply_runtime_query_operations(
              record,
              relationship,
              Map.get(related, value) || default,
              related_query
            )
          )
        else
          Map.put(
            record,
            relationship.name,
            apply_runtime_query_operations(
              record,
              relationship,
              Enum.at(List.wrap(Map.get(related, value) || default), 0),
              related_query
            )
          )
        end
      end)
    else
      Enum.map(records, fn record ->
        value = Map.get(record, relationship.source_attribute)

        if relationship.cardinality == :many do
          related
          |> Enum.filter(fn result ->
            destination_value = Map.get(result, relationship.destination_attribute)

            Ash.Type.equal?(attribute.type, value, destination_value)
          end)
          |> then(fn result ->
            result =
              apply_runtime_query_operations(record, relationship, {:ok, result}, related_query)

            put_result(record, result, relationship.name, default)
          end)
        else
          related
          |> Enum.find_value(:error, fn result ->
            destination_value = Map.get(result, relationship.destination_attribute)

            if Ash.Type.equal?(attribute.type, value, destination_value) do
              {:ok, result}
            end
          end)
          |> then(fn result ->
            result =
              apply_runtime_query_operations(record, relationship, result, related_query)

            put_result(record, result, relationship.name, default)
          end)
        end
      end)
    end
  end

  defp apply_runtime_query_operations(record, relationship, {:ok, value}, related_query) do
    {:ok, apply_runtime_query_operations(record, relationship, value, related_query)}
  end

  defp apply_runtime_query_operations(_record, _relationship, :error, _related_query), do: :error

  defp apply_runtime_query_operations(_record, _relationship, empty, _related_query)
       when empty in [nil, []],
       do: empty

  defp apply_runtime_query_operations(record, relationship, value, related_query)
       when not is_list(value) do
    record |> apply_runtime_query_operations(relationship, [value], related_query) |> Enum.at(0)
  end

  defp apply_runtime_query_operations(record, relationship, value, related_query) do
    value
    |> apply_runtime_offset(related_query)
    |> apply_runtime_limit(related_query)
    |> apply_runtime_pagination(record, relationship, related_query)
  end

  defp apply_runtime_pagination(
         value,
         source_record,
         relationship,
         %{page: page_opts} = related_query
       )
       when not is_nil(page_opts) do
    pagination_type =
      cond do
        related_query.action.pagination.keyset? && (page_opts[:before] || page_opts[:after]) ->
          :keyset

        page_opts[:offset] ->
          :offset

        related_query.action.pagination.offset? && related_query.action.pagination.keyset? ->
          :offset

        related_query.action.pagination.offset? ->
          :offset

        true ->
          :keyset
      end

    count = Map.get(source_record.aggregates, "__paginated_#{relationship.name}_count__")

    limit =
      page_opts[:limit] ||
        related_query.action.pagination.default_limit ||
        related_query.action.pagination.max_page_size || 250

    value =
      if pagination_type == :keyset && (page_opts[:before] || page_opts[:after]) do
        after_or_before =
          if page_opts[:before] do
            :before
          else
            :after
          end

        # Apparently dialyzer hated me trying to write this as a *with* statement???
        # giving up and making it a case statement because I don't have time for this shit

        case Ash.Page.Keyset.filter(
               related_query,
               page_opts[:before] || page_opts[:after],
               related_query.sort,
               after_or_before
             ) do
          {:ok, filter} ->
            case Ash.Query.do_filter(related_query.resource, filter) do
              %{valid?: true} = query ->
                case Ash.Filter.Runtime.filter_matches(
                       related_query.domain,
                       value,
                       query.filter,
                       tenant: query.tenant,
                       actor: query.actor
                     ) do
                  {:ok, value} ->
                    value

                  {:error, error} ->
                    raise Ash.Error.to_ash_error(error)
                end

              %{valid?: false, errors: errors} ->
                raise Ash.Error.to_ash_error(errors)
            end

          {:error, error} ->
            raise Ash.Error.to_ash_error(error)
        end
      else
        value
      end

    {value, rest} =
      value
      |> Enum.drop(page_opts[:offset] || 0)
      |> Enum.split(limit)

    more? = !Enum.empty?(rest)

    case pagination_type do
      :offset ->
        Ash.Page.Offset.new(value, count, related_query, more?, [])

      :keyset ->
        Ash.Page.Keyset.new(value, count, related_query.sort, related_query, more?, [])
    end
  end

  defp apply_runtime_pagination(value, _, _, _) do
    value
  end

  defp apply_runtime_offset(value, %{offset: nil}), do: value
  defp apply_runtime_offset(value, %{offset: offset}), do: Enum.drop(value, offset)

  defp apply_runtime_limit(value, %{limit: nil}), do: value
  defp apply_runtime_limit(value, %{limit: limit}), do: Enum.take(value, limit)

  defp put_result(record, {:ok, match}, key, _default) do
    Map.put(
      record,
      key,
      match
    )
  end

  defp put_result(record, :error, key, default) do
    Map.put(record, key, default)
  end

  defp attach_related(record, relationship_name, value) do
    Map.put(record, relationship_name, value)
  end

  defp attach_lateral_join_related_records(
         [%resource{} | _] = records,
         relationship,
         related_records,
         attach_fun \\ &attach_related/3
       ) do
    source_attribute =
      if Map.get(relationship, :no_attributes?) do
        nil
      else
        Ash.Resource.Info.attribute(relationship.source, relationship.source_attribute)
      end

    pkey_simple_equality? = Ash.Resource.Info.primary_key_simple_equality?(relationship.source)

    source_attribute_simple_equality? =
      is_nil(source_attribute) || Ash.Type.simple_equality?(source_attribute.type)

    primary_key = Ash.Resource.Info.primary_key(resource)

    if pkey_simple_equality? && source_attribute_simple_equality? do
      values =
        if relationship.cardinality == :many do
          Enum.group_by(related_records, & &1.__lateral_join_source__)
        else
          Map.new(Enum.reverse(related_records), &{&1.__lateral_join_source__, &1})
        end

      default =
        if relationship.cardinality == :many do
          []
        else
          nil
        end

      if source_attribute do
        Enum.map(records, fn record ->
          with :error <- Map.fetch(values, Map.take(record, primary_key)),
               :error <- Map.fetch(values, Map.get(record, relationship.source_attribute)) do
            attach_fun.(record, relationship.name, default)
          else
            {:ok, value} ->
              attach_fun.(record, relationship.name, value)
          end
        end)
      else
        Enum.map(records, fn record ->
          case Map.fetch(values, Map.take(record, primary_key)) do
            {:ok, value} ->
              attach_fun.(record, relationship.name, value)

            :error ->
              attach_fun.(record, relationship.name, default)
          end
        end)
      end
    else
      Enum.map(records, fn record ->
        func =
          if relationship.cardinality == :one do
            :find
          else
            :filter
          end

        related =
          apply(Enum, func, [
            related_records,
            fn related_record ->
              if is_map(related_record.__lateral_join_source__) do
                resource.primary_key_matches?(record, related_record.__lateral_join_source__)
              else
                Ash.Type.equal?(
                  source_attribute.type,
                  related_record.__lateral_join_source__,
                  Map.get(record, relationship.source_attribute)
                )
              end
            end
          ])

        attach_fun.(record, relationship.name, related)
      end)
    end
  end

  defp lateral_join?(%{action: action} = query, source_query, relationship, source_data) do
    if action.manual do
      raise_if_parent_expr!(relationship, "manual actions")
      false
    else
      {offset, limit} = offset_and_limit(query)

      resources =
        [relationship.source, Map.get(relationship, :through), relationship.destination]
        |> Enum.reject(&is_nil/1)

      has_distinct? = query.distinct not in [[], nil]
      has_page? = query.page not in [nil, false]

      cond do
        !Ash.DataLayer.data_layer_can?(
          relationship.source,
          {:lateral_join, resources}
        ) ->
          false

        is_many_to_many_not_unique_on_join?(relationship, query, source_query) ->
          raise_if_parent_expr!(
            relationship,
            "many to many relationships that don't have unique constraints on their join resource attributes"
          )

          false

        Map.get(relationship, :from_many?) ->
          true

        limit == 1 && is_nil(relationship.context) && is_nil(relationship.filter) &&
          is_nil(relationship.sort) && relationship.cardinality != :many ->
          has_parent_expr?(relationship)

        limit == 1 && (source_data == :unknown || Enum.count_until(source_data, 2) == 1) &&
            relationship.type != :many_to_many ->
          has_parent_expr?(relationship)

        has_parent_expr?(relationship) ->
          true

        relationship.type == :many_to_many &&
            Ash.DataLayer.prefer_lateral_join_for_many_to_many?(
              Ash.DataLayer.data_layer(relationship.source)
            ) ->
          true

        limit || offset || has_distinct? || has_page? ->
          true

        true ->
          false
      end
    end
  end

  defp raise_if_parent_expr!(relationship, reason) do
    if has_parent_expr?(relationship) do
      raise ArgumentError, "Found `parent_expr` in unsupported context: #{reason}"
    end
  end

  @doc false
  def has_parent_expr?(%{destination: destination, filter: filter, sort: sort, context: context}) do
    {:ok, sort} = Ash.Actions.Sort.process(destination, sort, %{}, context)
    do_has_parent_expr?(filter) || has_parent_expr_in_sort?(sort)
  end

  defp has_parent_expr_in_sort?(sort) do
    sort
    |> List.wrap()
    |> Enum.any?(fn
      atom when is_atom(atom) ->
        false

      {atom, _} when is_atom(atom) ->
        false

      %Ash.Query.Calculation{} = calculation ->
        expression = calculation.module.expression(calculation.opts, calculation.context)
        do_has_parent_expr?(expression)

      {%Ash.Query.Calculation{} = calculation, _} ->
        expression = calculation.module.expression(calculation.opts, calculation.context)
        do_has_parent_expr?(expression)
    end)
  end

  @doc false
  def do_has_parent_expr?(filter, depth \\ 0) do
    not is_nil(
      Ash.Filter.find(filter, fn
        %Ash.Query.Call{name: :parent, args: [expr]} ->
          if depth == 0 do
            true
          else
            do_has_parent_expr?(expr, depth - 1)
          end

        %Ash.Query.Exists{expr: expr} ->
          do_has_parent_expr?(expr, depth + 1)

        %Ash.Query.Parent{expr: expr} ->
          if depth == 0 do
            true
          else
            do_has_parent_expr?(expr, depth - 1)
          end

        _ ->
          false
      end)
    )
  end

  defp offset_and_limit(query) do
    if query.offset == 0 do
      {nil, query.limit}
    else
      {query.offset, query.limit}
    end
  end

  defp is_many_to_many_not_unique_on_join?(
         %{type: :many_to_many} = relationship,
         query,
         source_query
       ) do
    keys =
      [
        relationship.source_attribute_on_join_resource,
        relationship.destination_attribute_on_join_resource
      ]

    join_keys =
      if query.tenant && source_query.tenant &&
           Ash.Resource.Info.multitenancy_strategy(relationship.through) == :attribute do
        attr = Ash.Resource.Info.multitenancy_attribute(relationship.through)
        [attr | keys]
      else
        keys
      end

    primary_key_is_join_keys? =
      Enum.all?(
        Ash.Resource.Info.primary_key(relationship.through),
        &(&1 in join_keys)
      )

    is_unique_on_join_keys? =
      Enum.any?(Ash.Resource.Info.identities(relationship.through), fn identity ->
        is_nil(identity.where) && identity.nils_distinct? &&
          Enum.all?(identity.keys, &(&1 in join_keys))
      end)

    not (primary_key_is_join_keys? || is_unique_on_join_keys?)
  end

  defp is_many_to_many_not_unique_on_join?(_, _, _), do: false
end
