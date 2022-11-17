defmodule Ash.Actions.Load do
  @moduledoc false

  alias Ash.Engine.Request

  require Ash.Query

  def requests(
        query,
        lazy?,
        opts,
        request_path,
        root_query \\ nil,
        path \\ [],
        tenant \\ nil
      )

  def requests(nil, _, _opts, _, _, _, _), do: []

  def requests(%{load: []}, _, _opts, _, _, _, _), do: []

  def requests(
        %{load: loads} = query,
        lazy?,
        opts,
        request_path,
        root_query,
        path,
        tenant
      ) do
    root_query = root_query || query
    tenant = tenant || query.tenant || root_query.tenant

    loads
    |> List.wrap()
    |> Enum.flat_map(fn {relationship, further} ->
      relationship = Ash.Resource.Info.relationship(query.resource, relationship)

      related_query =
        case further do
          %Ash.Query{} = query ->
            Ash.Query.set_api(query, relationship.api || root_query.api)

          further ->
            relationship.destination
            |> Ash.Query.new(root_query.api)
            |> Ash.Query.load(further)
        end

      related_query =
        if tenant && !related_query.tenant do
          Ash.Query.set_tenant(related_query, tenant)
        else
          related_query
        end
        |> Ash.Query.ensure_selected(relationship.destination_attribute)

      related_query =
        if relationship.cardinality == :one do
          Ash.Query.limit(related_query, 1)
        else
          related_query
        end

      new_path = [relationship | path]

      requests(
        related_query,
        lazy?,
        opts,
        request_path,
        root_query,
        new_path,
        related_query.tenant
      ) ++
        do_requests(
          relationship,
          lazy?,
          opts,
          request_path,
          related_query,
          path,
          root_query
        )
    end)
  end

  def attach_loads([%resource{} | _] = data, loads) do
    loads
    |> Enum.sort_by(fn {key, _value} ->
      last_relationship = last_relationship!(resource, key)
      # We want to do `many_to_many` last, so we know we've
      # done their join assocs first. Pretty hacky
      {length(key), last_relationship.type == :many_to_many}
    end)
    |> Enum.reduce(data, fn {key, %{data: value}}, data ->
      last_relationship = last_relationship!(resource, key)
      lead_path = :lists.droplast(key)

      case last_relationship do
        %{type: :many_to_many} ->
          attach_many_to_many_loads(
            data,
            lead_path,
            last_relationship,
            loads,
            value
          )

        %{cardinality: :many} ->
          attach_to_many_loads(value, last_relationship, data, lead_path)

        %{cardinality: :one} ->
          attach_to_one_loads(value, last_relationship, data, lead_path)
      end
    end)
  end

  def attach_loads(data, state) when not is_list(data) do
    [data]
    |> attach_loads(state)
    |> List.first()
  end

  def attach_loads(data, _state) do
    data
  end

  defp attach_to_many_loads(
         value,
         %{name: name, no_attributes?: true},
         data,
         lead_path
       ) do
    map_or_update(data, lead_path, fn record ->
      Ash.Resource
      Map.put(record, name, List.wrap(value))
    end)
  end

  defp attach_to_many_loads(value, last_relationship, data, lead_path)
       when is_map(value) do
    if Ash.Resource.Info.primary_key_simple_equality?(last_relationship.destination) do
      primary_key = Ash.Resource.Info.primary_key(last_relationship.source)

      map_or_update(data, lead_path, fn record ->
        case primary_key do
          [field] ->
            Map.put(
              record,
              last_relationship.name,
              Map.get(value, Map.take(record, primary_key)) ||
                Map.get(value, Map.get(record, field)) || []
            )

          _ ->
            Map.put(record, last_relationship.name, Map.get(value, Map.take(record, primary_key)))
        end
      end)
    else
      primary_key = Ash.Resource.Info.primary_key(last_relationship.source)

      value = Map.to_list(value)

      single_primary_key =
        case primary_key do
          [field] ->
            field

          _ ->
            nil
        end

      map_or_update(data, lead_path, fn record ->
        related =
          value
          |> Enum.find_value(fn {key, value} ->
            key =
              if is_map(key) || !single_primary_key do
                key
              else
                %{single_primary_key => key}
              end

            if last_relationship.source.primary_key_matches?(record, key) do
              value
            end
          end)

        Map.put(
          record,
          last_relationship.name,
          related
        )
      end)
    end
  end

  defp attach_to_many_loads(value, last_relationship, data, lead_path) do
    if Ash.Resource.Info.primary_key_simple_equality?(last_relationship.destination) do
      values = Enum.group_by(value, &Map.get(&1, last_relationship.destination_attribute))

      map_or_update(data, lead_path, fn record ->
        source_key = Map.get(record, last_relationship.source_attribute)
        related_records = Map.get(values, source_key, [])
        Map.put(record, last_relationship.name, related_records)
      end)
    else
      destination_attribute = last_relationship.destination_attribute

      type =
        Ash.Resource.Info.attribute(last_relationship.destination, destination_attribute).type

      map_or_update(data, lead_path, fn record ->
        source_key = Map.get(record, last_relationship.source_attribute)

        related_records =
          Enum.filter(value, fn maybe_related ->
            Ash.Type.equal?(
              type,
              Map.get(maybe_related, destination_attribute),
              source_key
            )
          end)

        Map.put(record, last_relationship.name, related_records)
      end)
    end
  end

  defp attach_to_one_loads(value, %{name: name, no_attributes?: true}, data, lead_path) do
    map_or_update(data, lead_path, fn record ->
      if is_map(data) do
        Map.put(record, name, value |> List.wrap() |> Enum.at(0) |> elem(1))
      else
        Map.put(record, name, value |> List.wrap() |> Enum.at(0))
      end
    end)
  end

  defp attach_to_one_loads(value, last_relationship, data, lead_path) when is_map(value) do
    primary_key = Ash.Resource.Info.primary_key(last_relationship.source)

    single_primary_key =
      case primary_key do
        [field] ->
          field

        _ ->
          nil
      end

    map_or_update(data, lead_path, fn record ->
      related =
        Enum.find_value(value, fn {key, value} ->
          key =
            if is_map(key) || !single_primary_key do
              key
            else
              %{single_primary_key => key}
            end

          if last_relationship.source.primary_key_matches?(record, key) do
            {:ok, value}
          end
        end)
        |> case do
          {:ok, value} ->
            value

          _ ->
            nil
        end

      Map.put(
        record,
        last_relationship.name,
        related
      )
    end)
  end

  defp attach_to_one_loads(value, last_relationship, data, lead_path) do
    destination_attribute = last_relationship.destination_attribute

    type = Ash.Resource.Info.attribute(last_relationship.destination, destination_attribute).type

    if Ash.Type.simple_equality?(type) do
      values =
        value
        |> Enum.reverse()
        |> Enum.into(%{}, fn item ->
          {Map.get(item, last_relationship.destination_attribute), item}
        end)

      map_or_update(data, lead_path, fn record ->
        source_key = Map.get(record, last_relationship.source_attribute)
        related_record = Map.get(values, source_key)
        Map.put(record, last_relationship.name, related_record)
      end)
    else
      map_or_update(data, lead_path, fn record ->
        source_key = Map.get(record, last_relationship.source_attribute)

        related_record =
          Enum.find(value, fn maybe_related ->
            Ash.Type.equal?(
              type,
              Map.get(maybe_related, destination_attribute),
              source_key
            )
          end)

        Map.put(record, last_relationship.name, related_record)
      end)
    end
  end

  defp attach_many_to_many_loads(data, lead_path, last_relationship, loads, value) do
    join_path = lead_path ++ [last_relationship.join_relationship]

    join_data =
      loads
      |> Map.get(join_path, %{})
      |> Map.get(:data, [])

    source_attribute_on_join_resource_type =
      Ash.Resource.Info.attribute(
        last_relationship.through,
        last_relationship.source_attribute_on_join_resource
      ).type

    destination_attribute_on_join_resource_type =
      Ash.Resource.Info.attribute(
        last_relationship.through,
        last_relationship.destination_attribute_on_join_resource
      ).type

    map_or_update(data, lead_path, fn record ->
      source_value = Map.get(record, last_relationship.source_attribute)

      join_values =
        join_data
        |> Enum.filter(fn join_row ->
          Ash.Type.equal?(
            source_attribute_on_join_resource_type,
            Map.get(join_row, last_relationship.source_attribute_on_join_resource),
            source_value
          )
        end)
        |> Enum.map(&Map.get(&1, last_relationship.destination_attribute_on_join_resource))

      related_records =
        value
        |> Enum.filter(fn
          %{__lateral_join_source__: join_value} ->
            Ash.Type.equal?(
              destination_attribute_on_join_resource_type,
              source_value,
              join_value
            )

          value ->
            destination_value = Map.get(value, last_relationship.destination_attribute)

            Enum.any?(join_values, fn join_value ->
              Ash.Type.equal?(
                destination_attribute_on_join_resource_type,
                destination_value,
                join_value
              )
            end)
        end)

      Map.put(record, last_relationship.name, related_records)
    end)
  end

  defp map_or_update(nil, _, _), do: nil

  defp map_or_update(record, [], func) when not is_list(record), do: func.(record)

  defp map_or_update(records, [], func) do
    Enum.map(records, fn record ->
      if record do
        func.(record)
      else
        nil
      end
    end)
  end

  defp map_or_update(records, [path | tail], func) do
    map_or_update(records, [], fn record ->
      Map.update!(record, path, &map_or_update(&1, tail, func))
    end)
  end

  defp last_relationship!(resource, [last]) do
    Ash.Resource.Info.relationship(resource, last) || raise "Assumption Failed"
  end

  defp last_relationship!(resource, [first | rest]) do
    relationship = Ash.Resource.Info.relationship(resource, first) || raise "Assumption Failed"

    last_relationship!(relationship.destination, rest)
  end

  defp do_requests(relationship, lazy?, opts, request_path, related_query, path, root_query) do
    load_request =
      load_request(
        relationship,
        lazy?,
        opts,
        request_path,
        related_query,
        root_query,
        path
      )

    case relationship.type do
      :many_to_many ->
        if lateral_join?(related_query, relationship, :unknown) do
          [load_request]
        else
          join_assoc_request =
            join_assoc_request(
              relationship,
              request_path,
              related_query,
              root_query,
              path,
              opts,
              lazy?
            )

          [join_assoc_request, load_request]
        end

      _ ->
        [load_request]
    end
  end

  defp load_request(
         relationship,
         lazy?,
         opts,
         request_path,
         related_query,
         root_query,
         path
       ) do
    relationship_path = Enum.reverse(Enum.map([relationship | path], &Map.get(&1, :name)))

    this_request_path =
      request_path ++
        [
          :load,
          relationship_path
        ]

    dependencies =
      case path do
        [] ->
          [this_request_path ++ [:authorization_filter]]

        dependent_path ->
          [
            this_request_path ++ [:authorization_filter],
            request_path ++
              [:load, Enum.reverse(Enum.map(dependent_path, &Map.get(&1, :name))), :data],
            request_path ++
              [:load, Enum.reverse(Enum.map(dependent_path, &Map.get(&1, :name))), :query]
          ]
      end

    dependencies = [request_path ++ [:data] | dependencies]

    dependencies =
      if relationship.type == :many_to_many &&
           !lateral_join?(related_query, relationship, :unknown) do
        join_relationship = join_relationship(relationship)

        join_relationship_path =
          Enum.map(join_relationship_path(path, join_relationship), & &1.name)

        [
          request_path ++
            [
              :load,
              join_relationship_path,
              :data
            ]
          | dependencies
        ]
      else
        dependencies
      end

    source =
      [relationship | path]
      |> Enum.reverse()
      |> Enum.map_join(".", &Map.get(&1, :name))

    Request.new(
      action:
        related_query.action || Ash.Resource.Info.primary_action(relationship.destination, :read),
      resource: relationship.destination,
      name: "load #{source}",
      api: related_query.api,
      path: this_request_path,
      query:
        load_query(
          relationship,
          related_query
        ),
      data:
        data(
          relationship,
          lazy?,
          dependencies,
          this_request_path,
          request_path,
          related_query,
          path,
          root_query,
          opts
        )
    )
  end

  defp data(
         %{manual: manual} = relationship,
         lazy?,
         dependencies,
         _this_request_path,
         request_path,
         related_query,
         path,
         root_query,
         request_opts
       )
       when not is_nil(manual) do
    {mod, opts} =
      case manual do
        {mod, opts} ->
          {mod, opts}

        mod ->
          {mod, []}
      end

    Request.resolve(dependencies, fn data ->
      data =
        case path do
          [] ->
            get_in(data, request_path ++ [:data, :results])

          path ->
            data =
              data
              |> get_in(request_path)
              |> Kernel.||(%{})
              |> Map.get(:load, %{})
              |> Map.get(Enum.reverse(Enum.map(path, & &1.name)), %{})
              |> Map.get(:data, %{})

            if is_map(data) do
              data
              |> Map.values()
              |> Enum.flat_map(&List.wrap/1)
            else
              data
            end
        end

      lazy_load_or(
        data,
        lazy?,
        relationship.name,
        related_query.api,
        related_query,
        request_opts,
        fn ->
          data
          |> mod.load(opts, %{
            relationship: relationship,
            query: related_query,
            root_query: root_query,
            actor: request_opts[:actor],
            authorize?: request_opts[:authorize?],
            api: related_query.api,
            tenant: related_query.tenant
          })
          |> case do
            {:ok, result} ->
              # TODO: this will result in quite a few requests potentially for aggs/calcs
              # This should be optimized.
              Enum.reduce_while(result, {:ok, %{}}, fn {key, records}, {:ok, acc} ->
                case related_query.api.load(records, %{related_query | load: []},
                       lazy?: true,
                       tenant: related_query.tenant,
                       actor: request_opts[:actor],
                       authorize?: request_opts[:authorize?],
                       tracer: request_opts[:tracer]
                     ) do
                  {:ok, results} ->
                    {:cont, {:ok, Map.put(acc, key, results)}}

                  {:error, error} ->
                    {:halt, {:error, error}}
                end
              end)

            {:error, error} ->
              {:error, error}
          end
        end
      )
    end)
  end

  defp data(
         relationship,
         lazy?,
         dependencies,
         this_request_path,
         request_path,
         related_query,
         path,
         root_query,
         opts
       ) do
    Request.resolve(dependencies, fn data ->
      base_query =
        case get_in(data, this_request_path ++ [:authorization_filter]) do
          nil ->
            related_query

          authorization_filter ->
            Ash.Query.filter(related_query, ^authorization_filter)
        end

      source_query =
        case path do
          [] ->
            root_query

          path ->
            get_in(
              data,
              request_path ++
                [
                  :load,
                  Enum.reverse(Enum.map(path, &Map.get(&1, :name))),
                  :query
                ]
            )
        end

      source_query =
        if related_query.tenant do
          Ash.Query.set_tenant(source_query, related_query.tenant)
        else
          source_query
        end

      with {:ok, new_query} <-
             true_load_query(
               relationship,
               base_query,
               data,
               path,
               request_path
             ),
           {:ok, results} <-
             run_actual_query(
               new_query,
               base_query,
               data,
               path,
               relationship,
               source_query,
               request_path,
               opts,
               lazy?
             ) do
        {:ok, results}
      else
        :nothing ->
          {:ok, []}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp lazy_load_or(data, lazy?, relationship, api, related_query, request_opts, func) do
    if lazy? && Ash.Resource.loaded?(data, relationship) do
      pkey = Ash.Resource.Info.primary_key(related_query.resource)

      data
      |> get_related(relationship)
      |> Enum.uniq_by(&Map.take(&1, pkey))
      |> api.load(related_query,
        lazy?: true,
        authorize?: request_opts[:authorize?],
        actor: request_opts[:actor]
      )
    else
      func.()
    end
  end

  defp get_related(data, relationship) when is_list(data) do
    Enum.flat_map(data, fn item ->
      item
      |> Map.get(relationship)
      |> List.wrap()
    end)
  end

  defp get_related(data, relationship) do
    get_related(List.wrap(data), relationship)
  end

  defp join_relationship(relationship) do
    Ash.Resource.Info.relationship(relationship.source, relationship.join_relationship)
  end

  defp join_relationship_path(path, join_relationship) do
    Enum.reverse([join_relationship | path])
  end

  defp join_assoc_request(
         relationship,
         request_path,
         related_query,
         root_query,
         path,
         opts,
         lazy?
       ) do
    join_relationship = join_relationship(relationship)

    join_relationship_path = join_relationship_path(path, join_relationship)
    join_relationship_path_names = Enum.map(join_relationship_path, & &1.name)

    dependencies =
      if path == [] do
        [
          [:load, join_relationship_path_names, :authorization_filter]
        ]
      else
        [
          [:load, join_relationship_path_names, :authorization_filter],
          [:load, Enum.reverse(Enum.map(path, &Map.get(&1, :name))), :data],
          [:load, Enum.reverse(Enum.map(path, &Map.get(&1, :name))), :query]
        ]
      end

    lateral_join? = lateral_join?(related_query, relationship, :unknown)

    dependencies =
      if lateral_join? do
        dependencies ++
          [
            [
              :load,
              Enum.reverse(Enum.map(path, &Map.get(&1, :name))) ++ [relationship.name],
              :data
            ]
          ]
      else
        dependencies
      end

    dependencies = [[:data] | dependencies]

    related_query =
      if related_query.tenant do
        join_relationship.destination
        |> Ash.Query.new(related_query.api)
        |> Ash.Query.set_tenant(related_query.tenant)
      else
        Ash.Query.new(join_relationship.destination, related_query.api)
      end

    dependencies = Enum.map(dependencies, &(request_path ++ &1))

    Request.new(
      action:
        related_query.action ||
          Ash.Resource.Info.primary_action(relationship.destination, :read),
      resource: relationship.through,
      name: "load join #{join_relationship.name}",
      api: related_query.api,
      path: request_path ++ [:load, join_relationship_path_names],
      query:
        load_query(
          join_relationship,
          related_query
        ),
      data:
        Request.resolve(dependencies, fn
          data ->
            base_query =
              case get_in(
                     data,
                     request_path ++
                       [
                         :load,
                         join_relationship_path_names,
                         :authorization_filter
                       ]
                   ) do
                nil ->
                  related_query

                authorization_filter ->
                  Ash.Query.filter(related_query, ^authorization_filter)
              end

            source_data =
              case path do
                [] ->
                  get_in(data, request_path ++ [:data, :results])

                path ->
                  data
                  |> get_in(request_path)
                  |> Kernel.||(%{})
                  |> Map.get(:load, %{})
                  |> Map.get(Enum.reverse(Enum.map(path, & &1.name)), %{})
                  |> Map.get(:data, %{})
              end

            lateral_join? = lateral_join?(related_query, relationship, source_data)

            base_query =
              if related_query.tenant do
                Ash.Query.set_tenant(base_query, related_query.tenant)
              else
                base_query
              end

            source_query =
              case path do
                [] ->
                  root_query

                path ->
                  get_in(
                    data,
                    request_path ++
                      [:load, Enum.reverse(Enum.map(path, &Map.get(&1, :name))), :query]
                  )
              end

            source_query =
              if related_query.tenant do
                Ash.Query.set_tenant(source_query, related_query.tenant)
              else
                source_query
              end

            with {:ok, new_query} <-
                   true_load_query(
                     join_relationship,
                     base_query,
                     data,
                     path,
                     request_path
                   ),
                 new_query <-
                   add_join_destination_filter(
                     new_query,
                     lateral_join?,
                     data,
                     relationship,
                     Enum.reverse(Enum.map(path, &Map.get(&1, :name))) ++ [relationship.name]
                   ),
                 {:ok, results} <-
                   run_actual_query(
                     new_query,
                     base_query,
                     data,
                     path,
                     join_relationship,
                     source_query,
                     request_path,
                     opts,
                     lazy?
                   ) do
              {:ok, results}
            else
              :nothing ->
                {:ok, []}

              {:error, error} ->
                {:error, error}
            end
        end)
    )
  end

  defp add_join_destination_filter(query, true, data, relationship, destination_path) do
    ids =
      data
      |> get_in([
        :load,
        destination_path,
        :data
      ])
      |> case do
        %page{results: results} when page in [Ash.Page.Keyset, Ash.Page.Offset] ->
          results

        data ->
          data
      end
      |> List.wrap()
      |> Enum.map(fn related ->
        Map.get(related, relationship.destination_attribute)
      end)

    filter = [{relationship.destination_attribute_on_join_resource, [{:in, ids}]}]

    Ash.Query.filter(query, ^filter)
  end

  defp add_join_destination_filter(query, false, _, _, _) do
    query
  end

  defp lateral_join?(query, relationship, source_data) do
    action =
      case relationship.read_action do
        nil ->
          Ash.Resource.Info.primary_action!(relationship.destination, :read)

        action ->
          Ash.Resource.Info.action(relationship.destination, action)
      end

    if action.manual do
      false
    else
      {offset, limit} = offset_and_limit(query)

      resources =
        [relationship.source, Map.get(relationship, :through), relationship.destination]
        |> Enum.reject(&is_nil/1)

      cond do
        is_many_to_many_not_unique_on_join?(relationship) ->
          false

        limit == 1 && is_nil(relationship.context) && is_nil(relationship.filter) &&
          is_nil(relationship.sort) && relationship.type != :many_to_many ->
          false

        limit == 1 && (source_data == :unknown || Enum.count_until(source_data, 2) == 1) &&
            relationship.type != :many_to_many ->
          false

        true ->
          lateral_join =
            (limit || offset || relationship.type == :many_to_many) &&
              Ash.DataLayer.data_layer_can?(
                relationship.source,
                {:lateral_join, resources}
              )

          !!lateral_join
      end
    end
  end

  defp is_many_to_many_not_unique_on_join?(%{type: :many_to_many} = relationship) do
    join_keys =
      Enum.sort([
        relationship.source_attribute_on_join_resource,
        relationship.destination_attribute_on_join_resource
      ])

    primary_key_is_join_keys? =
      Enum.sort(Ash.Resource.Info.primary_key(relationship.through)) == join_keys

    is_unique_on_join_keys? =
      Enum.any?(Ash.Resource.Info.identities(relationship.through), fn identity ->
        Enum.sort(identity.keys) == join_keys
      end)

    not (primary_key_is_join_keys? || is_unique_on_join_keys?)
  end

  defp is_many_to_many_not_unique_on_join?(_), do: false

  defp run_actual_query(
         query,
         base_query,
         data,
         path,
         relationship,
         source_query,
         request_path,
         request_opts,
         lazy?
       ) do
    {offset, limit} = offset_and_limit(base_query)

    source_data =
      case path do
        [] ->
          get_in(data, request_path ++ [:data, :results])

        path ->
          data =
            data
            |> get_in(request_path)
            |> Kernel.||(%{})
            |> Map.get(:load, %{})
            |> Map.get(Enum.reverse(Enum.map(path, & &1.name)), %{})
            |> Map.get(:data, %{})

          if is_map(data) do
            data
            |> Map.values()
            |> Enum.flat_map(&List.wrap/1)
          else
            data
          end
      end

    lazy_load_or(
      source_data,
      lazy?,
      relationship.name,
      query.api,
      query,
      request_opts,
      fn ->
        cond do
          lateral_join?(query, relationship, source_data) && relationship.type == :many_to_many ->
            join_relationship =
              Ash.Resource.Info.relationship(relationship.source, relationship.join_relationship)

            query
            |> Ash.Query.set_context(%{
              data_layer: %{
                lateral_join_source: {
                  source_data,
                  [
                    {source_query, relationship.source_attribute,
                     relationship.source_attribute_on_join_resource, relationship},
                    {relationship.through, relationship.destination_attribute_on_join_resource,
                     relationship.destination_attribute, join_relationship}
                  ]
                }
              }
            })
            |> Ash.Query.set_context(relationship.context)
            |> Ash.Query.do_filter(relationship.filter)
            |> Ash.Query.sort(relationship.sort, prepend?: true)
            |> remove_relationships_from_load()
            |> read(relationship.read_action, request_opts)

          lateral_join?(query, relationship, source_data) && (limit || offset) ->
            query
            |> Ash.Query.set_context(%{
              data_layer: %{
                lateral_join_source:
                  {source_data,
                   [
                     {source_query, relationship.source_attribute,
                      relationship.destination_attribute, relationship}
                   ]}
              }
            })
            |> Ash.Query.set_context(relationship.context)
            |> Ash.Query.do_filter(relationship.filter)
            |> Ash.Query.sort(relationship.sort, prepend?: true)
            |> remove_relationships_from_load()
            |> read(relationship.read_action, request_opts)

          limit || offset ->
            artificial_limit_and_offset(
              query,
              limit,
              offset,
              relationship,
              request_opts,
              source_data,
              request_path,
              path,
              data
            )

          true ->
            query
            |> Ash.Query.set_context(relationship.context)
            |> Ash.Query.do_filter(relationship.filter)
            |> Ash.Query.sort(relationship.sort, prepend?: true)
            |> remove_relationships_from_load()
            |> read(relationship.read_action, request_opts)
        end
      end
    )
  end

  defp offset_and_limit(query) do
    if query.offset == 0 do
      {nil, query.limit}
    else
      {query.offset, query.limit}
    end
  end

  defp artificial_limit_and_offset(
         query,
         limit,
         offset,
         relationship,
         request_opts,
         _source_data,
         request_path,
         path,
         data
       ) do
    query
    |> Ash.Query.set_context(relationship.context)
    |> Ash.Query.do_filter(relationship.filter)
    |> Ash.Query.sort(relationship.sort, prepend?: true)
    |> remove_relationships_from_load()
    |> read(relationship.read_action, request_opts)
    |> case do
      {:ok, results} ->
        new_results =
          if relationship.type == :many_to_many do
            results = Enum.with_index(results)
            join_path = path ++ [relationship.join_relationship]

            join_data =
              data
              |> get_in(request_path ++ [:load])
              |> Kernel.||(%{})
              |> Map.get(join_path, %{})
              |> Map.get(:data, [])

            destination_attribute_on_join_resource_type =
              Ash.Resource.Info.attribute(
                relationship.through,
                relationship.destination_attribute_on_join_resource
              ).type

            join_data
            |> Enum.uniq_by(
              &Map.take(&1, [
                relationship.source_attribute_on_join_resource,
                relationship.destination_attribute_on_join_resource
              ])
            )
            |> Enum.group_by(&Map.get(&1, relationship.source_attribute_on_join_resource))
            |> Enum.flat_map(fn {_, group} ->
              group =
                Enum.map(group, fn join_row ->
                  Enum.find_value(results, fn {record, index} ->
                    if Ash.Type.equal?(
                         destination_attribute_on_join_resource_type,
                         Map.get(join_row, relationship.destination_attribute_on_join_resource),
                         Map.get(record, relationship.destination_attribute)
                       ) do
                      {Map.put(
                         record,
                         :__lateral_join_source__,
                         Map.get(join_row, relationship.source_attribute_on_join_resource)
                       ), index}
                    end
                  end)
                end)

              offset_records =
                group
                |> Enum.sort_by(&elem(&1, 1))
                |> Enum.map(&elem(&1, 0))
                |> Enum.drop(offset || 0)

              if limit do
                Enum.take(offset_records, limit)
              else
                offset_records
              end
            end)
          else
            results
            |> Enum.with_index()
            |> Enum.group_by(fn {record, _i} ->
              Map.get(record, relationship.destination_attribute)
            end)
            |> Enum.flat_map(fn {_, group} ->
              group =
                group
                |> Enum.sort_by(&elem(&1, 1))
                |> Enum.map(&elem(&1, 0))

              offset_records = Enum.drop(group, offset || 0)

              if limit do
                Enum.take(offset_records, limit)
              else
                offset_records
              end
            end)
          end

        {:ok, new_results}

      {:error, error} ->
        {:error, error}
    end
  end

  defp read(query, action, request_opts) do
    action = action || primary_read(query)

    Ash.Actions.Read.unpaginated_read(
      query,
      action,
      request_opts
    )
  end

  defp primary_read(query) do
    action = Ash.Resource.Info.primary_action(query.resource, :read)

    if action do
      action
    else
      raise """
      No read action for loaded resource: #{query.resource}
      """
    end
  end

  defp remove_relationships_from_load(query) do
    case query.load do
      empty when empty in [nil, []] ->
        query

      load ->
        new_load =
          load
          |> List.wrap()
          |> Enum.reject(fn
            item when is_atom(item) ->
              Ash.Resource.Info.relationship(query.resource, item)

            {item, _} ->
              Ash.Resource.Info.relationship(query.resource, item)
          end)

        %{query | load: new_load}
    end
  end

  defp load_query(%{manual: manual}, related_query)
       when not is_nil(manual) do
    related_query
  end

  defp load_query(
         relationship,
         related_query
       ) do
    if Map.get(relationship, :no_attributes?) do
      relationship.destination
      |> Ash.Query.new(related_query.api)
    else
      relationship.destination
      |> Ash.Query.new(related_query.api)
      |> Ash.Query.filter(^related_query.filter)
    end
  end

  defp true_load_query(relationship, query, data, path, request_path) do
    {source_attribute, path} =
      if relationship.type == :many_to_many do
        join_relationship = join_relationship(relationship)

        {relationship.destination_attribute_on_join_resource,
         join_relationship_path(path, join_relationship) |> Enum.map(& &1.name)}
      else
        {relationship.source_attribute, path |> Enum.reverse() |> Enum.map(& &1.name)}
      end

    source_data =
      case path do
        [] ->
          get_in(data, request_path ++ [:data, :results])

        path ->
          data
          |> get_in(request_path)
          |> Kernel.||(%{})
          |> Map.get(:load, %{})
          |> Map.get(path, %{})
          |> Map.get(:data, %{})
      end

    case source_data do
      %{data: empty} when empty in [[], nil] ->
        :nothing

      empty when empty in [[], nil] ->
        :nothing

      _ ->
        get_query(query, relationship, source_data, source_attribute)
    end
  end

  defp get_query(query, relationship, source_data, source_attribute) do
    {offset, limit} = offset_and_limit(query)

    cond do
      lateral_join?(query, relationship, source_data) ->
        {:ok, Ash.Query.unset(query, :load)}

      Map.get(relationship, :no_attributes?) ->
        {:ok, query}

      true ->
        query =
          if limit || offset do
            Ash.Query.unset(query, [:limit, :offset])
          else
            query
          end

        related_data =
          case source_data do
            %page{results: results} when page in [Ash.Page.Keyset, Ash.Page.Offset] ->
              results

            data ->
              data
          end

        ids =
          Enum.flat_map(related_data, fn data ->
            data
            |> Map.get(source_attribute)
            |> List.wrap()
          end)

        filter_value =
          case ids do
            [id] ->
              id

            ids ->
              [in: ids]
          end

        new_query =
          query
          |> Ash.Query.filter(^[{relationship.destination_attribute, filter_value}])
          |> Ash.Query.unset(:load)

        {:ok, new_query}
    end
  end

  def reverse_relationship_path(relationship, []) do
    relationship.destination
    |> Ash.Resource.Info.relationships()
    |> Enum.find(fn destination_relationship ->
      reverse_relationship?(relationship, destination_relationship)
    end)
    |> case do
      nil ->
        :error

      reverse ->
        {:ok, [reverse.name]}
    end
  end

  def reverse_relationship_path(relationship, rest) do
    relationship.destination
    |> Ash.Resource.Info.relationships()
    |> Enum.find(fn destination_relationship ->
      reverse_relationship?(relationship, destination_relationship)
    end)
    |> case do
      nil ->
        :error

      reverse ->
        case do_reverse_relationship_path(reverse, rest) do
          :error ->
            :error

          {:ok, path} ->
            {:ok, [reverse.name | path]}
        end
    end
  end

  def do_reverse_relationship_path(relationship, prior_path, acc \\ [])

  def do_reverse_relationship_path(nil, _, _) do
    :error
  end

  def do_reverse_relationship_path(relationship, [], acc) do
    relationship.source
    |> Ash.Resource.Info.relationships()
    |> Enum.find(fn destination_relationship ->
      reverse_relationship?(relationship, destination_relationship)
    end)
    |> case do
      nil ->
        :error

      reverse ->
        {:ok, [reverse.name | acc]}
    end
  end

  def do_reverse_relationship_path(relationship, [next_relationship | rest], acc) do
    relationship.destination
    |> Ash.Resource.Info.relationships()
    |> Enum.find(fn destination_relationship ->
      reverse_relationship?(relationship, destination_relationship)
    end)
    |> case do
      nil ->
        :error

      reverse ->
        do_reverse_relationship_path(
          Ash.Resource.Info.relationship(relationship.destination, next_relationship),
          rest,
          [reverse.name | acc]
        )
    end
  end

  defp reverse_relationship?(rel, destination_rel) do
    rel.source == destination_rel.destination &&
      rel.destination == destination_rel.source &&
      rel.source_attribute == destination_rel.destination_attribute &&
      rel.destination_attribute == destination_rel.source_attribute &&
      Map.fetch(rel, :source_attribute_on_join_resource) ==
        Map.fetch(destination_rel, :destination_attribute_on_join_resource) &&
      Map.fetch(rel, :destination_attribute_on_join_resource) ==
        Map.fetch(destination_rel, :source_attribute_on_join_resource) &&
      is_nil(destination_rel.context) &&
      is_nil(rel.context)
  end
end
