defmodule Ash.Actions.Helpers do
  @moduledoc false
  require Logger
  require Ash.Flags

  def rollback_if_in_transaction({:error, error}, resource, changeset) do
    if Ash.DataLayer.in_transaction?(resource) do
      case changeset do
        %Ash.Changeset{} = changeset ->
          Ash.DataLayer.rollback(resource, Ash.Changeset.add_error(changeset, error))

        %Ash.Query{} = query ->
          Ash.DataLayer.rollback(resource, Ash.Query.add_error(query, error))

        _ ->
          Ash.DataLayer.rollback(resource, Ash.Error.to_error_class(error))
      end
    else
      {:error, error}
    end
  end

  def rollback_if_in_transaction({:error, :no_rollback, error}, _, _changeset) do
    {:error, error}
  end

  def rollback_if_in_transaction(success, _, _), do: success

  def validate_calculation_load!(%{__struct__: Ash.Query}, module) do
    raise """
    `#{inspect(module)}.load/3` returned a query.

    Returning a query from the `load/3` callback of a calculation is now deprecated.
    Instead, return the load statement itself, i.e instead of `Ash.Query.load(query, [...])`,
    just return `[...]`. This is so that Ash can examine the requirements of just this single
    calculation to ensure that all required values are present
    """
  end

  def validate_calculation_load!(other, _), do: other

  defp set_context(%{__struct__: Ash.Changeset} = changeset, context),
    do: Ash.Changeset.set_context(changeset, context)

  defp set_context(%{__struct__: Ash.Query} = query, context),
    do: Ash.Query.set_context(query, context)

  defp set_context(%{__struct__: Ash.ActionInput} = action_input, context),
    do: Ash.ActionInput.set_context(action_input, context)

  def add_process_context(api, query_or_changeset, opts) do
    query_or_changeset = set_context(query_or_changeset, opts[:context] || %{})
    api = api || query_or_changeset.api

    opts =
      case query_or_changeset.context do
        %{
          private: %{
            actor: actor
          }
        } ->
          Keyword.put_new(opts, :actor, actor)

        _ ->
          opts
      end

    opts =
      case query_or_changeset.context do
        %{
          private: %{
            authorize?: authorize?
          }
        } ->
          Keyword.put_new(opts, :authorize?, authorize?)

        _ ->
          opts
      end

    opts =
      case query_or_changeset.context do
        %{
          private: %{
            tracer: tracer
          }
        } ->
          do_add_tracer(opts, tracer)

        _ ->
          opts
      end

    opts = set_opts(opts, api, query_or_changeset)

    query_or_changeset = add_context(query_or_changeset, opts)

    {query_or_changeset, opts}
  end

  def set_opts(opts, api, query_or_changeset \\ nil) do
    opts
    |> add_actor(query_or_changeset, api)
    |> add_authorize?(query_or_changeset, api)
    |> add_tenant()
    |> add_tracer()
  end

  def add_context(query_or_changeset, opts) do
    context = Process.get(:ash_context, %{}) || %{}
    private_context = Map.new(Keyword.take(opts, [:actor, :authorize?, :tracer]))

    context =
      context
      |> Map.merge(private_context)
      |> Map.put_new(:actor, nil)
      |> Map.put_new(:authorize?, false)

    case query_or_changeset do
      %{__struct__: Ash.ActionInput} ->
        query_or_changeset
        |> Ash.ActionInput.set_context(context)
        |> Ash.ActionInput.set_context(%{private: private_context})
        |> Ash.ActionInput.set_tenant(query_or_changeset.tenant || opts[:tenant])

      %{__struct__: Ash.Query} ->
        query_or_changeset
        |> Ash.Query.set_context(context)
        |> Ash.Query.set_context(%{private: private_context})
        |> Ash.Query.set_tenant(query_or_changeset.tenant || opts[:tenant])

      %{__struct__: Ash.Changeset} ->
        query_or_changeset
        |> Ash.Changeset.set_context(context)
        |> Ash.Changeset.set_context(%{
          private: private_context
        })
        |> Ash.Changeset.set_tenant(query_or_changeset.tenant || opts[:tenant])
    end
  end

  defp add_actor(opts, query_or_changeset, api) do
    opts =
      if Keyword.has_key?(opts, :actor) do
        opts
      else
        case Process.get(:ash_actor) do
          {:actor, value} ->
            Keyword.put(opts, :actor, value)

          _ ->
            opts
        end
      end

    if api do
      if !internal?(query_or_changeset) && !Keyword.has_key?(opts, :actor) &&
           Ash.Api.Info.require_actor?(api) do
        raise Ash.Error.to_error_class(Ash.Error.Forbidden.ApiRequiresActor.exception(api: api))
      end

      opts
    else
      # The only time api would be nil here is when we call this helper inside of `Changeset.for_*` and `Query.for_read`
      # meaning this will be run again later with the api, so we skip the validations on the api
      opts
    end
  end

  defp internal?(%{context: %{private: %{internal?: true}}}), do: true
  defp internal?(_), do: false

  defp add_authorize?(opts, query_or_changeset, api) do
    opts =
      if Keyword.has_key?(opts, :authorize?) do
        opts
      else
        case Process.get(:ash_authorize?) do
          {:authorize?, value} ->
            Keyword.put(opts, :authorize?, value)

          _ ->
            opts
        end
      end

    if api do
      case Ash.Api.Info.authorize(api) do
        :always ->
          if opts[:authorize?] == false && internal?(query_or_changeset) do
            opts
          else
            Keyword.put(opts, :authorize?, true)
          end

        :by_default ->
          Keyword.put_new(opts, :authorize?, true)

        :when_requested ->
          if Keyword.has_key?(opts, :actor) do
            Keyword.put_new(opts, :authorize?, true)
          else
            Keyword.put(opts, :authorize?, opts[:authorize?] || Keyword.has_key?(opts, :actor))
          end
      end
    else
      # The only time api would be nil here is when we call this helper inside of `Changeset.for_*` and `Query.for_read`
      # meaning this will be run again later with the api, so we skip the validations on the api
      opts
    end
  end

  defp add_tenant(opts) do
    if Keyword.has_key?(opts, :tenant) do
      opts
    else
      case Process.get(:ash_tenant) do
        {:tenant, value} ->
          Keyword.put(opts, :tenant, value)

        _ ->
          opts
      end
    end
  end

  defp add_tracer(opts) do
    opts =
      case Process.get(:ash_tracer) do
        {:tracer, value} ->
          do_add_tracer(opts, value)

        _ ->
          opts
      end

    case Application.get_env(:ash, :tracer) do
      nil ->
        opts

      tracer ->
        do_add_tracer(opts, tracer)
    end
  end

  defp do_add_tracer(opts, tracer) do
    tracer = List.wrap(tracer)

    Keyword.update(opts, :tracer, tracer, fn existing_tracer ->
      if is_list(existing_tracer) do
        Enum.uniq(tracer ++ existing_tracer)
      else
        if is_nil(existing_tracer) do
          tracer
        else
          Enum.uniq(tracer ++ existing_tracer)
        end
      end
    end)
  end

  @doc false
  def notify({:ok, record, instructions}, changeset, opts) do
    resource_notification = resource_notification(changeset, record, opts)

    if opts[:return_notifications?] do
      {:ok, record,
       Map.update(
         instructions,
         :notifications,
         [resource_notification],
         &[resource_notification | &1]
       )}
    else
      if Process.get(:ash_started_transaction?) do
        current_notifications = Process.get(:ash_notifications, [])

        Process.put(
          :ash_notifications,
          [resource_notification | current_notifications]
        )
      else
        unsent_notifications = Ash.Notifier.notify([resource_notification])

        warn_missed!(changeset.resource, changeset.action, %{
          resource_notifications: unsent_notifications
        })
      end

      {:ok, record, instructions}
    end
  end

  def notify(other, _changeset, _opts), do: other

  defp resource_notification(changeset, result, opts) do
    %Ash.Notifier.Notification{
      resource: changeset.resource,
      api: changeset.api,
      actor: changeset.context[:private][:actor],
      action: changeset.action,
      data: result,
      changeset: changeset,
      from: self(),
      metadata: opts[:notification_metadata] || %{}
    }
  end

  def warn_missed!(resource, action, result) do
    case Map.get(result, :resource_notifications, Map.get(result, :notifications, [])) do
      empty when empty in [nil, []] ->
        :ok

      missed ->
        case Application.get_env(:ash, :missed_notifications, :warn) do
          :ignore ->
            :ok

          :raise ->
            raise """
            Missed #{Enum.count(missed)} notifications in action #{inspect(resource)}.#{action.name}.

            This happens when the resources are in a transaction, and you did not pass
            `return_notifications?: true`. If you are in a changeset hook, you can
            return the notifications. If not, you can send the notifications using
            `Ash.Notifier.notify/1` once your resources are out of a transaction.

            To ignore these in all cases:

            config :ash, :missed_notifications, :ignore

            To turn this into warnings:

            config :ash, :missed_notifications, :warn
            """

          :warn ->
            {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

            Logger.warning("""
            Missed #{Enum.count(missed)} notifications in action #{inspect(resource)}.#{action.name}.

            This happens when the resources are in a transaction, and you did not pass
            `return_notifications?: true`. If you are in a changeset hook, you can
            return the notifications. If not, you can send the notifications using
            `Ash.Notifier.notify/1` once your resources are out of a transaction.

            #{Exception.format_stacktrace(stacktrace)}

            While you should likely leave this setting on, you can ignore these or turn them into errors.

            To ignore these in all cases:

            config :ash, :missed_notifications, :ignore

            To turn this into raised errors:

            config :ash, :missed_notifications, :raise
            """)
        end
    end
  end

  def process_errors(changeset, [error]) do
    %{changeset | errors: []}
    |> Ash.Changeset.add_error(error)
    |> Map.get(:errors)
    |> case do
      [error] ->
        error

      errors ->
        errors
    end
  end

  def process_errors(changeset, errors) when is_list(errors) do
    %{changeset | errors: []}
    |> Ash.Changeset.add_error(errors)
    |> Map.get(:errors)
  end

  def process_errors(changeset, error), do: process_errors(changeset, [error])

  def load_runtime_types({:ok, results}, query, attributes?) do
    load_runtime_types(results, query, attributes?)
  end

  def load_runtime_types({:error, error}, _query, _attributes?) do
    {:error, error}
  end

  def load_runtime_types(results, query, attributes?) when is_list(results) do
    attributes = runtime_attributes(query, attributes?)
    calcs = runtime_calculations(query)

    if Enum.empty?(attributes) && Enum.empty?(calcs) do
      {:ok, results}
    else
      Enum.reduce_while(results, {:ok, []}, fn result, {:ok, results} ->
        case do_load_runtime_types(result, attributes, calcs) do
          {:ok, result} ->
            {:cont, {:ok, [result | results]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
      |> case do
        {:ok, results} -> {:ok, Enum.reverse(results)}
        {:error, error} -> {:error, error}
      end
    end
  end

  def load_runtime_types(nil, _, _attributes?), do: {:ok, nil}

  def load_runtime_types(result, query, attributes?) do
    do_load_runtime_types(
      result,
      runtime_attributes(query, attributes?),
      runtime_calculations(query)
    )
  end

  defp runtime_attributes(query, true) do
    case query.select do
      nil ->
        Ash.Resource.Info.attributes(query.resource)

      select ->
        Enum.map(select, &Ash.Resource.Info.attribute(query.resource, &1))
    end
    |> Enum.reject(fn %{type: type, constraints: constraints} ->
      Ash.Type.cast_in_query?(type, constraints)
    end)
  end

  defp runtime_attributes(_, _), do: []

  defp runtime_calculations(query) do
    query.calculations
    |> Kernel.||(%{})
    |> Enum.filter(fn {_name, calc} ->
      calc.type
    end)
    |> Enum.reject(fn {_name, calc} ->
      constraints = Map.get(calc, :constraints, [])

      if function_exported?(Ash.Type, :cast_in_query?, 2) do
        Ash.Type.cast_in_query?(calc.type, constraints)
      else
        Ash.Type.cast_in_query?(calc.type)
      end
    end)
  end

  defp do_load_runtime_types(record, select, calculations) do
    select
    |> Enum.reduce_while({:ok, record}, fn attr, {:ok, record} ->
      case Map.get(record, attr.name) do
        nil ->
          {:cont, {:ok, record}}

        %Ash.NotLoaded{} ->
          {:cont, {:ok, record}}

        %Ash.ForbiddenField{} ->
          {:cont, {:ok, record}}

        value ->
          case Ash.Type.cast_stored(
                 attr.type,
                 value,
                 attr.constraints
               ) do
            {:ok, value} ->
              {:cont, {:ok, Map.put(record, attr.name, value)}}

            :error ->
              {:halt, {:error, message: "is invalid", field: attr.name}}
          end
      end
    end)
    |> case do
      {:ok, record} ->
        Enum.reduce_while(calculations, {:ok, record}, fn {name, calc}, {:ok, record} ->
          case calc.load do
            nil ->
              case Map.get(record.calculations || %{}, calc.name) do
                nil ->
                  {:cont, {:ok, record}}

                value ->
                  case Ash.Type.cast_stored(
                         calc.type,
                         value,
                         Map.get(calc, :constraints, [])
                       ) do
                    {:ok, value} ->
                      {:cont,
                       {:ok, Map.update!(record, :calculations, &Map.put(&1, name, value))}}

                    :error ->
                      {:halt, {:error, message: "is invalid", field: calc.name}}
                  end
              end

            load ->
              case Map.get(record, load) do
                nil ->
                  {:cont, {:ok, record}}

                value ->
                  case Ash.Type.cast_stored(
                         calc.type,
                         value,
                         Map.get(calc, :constraints, [])
                       ) do
                    {:ok, casted} ->
                      {:cont, {:ok, Map.put(record, load, casted)}}

                    :error ->
                      {:halt, {:error, message: "is invalid", field: calc.name}}
                  end
              end
          end
        end)

      other ->
        other
    end
  end

  def load({:ok, result, instructions}, changeset, api, opts) do
    if changeset.load in [nil, []] do
      {:ok, result, instructions}
    else
      query =
        changeset.resource
        |> Ash.Query.load(changeset.load)
        |> select_selected(result)

      case api.load(result, query, opts) do
        {:ok, result} ->
          {:ok, result, instructions}

        {:error, error} ->
          {:error, error}
      end
    end
  end

  def load({:ok, result}, changeset, api, opts) do
    if changeset.load in [nil, []] do
      {:ok, result, %{}}
    else
      query =
        changeset.resource
        |> Ash.Query.load(changeset.load)
        |> select_selected(result)

      case api.load(result, query, opts) do
        {:ok, result} ->
          {:ok, result, %{}}

        {:error, error} ->
          {:error, error}
      end
    end
  end

  def load(other, _, _, _), do: other

  defp select_selected(query, result) do
    select =
      query.resource
      |> Ash.Resource.Info.attributes()
      |> Enum.filter(&Ash.Resource.selected?(result, &1.name))
      |> Enum.map(& &1.name)

    Ash.Query.ensure_selected(query, select)
  end

  def restrict_field_access({:ok, record, instructions}, query_or_changeset) do
    {:ok, restrict_field_access(record, query_or_changeset), instructions}
  end

  def restrict_field_access({:ok, record}, query_or_changeset) do
    {:ok, restrict_field_access(record, query_or_changeset)}
  end

  def restrict_field_access({:error, error}, _), do: {:error, error}

  def restrict_field_access(records, query_or_changeset) when is_list(records) do
    Enum.map(records, &restrict_field_access(&1, query_or_changeset))
  end

  def restrict_field_access(%_{} = record, query_or_changeset) do
    if internal?(query_or_changeset) do
      record
    else
      record.calculations
      |> Enum.reduce(record, fn
        {{:__ash_fields_are_visible__, fields}, value}, record ->
          if value do
            record
          else
            Enum.reduce(fields, record, fn field, record ->
              type =
                case Ash.Resource.Info.field(query_or_changeset.resource, field) do
                  %Ash.Resource.Aggregate{} -> :aggregate
                  %Ash.Resource.Attribute{} -> :attribute
                  %Ash.Resource.Calculation{} -> :calculation
                end

              record
              |> Map.put(field, %Ash.ForbiddenField{field: field, type: type})
              |> replace_dynamic_loads(field, type, query_or_changeset)
            end)
          end
          |> Map.update!(
            :calculations,
            &Map.delete(&1, {:__ash_fields_are_visible__, fields})
          )

        _, record ->
          record
      end)
    end
  end

  defp replace_dynamic_loads(record, _, :aggregate, _), do: record

  defp replace_dynamic_loads(record, field, type, %Ash.Changeset{} = changeset)
       when type in [:attribute, :calculation] do
    query =
      changeset.resource
      |> Ash.Query.new()
      |> Ash.Query.load(changeset.load)

    replace_dynamic_loads(record, field, type, query)
  end

  defp replace_dynamic_loads(record, field, type, query)
       when type in [:attribute, :calculation] do
    Enum.reduce(
      query.calculations,
      record,
      fn
        {key, %{module: Ash.Resource.Calculation.LoadAttribute, opts: opts, load: load}},
        record ->
          if type == :attribute && opts[:attribute] == field do
            if load do
              Map.put(record, load, %Ash.ForbiddenField{field: load, type: type})
            else
              Map.update!(
                record,
                :calculations,
                &Map.put(&1, key, %Ash.ForbiddenField{field: field, type: type})
              )
            end
          else
            record
          end

        {key, %{calc_name: calc_name, load: load}}, record ->
          if calc_name == field and type == :calculation do
            if load do
              Map.put(record, load, %Ash.ForbiddenField{field: load, type: type})
            else
              Map.update!(
                record,
                :calculations,
                &Map.put(&1, key, %Ash.ForbiddenField{field: field, type: type})
              )
            end
          else
            record
          end

        _, record ->
          record
      end
    )
  end

  def select({:ok, results, instructions}, query) do
    {:ok, select(results, query), instructions}
  end

  def select({:ok, results}, query) do
    {:ok, select(results, query)}
  end

  def select({:error, error}, _query) do
    {:error, error}
  end

  def select(results, query) when is_list(results) do
    Enum.map(results, &select(&1, query))
  end

  def select(nil, _), do: nil

  def select(result, %{select: nil}) do
    result
  end

  def select(result, %{resource: resource, select: select}) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.flat_map(fn attribute ->
      if attribute.always_select? || attribute.primary_key? || attribute.name in select do
        []
      else
        [attribute.name]
      end
    end)
    |> Enum.reduce(result, fn key, record ->
      default_field_value =
        if Ash.Flags.ash_three?() do
          %Ash.NotSelected{field: key}
        else
          nil
        end

      Map.put(record, key, default_field_value)
    end)
    |> Ash.Resource.put_metadata(:selected, select)
  end

  def attributes_to_select(%{select: nil, resource: resource}) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.map(& &1.name)
  end

  def attributes_to_select(%{select: select, resource: resource}) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.flat_map(fn attribute ->
      if attribute.always_select? || attribute.primary_key? || attribute.name in select do
        [attribute.name]
      else
        []
      end
    end)
  end
end
