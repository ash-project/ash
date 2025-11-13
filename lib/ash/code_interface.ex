# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.CodeInterface do
  @moduledoc """
  Used to define the functions of a code interface for a resource.
  """

  @doc false
  def require_action(resource, interface) do
    action = Ash.Resource.Info.action(resource, interface.action || interface.name)

    if !action do
      # Get location info from the interface entity
      location = Spark.Dsl.Entity.anno(interface)

      raise Spark.Error.DslError,
        module: resource,
        location: location,
        message:
          "The interface of #{inspect(resource)} refers to a non-existent action #{interface.action || interface.name}",
        path: [:interfaces, :interface, interface.name]
    end

    action
  end

  @doc false
  def default_value(resource, action, key) do
    {field_type, field} =
      case Enum.find(action.arguments, fn argument ->
             argument.name == key
           end) do
        nil ->
          {:attribute, Ash.Resource.Info.attribute(resource, key)}

        argument ->
          {:argument, argument}
      end

    cond do
      field.allow_nil? && !(field.name in Map.get(action, :require_attributes, [])) ->
        :ok

      field.name in Map.get(action, :allow_nil_input, []) ->
        :ok

      !(field.name in Map.get(action, :accept, [])) ->
        :ok

      action.type == :create and not is_nil(Map.get(field, :default)) ->
        :ok

      true ->
        raise "Code interface for #{action.name} has optional argument #{key} but it is not optional"
    end

    default =
      if field_type == :argument do
        field.default
      else
        if action.type == :update || (action.type == :destroy && action.soft?) do
          if is_nil(field.update_default) do
            field.default
          else
            field.update_default
          end
        else
          field.default
        end
      end

    if is_function(default) do
      quote do
        unquote(Macro.escape(default)).()
      end
    else
      quote do
        unquote(Macro.escape(default))
      end
    end
  end

  @doc false
  def without_optional(keys) do
    Enum.map(keys, fn
      {:optional, key} ->
        key

      key ->
        key
    end)
  end

  @doc false
  def unwrap_calc_interface_args(keys, resource, arguments, function_head? \\ false) do
    {Enum.map(keys, &unwrap_calc_interface_arg_binding(resource, arguments, &1, function_head?)),
     Enum.map(keys, &unwrap_calc_interface_arg_access(&1))}
  end

  defp unwrap_calc_interface_arg_access({:optional, value}),
    do: unwrap_calc_interface_arg_access(value)

  defp unwrap_calc_interface_arg_access({:optional, value, _}),
    do: unwrap_calc_interface_arg_access(value)

  defp unwrap_calc_interface_arg_access(value) do
    case value do
      :_record ->
        [type: :_record, name: :record, value: {:record, [], Elixir}]

      {tag, name} ->
        [type: tag, name: name, value: {name, [], Elixir}]

      name ->
        [type: :both, name: name, value: {name, [], Elixir}]
    end
  end

  defp unwrap_calc_interface_arg_binding(resource, arguments, {:optional, binding}, head?) do
    access = unwrap_calc_interface_arg_binding(resource, arguments, binding, head?)

    if head? do
      {:\\, [], [access, default_calc_value(resource, arguments, binding)]}
    else
      access
    end
  end

  defp unwrap_calc_interface_arg_binding(
         resource,
         arguments,
         {:optional, binding, default},
         head?
       ) do
    access = unwrap_calc_interface_arg_binding(resource, arguments, binding, head?)

    if head? do
      {:\\, [], [access, default]}
    else
      access
    end
  end

  defp unwrap_calc_interface_arg_binding(_resource, _arguments, {tag, value}, _)
       when tag in [:arg, :ref] do
    {value, [], Elixir}
  end

  defp unwrap_calc_interface_arg_binding(resource, _arguments, :_record, false) do
    {:=, [],
     [
       {:%, [], [{:__aliases__, [alias: false], [resource]}, {:%{}, [], []}]},
       {:record, [], Elixir}
     ]}
  end

  defp unwrap_calc_interface_arg_binding(_resource, _arguments, value, _) do
    {value, [], Elixir}
  end

  @doc false
  def default_calc_value(_resource, arguments, {:arg, arg_name}) do
    arguments
    |> Enum.find(&(&1.name == arg_name))
    |> case do
      nil ->
        nil

      argument ->
        argument.default
    end
  end

  def default_calc_value(resource, _, {:ref, attribute}) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.find(&(&1.name == attribute))
    |> case do
      nil ->
        nil

      attribute ->
        attribute.default
    end
  end

  def default_calc_value(resource, arguments, name) do
    case default_calc_value(resource, arguments, {:arg, name}) do
      nil ->
        default_calc_value(resource, arguments, {:ref, name})

      value ->
        value
    end
  end

  @doc false
  def execute_action_with_result_handling(subject, opts, raise_on_error?, executor_fn) do
    result = executor_fn.(subject, opts)

    if raise_on_error? do
      case result do
        {:ok, val} -> val
        {:error, error} -> raise Ash.Error.to_error_class(error)
        %Ash.BulkResult{} = bulk -> bulk
        other -> other
      end
    else
      result
    end
  end

  @doc false
  def handle_params_and_opts_for_create(params, opts) do
    if params == [] and opts == nil do
      raise ArgumentError, """
      Cannot provide an empty list for params without also specifying options.

      We cannot tell the difference between an empty list of inputs and an empty list of options.

      If you are trying to provide an empty list of options,
      you should also specify empty `params`, i.e `function(..., %{}, params)`

      If you are trying to provide an empty list of records to create,
      you should also specify empty `opts`, i.e `function(..., params, [])`
      """
    else
      if Keyword.keyword?(params) and is_nil(opts) do
        {%{}, params}
      else
        {params || %{}, opts || []}
      end
    end
  end

  @doc false
  def handle_params_and_opts(params, opts) do
    keyword? = Keyword.keyword?(params)

    if keyword? and is_nil(opts) do
      {%{}, params}
    else
      if keyword? do
        {Map.new(params), opts || []}
      else
        {params || %{}, opts || []}
      end
    end
  end

  @doc """
  Resolves the subject (ActionInput) for a generic action.

  Handles validation, domain setting, and error accumulation for ActionInput subjects.

  ## Parameters
  - `input` - Map with `:opts` key containing keyword list
  - `params` - Action parameters
  - `custom_input_errors` - Accumulated custom validation errors
  - `resource` - The Ash resource module
  - `domain` - The Ash domain module
  - `action_name` - Name of the action to invoke

  ## Returns
  `{resolved_input, opts}` where:
  - `resolved_input` is an `%Ash.ActionInput{}` struct
  - `opts` is remaining options after extracting input-specific opts
  """
  def resolve_action_subject_for_action(
        input,
        params,
        custom_input_errors,
        resource,
        domain,
        action_name
      ) do
    {input_opts, opts} =
      Keyword.split(input[:opts] || [], [
        :input,
        :actor,
        :tenant,
        :authorize?,
        :tracer,
        :scope,
        :private_arguments
      ])

    {input_val, input_opts} = Keyword.pop(input_opts, :input)

    input_opts = Keyword.put(input_opts, :domain, domain)

    case input_val do
      %Ash.ActionInput{resource: ^resource} ->
        input_val

      %Ash.ActionInput{resource: other_resource} ->
        raise ArgumentError,
              "Action input resource #{inspect(other_resource)} does not match expected resource #{inspect(resource)}."

      _ ->
        input_val
    end

    resolved_input =
      (input_val || resource)
      |> Ash.ActionInput.for_action(action_name, params, input_opts)
      |> Ash.ActionInput.add_error(custom_input_errors)

    {resolved_input, opts}
  end

  @doc """
  Resolves the subject (Query) for a read action.

  Handles query building, filter application, and validation for read operations.

  ## Parameters
  - `query_input` - Map with `:opts` key containing keyword list
  - `params` - Action parameters
  - `custom_input_errors` - Accumulated custom validation errors
  - `filter_params` - Filter parameters to apply to the query
  - `resource` - The Ash resource module
  - `domain` - The Ash domain module
  - `action_name` - Name of the read action
  - `filter_keys` - List of filter keys to apply

  ## Returns
  `{query, opts}` where:
  - `query` is an `%Ash.Query{}` struct with filters and action applied
  - `opts` is remaining options after extracting query-specific opts
  """
  def resolve_action_subject_for_read(
        query_input,
        params,
        custom_input_errors,
        filter_params,
        resource,
        domain,
        action_name,
        filter_keys
      ) do
    {query_opts, opts} =
      Keyword.split(query_input[:opts] || [], [
        :query,
        :actor,
        :tenant,
        :authorize?,
        :tracer,
        :context,
        :scope
      ])

    {query_val, query_opts} = Keyword.pop(query_opts, :query)

    query_opts = Keyword.put(query_opts, :domain, domain)

    query =
      case query_val do
        %Ash.Query{resource: ^resource} = query ->
          query

        %Ash.Query{resource: other_resource} ->
          raise ArgumentError,
                "Query resource #{inspect(other_resource)} does not match expected resource #{inspect(resource)}."

        ^resource ->
          resource
          |> Ash.Query.new()

        other_resource
        when is_atom(other_resource) and not is_nil(other_resource) ->
          raise ArgumentError,
                "Query resource #{inspect(other_resource)} does not match expected resource #{inspect(resource)}."

        query ->
          Ash.Query.build(resource, query || [])
      end
      |> Ash.Query.add_error(custom_input_errors)

    query =
      if filter_keys && !Enum.empty?(filter_keys) do
        require Ash.Query

        query
        |> Ash.Query.for_read(action_name, params, query_opts)
        |> Ash.Query.do_filter(filter_params)
      else
        Ash.Query.for_read(query, action_name, params, query_opts)
      end
      |> Ash.Query.add_error(custom_input_errors)

    {query, opts}
  end

  @doc """
  Resolves the subject (Changeset or bulk tuple) for a create action.

  Handles changeset building and bulk operation detection based on params type.

  ## Parameters
  - `changeset_input` - Map with `:opts` key containing keyword list
  - `params` - Either a map (single create) or list (bulk create)
  - `custom_input_errors` - Accumulated custom validation errors
  - `resource` - The Ash resource module
  - `domain` - The Ash domain module
  - `action_name` - Name of the create action

  ## Returns
  `{changeset, changeset_opts, opts}` where:
  - `changeset` is an `%Ash.Changeset{}` or `{:bulk, inputs}` tuple
  - `changeset_opts` are changeset-specific options (actor, tenant, etc.)
  - `opts` is remaining options after extraction
  """
  def resolve_action_subject_for_create(
        changeset_input,
        params,
        custom_input_errors,
        resource,
        domain,
        action_name
      ) do
    {changeset_val, opts} = Keyword.pop(changeset_input[:opts] || [], :changeset)

    {changeset_opts, opts} =
      Keyword.split(opts, [
        :actor,
        :tenant,
        :scope,
        :authorize?,
        :tracer,
        :context,
        :skip_unknown_inputs,
        :private_arguments
      ])

    changeset_opts = Keyword.put(changeset_opts, :domain, domain)

    changeset =
      if is_map(params) do
        changeset_val
        |> Kernel.||(resource)
        |> case do
          %Ash.Changeset{resource: ^resource} ->
            changeset_val

          %Ash.Changeset{resource: _other_resource} ->
            raise ArgumentError,
                  "Changeset #{inspect(changeset_val)} does not match expected resource #{inspect(resource)}."

          other_resource
          when is_atom(other_resource) and other_resource != resource ->
            raise ArgumentError,
                  "Resource #{inspect(other_resource)} does not match expected resource #{inspect(resource)}."

          changeset ->
            changeset
        end
        |> Ash.Changeset.new()
        |> Ash.Changeset.add_error(custom_input_errors)
        |> Ash.Changeset.for_create(action_name, params, changeset_opts)
      else
        {:bulk, params}
      end

    {changeset, changeset_opts, opts}
  end

  @doc """
  Resolves the subject (Changeset or atomic tuple) for an update action.

  Handles both single-record updates and bulk/atomic operations based on input type.

  ## Parameters
  - `record_input` - Map with `:opts` and optional `:record` keys
  - `params` - Update parameters
  - `custom_input_errors` - Accumulated custom validation errors
  - `filter_params` - Filter parameters for atomic operations
  - `resource` - The Ash resource module
  - `domain` - The Ash domain module
  - `action_name` - Name of the update action
  - `require_reference?` - Whether a record reference is required
  - `filter_keys` - List of filter keys

  ## Returns
  `{changeset, changeset_opts, opts}` where:
  - `changeset` is an `%Ash.Changeset{}` or `{:atomic, method, id}` tuple
  - `changeset_opts` are changeset-specific options
  - `opts` is remaining options after extraction
  """
  def resolve_action_subject_for_update(
        record_input,
        params,
        custom_input_errors,
        filter_params,
        resource,
        domain,
        action_name,
        require_reference?,
        filter_keys
      ) do
    {changeset_opts, opts} =
      Keyword.split(record_input[:opts] || [], [
        :actor,
        :tenant,
        :scope,
        :authorize?,
        :tracer,
        :context,
        :skip_unknown_inputs,
        :private_arguments
      ])

    changeset_opts = Keyword.put(changeset_opts, :domain, domain)

    changeset =
      if Enum.empty?(filter_keys) and require_reference? do
        record = record_input[:record]

        record
        |> case do
          %Ash.Changeset{resource: ^resource} ->
            record
            |> Ash.Changeset.filter(filter_params)
            |> Ash.Changeset.add_error(custom_input_errors)
            |> Ash.Changeset.for_update(action_name, params, changeset_opts)

          %Ash.Changeset{resource: _other_resource} ->
            raise ArgumentError,
                  "Changeset #{inspect(record)} does not match expected resource #{inspect(resource)}."

          %struct{} = record when struct == resource ->
            record
            |> Ash.Changeset.new()
            |> Ash.Changeset.filter(filter_params)
            |> Ash.Changeset.add_error(custom_input_errors)
            |> Ash.Changeset.for_update(action_name, params, changeset_opts)

          %Ash.Query{} = query ->
            {:atomic, :query, query}

          %other_resource{} when other_resource != resource ->
            raise ArgumentError,
                  "Record #{inspect(record)} does not match expected resource #{inspect(resource)}."

          [{_key, _val} | _] = id ->
            {:atomic, :id, id}

          list when is_list(list) ->
            {:atomic, :stream, list}

          other ->
            {:atomic, :id, other}
        end
      else
        {:atomic, :query, Ash.Query.do_filter(resource, filter_params)}
      end

    {changeset, changeset_opts, opts}
  end

  @doc """
  Resolves the subject (Changeset or atomic tuple) for a destroy action.

  Handles both single-record destruction and bulk/atomic operations.

  ## Parameters
  - `record_input` - Map with `:opts` and optional `:record` keys
  - `params` - Destroy parameters
  - `custom_input_errors` - Accumulated custom validation errors
  - `filter_params` - Filter parameters for atomic operations
  - `resource` - The Ash resource module
  - `domain` - The Ash domain module
  - `action_name` - Name of the destroy action
  - `require_reference?` - Whether a record reference is required

  ## Returns
  `{changeset, changeset_opts, opts}` where:
  - `changeset` is an `%Ash.Changeset{}` or `{:atomic, method, id}` tuple
  - `changeset_opts` are changeset-specific options
  - `opts` is remaining options after extraction
  """
  def resolve_action_subject_for_destroy(
        record_input,
        params,
        custom_input_errors,
        filter_params,
        resource,
        domain,
        action_name,
        require_reference?
      ) do
    {changeset_opts, opts} =
      Keyword.split(record_input[:opts] || [], [
        :actor,
        :tenant,
        :scope,
        :authorize?,
        :tracer,
        :context,
        :skip_unknown_inputs,
        :private_arguments
      ])

    changeset_opts = Keyword.put(changeset_opts, :domain, domain)

    changeset =
      if require_reference? do
        record = record_input[:record]

        record
        |> case do
          %Ash.Changeset{resource: ^resource} ->
            record
            |> Ash.Changeset.filter(filter_params)
            |> Ash.Changeset.add_error(custom_input_errors)
            |> Ash.Changeset.for_destroy(action_name, params, changeset_opts)

          %Ash.Changeset{resource: _other_resource} ->
            raise ArgumentError,
                  "Changeset #{inspect(record)} does not match expected resource #{inspect(resource)}."

          %struct{} = record when struct == resource ->
            record
            |> Ash.Changeset.new()
            |> Ash.Changeset.filter(filter_params)
            |> Ash.Changeset.add_error(custom_input_errors)
            |> Ash.Changeset.for_destroy(action_name, params, changeset_opts)

          %Ash.Query{} = query ->
            {:atomic, :query, query}

          %other_resource{} when other_resource != resource ->
            raise ArgumentError,
                  "Record #{inspect(record)} does not match expected resource #{inspect(resource)}."

          [{_key, _val} | _] = id ->
            {:atomic, :id, id}

          list when is_list(list) ->
            {:atomic, :stream, list}

          other ->
            {:atomic, :id, other}
        end
      else
        {:atomic, :query, Ash.Query.do_filter(resource, filter_params)}
      end

    {changeset, changeset_opts, opts}
  end

  @doc """
  Executes a create action, handling both single-record and bulk create operations.

  This function is called at runtime by generated code interface functions. It handles:
  - Single record creation via `Ash.create/2` or `Ash.create!/2`
  - Bulk creation via `Ash.bulk_create/4` or `Ash.bulk_create!/4`
  - Error handling based on `raise_on_error?` flag
  - Custom input validation and errors

  ## Parameters
  - `changeset` - Either an `%Ash.Changeset{}` or `{:bulk, inputs}` tuple
  - `custom_input_errors` - List of custom validation errors from input processing
  - `changeset_opts` - Options to pass to changeset creation (actor, tenant, etc.)
  - `opts` - Additional options including `:bulk_options`
  - `resource` - The Ash resource module
  - `action_name` - Name of the create action to invoke
  - `raise_on_error?` - If true, raises on error; if false, returns `{:ok, result}` tuples

  ## Returns
  - When `raise_on_error?` is false: `{:ok, record}`, `{:error, error}`, or `%Ash.BulkResult{}`
  - When `raise_on_error?` is true: `record`, `%Ash.BulkResult{}`, or raises exception
  """
  def execute_create_action(
        changeset,
        custom_input_errors,
        changeset_opts,
        opts,
        resource,
        action_name,
        raise_on_error?
      ) do
    case changeset do
      {:bulk, inputs} ->
        if Enum.any?(custom_input_errors) do
          if raise_on_error? do
            raise Ash.Error.to_error_class(custom_input_errors)
          else
            %Ash.BulkResult{
              errors: [Ash.Error.to_error_class(custom_input_errors)],
              error_count: 1
            }
          end
        else
          bulk_opts =
            opts
            |> Keyword.delete(:bulk_options)
            |> Keyword.put(:notify?, true)
            |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
            |> Enum.concat(changeset_opts)

          if raise_on_error? do
            Ash.bulk_create!(inputs, resource, action_name, bulk_opts)
          else
            Ash.bulk_create(inputs, resource, action_name, bulk_opts)
          end
        end

      changeset ->
        cleaned_opts = Keyword.delete(opts, :bulk_options)

        if raise_on_error? do
          Ash.create!(changeset, cleaned_opts)
        else
          Ash.create(changeset, cleaned_opts)
        end
    end
  end

  @doc """
  Executes a destroy action, handling both single-record and bulk/atomic destroy operations.

  This function is called at runtime by generated code interface functions. It handles:
  - Single record destruction via `Ash.destroy/2` or `Ash.destroy!/2`
  - Bulk destruction via `Ash.bulk_destroy/4` or `Ash.bulk_destroy!/4`
  - Atomic operations with method detection (:id, :query, :stream)
  - Return destroyed record handling based on `:return_destroyed?` option
  - Error handling based on `raise_on_error?` flag

  ## Parameters
  - `changeset` - Either an `%Ash.Changeset{}` or `{:atomic, method, id}` tuple
  - `custom_input_errors` - List of custom validation errors
  - `changeset_opts` - Options for changeset (actor, tenant, authorize?, etc.)
  - `opts` - Additional options including `:bulk_options`, `:return_destroyed?`, `:return_notifications?`
  - `params` - Action parameters
  - `filter_params` - Filter parameters for bulk operations
  - `resource` - The Ash resource module
  - `action_name` - Name of the destroy action
  - `interface_get` - Whether this is a get? operation (expects single record)
  - `module` - Module for data layer capabilities check
  - `raise_on_error?` - If true, raises on error; if false, returns tuples

  ## Returns
  - When `raise_on_error?` is false: `:ok`, `{:ok, record}`, `{:ok, notifications}`, `{:error, error}`, or `%Ash.BulkResult{}`
  - When `raise_on_error?` is true: `:ok`, `record`, `notifications`, `%Ash.BulkResult{}`, or raises exception
  """
  def execute_destroy_action(
        changeset,
        custom_input_errors,
        changeset_opts,
        opts,
        params,
        filter_params,
        resource,
        action_name,
        interface_get,
        module,
        raise_on_error?
      ) do
    case changeset do
      {:atomic, method, id} ->
        if Enum.any?(custom_input_errors) do
          if raise_on_error? do
            raise Ash.Error.to_error_class(custom_input_errors)
          else
            %Ash.BulkResult{
              errors: [Ash.Error.to_error_class(custom_input_errors)],
              error_count: 1
            }
          end
        else
          bulk_opts =
            opts
            |> Keyword.drop([:bulk_options, :return_destroyed?])
            |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
            |> Enum.concat(changeset_opts)
            |> Keyword.put(:resource, resource)
            |> then(fn bulk_opts ->
              if method == :id || interface_get do
                authorize_with =
                  if Ash.DataLayer.data_layer_can?(module, :expr_error) do
                    :error
                  else
                    :filter
                  end

                bulk_opts
                |> Keyword.put(:return_records?, true)
                |> Keyword.put(:return_errors?, true)
                |> Keyword.put(:allow_stream_with, :full_read)
                |> Keyword.put_new(:authorize_with, authorize_with)
                |> Keyword.put(:notify?, true)
              else
                Keyword.put(bulk_opts, :return_records?, opts[:return_destroyed?])
              end
            end)
            |> Keyword.put_new(:strategy, [:atomic, :stream, :atomic_batches])

          bulk_opts =
            if method in [:stream, :query] do
              Keyword.put(bulk_opts, :filter, filter_params)
            else
              bulk_opts
            end

          case bulk_query(resource, method, id) do
            {:ok, query} ->
              result =
                if raise_on_error? do
                  Ash.bulk_destroy!(query, action_name, params, bulk_opts)
                else
                  Ash.bulk_destroy(query, action_name, params, bulk_opts)
                end

              case result do
                %Ash.BulkResult{} = result
                when method in [:stream, :query] and not interface_get ->
                  result

                %Ash.BulkResult{status: :success, records: [_, _ | _] = records}
                when interface_get ->
                  error =
                    Ash.Error.Invalid.MultipleResults.exception(
                      count: Enum.count(records),
                      query: query
                    )

                  if raise_on_error? do
                    raise Ash.Error.to_error_class(error)
                  else
                    {:error, error}
                  end

                %Ash.BulkResult{status: :success, records: [record]} = result ->
                  if opts[:return_destroyed?] do
                    if raise_on_error? do
                      if opts[:return_notifications?] do
                        {record, result.notifications}
                      else
                        record
                      end
                    else
                      if opts[:return_notifications?] do
                        {:ok, record, result.notifications}
                      else
                        {:ok, record}
                      end
                    end
                  else
                    if raise_on_error? do
                      if opts[:return_notifications?] do
                        result.notifications
                      else
                        :ok
                      end
                    else
                      if opts[:return_notifications?] do
                        {:ok, result.notifications}
                      else
                        :ok
                      end
                    end
                  end

                %Ash.BulkResult{status: :success, records: empty}
                when empty in [[], nil] and (interface_get or method == :id) ->
                  error =
                    Ash.Error.Query.NotFound.exception(
                      resource: resource,
                      primary_key: id
                    )

                  if raise_on_error? do
                    raise Ash.Error.to_error_class(error)
                  else
                    {:error, Ash.Error.to_error_class(error)}
                  end

                %Ash.BulkResult{status: :success, records: empty}
                when empty in [[], nil] ->
                  if opts[:return_destroyed?] do
                    error =
                      Ash.Error.Query.NotFound.exception(
                        resource: resource,
                        primary_key: id
                      )

                    if raise_on_error? do
                      raise Ash.Error.to_error_class(error)
                    else
                      {:error, Ash.Error.to_error_class(error)}
                    end
                  else
                    if raise_on_error? do
                      if opts[:return_notifications?] do
                        result.notifications
                      else
                        :ok
                      end
                    else
                      if opts[:return_notifications?] do
                        {:ok, result.notifications}
                      else
                        :ok
                      end
                    end
                  end

                %Ash.BulkResult{status: :error, errors: errors} ->
                  if raise_on_error? do
                    raise Ash.Error.to_error_class(errors)
                  else
                    {:error, Ash.Error.to_error_class(errors)}
                  end
              end

            {:error, error} ->
              if raise_on_error? do
                raise Ash.Error.to_error_class(error)
              else
                {:error, Ash.Error.to_error_class(error)}
              end
          end
        end

      changeset ->
        cleaned_opts = Keyword.delete(opts, :bulk_options)

        if raise_on_error? do
          Ash.destroy!(changeset, cleaned_opts)
        else
          Ash.destroy(changeset, cleaned_opts)
        end
    end
  end

  @doc """
  Executes an update action, handling both single-record and bulk/atomic update operations.

  This function is called at runtime by generated code interface functions. It handles:
  - Single record updates via `Ash.update/2` or `Ash.update!/2`
  - Bulk updates via `Ash.bulk_update/4` or `Ash.bulk_update!/4`
  - Atomic operations with method detection (:id, :query, :stream)
  - Result extraction for get? operations
  - Notification handling with `:return_notifications?` option
  - Error handling based on `raise_on_error?` flag

  ## Parameters
  - `changeset` - Either an `%Ash.Changeset{}` or `{:atomic, method, id}` tuple
  - `custom_input_errors` - List of custom validation errors
  - `changeset_opts` - Options for changeset (actor, tenant, authorize?, etc.)
  - `opts` - Additional options including `:bulk_options`, `:return_notifications?`
  - `params` - Action parameters to update
  - `filter_params` - Filter parameters for bulk operations
  - `resource` - The Ash resource module
  - `action_name` - Name of the update action
  - `interface_get` - Whether this is a get? operation (expects single record)
  - `module` - Module for data layer capabilities check
  - `raise_on_error?` - If true, raises on error; if false, returns tuples

  ## Returns
  - When `raise_on_error?` is false: `{:ok, record}`, `{:ok, record, notifications}`, `{:error, error}`, or `%Ash.BulkResult{}`
  - When `raise_on_error?` is true: `record`, `{record, notifications}`, `%Ash.BulkResult{}`, or raises exception
  """
  def execute_update_action(
        changeset,
        custom_input_errors,
        changeset_opts,
        opts,
        params,
        filter_params,
        resource,
        action_name,
        interface_get,
        module,
        raise_on_error?
      ) do
    case changeset do
      {:atomic, method, id} ->
        if Enum.any?(custom_input_errors) do
          if raise_on_error? do
            raise Ash.Error.to_error_class(custom_input_errors)
          else
            %Ash.BulkResult{
              errors: [Ash.Error.to_error_class(custom_input_errors)],
              error_count: 1
            }
          end
        else
          bulk_opts =
            opts
            |> Keyword.drop([:bulk_options, :atomic_upgrade?])
            |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
            |> Enum.concat(changeset_opts)
            |> Keyword.put(:resource, resource)
            |> then(fn bulk_opts ->
              if method == :id || interface_get do
                authorize_with =
                  if Ash.DataLayer.data_layer_can?(module, :expr_error) do
                    :error
                  else
                    :filter
                  end

                bulk_opts
                |> Keyword.put(:return_records?, true)
                |> Keyword.put(:return_errors?, true)
                |> then(fn opts ->
                  if raise_on_error? do
                    Keyword.put(opts, :allow_stream_with, :full_read)
                  else
                    opts
                  end
                end)
                |> Keyword.put_new(:authorize_with, authorize_with)
                |> Keyword.put(:notify?, true)
              else
                bulk_opts
              end
            end)
            |> Keyword.put_new(:strategy, [:atomic, :stream, :atomic_batches])

          bulk_opts =
            if method in [:stream, :query] do
              Keyword.put(bulk_opts, :filter, filter_params)
            else
              bulk_opts
            end

          case bulk_query(resource, method, id) do
            {:ok, query} ->
              result =
                if raise_on_error? do
                  Ash.bulk_update!(query, action_name, params, bulk_opts)
                else
                  Ash.bulk_update(query, action_name, params, bulk_opts)
                end

              case result do
                %Ash.BulkResult{} = result
                when method in [:stream, :query] and not interface_get ->
                  result

                %Ash.BulkResult{status: :success, records: [_, _ | _] = records}
                when interface_get ->
                  error =
                    Ash.Error.Invalid.MultipleResults.exception(
                      count: Enum.count(records),
                      query: query
                    )

                  if raise_on_error? do
                    raise Ash.Error.to_error_class(error)
                  else
                    {:error, error}
                  end

                %Ash.BulkResult{status: :success, records: [record]} = result ->
                  if raise_on_error? do
                    if opts[:return_notifications?] do
                      {record, result.notifications}
                    else
                      record
                    end
                  else
                    if opts[:return_notifications?] do
                      {:ok, record, result.notifications}
                    else
                      {:ok, record}
                    end
                  end

                %Ash.BulkResult{status: :success, records: []} ->
                  error =
                    Ash.Error.Query.NotFound.exception(
                      resource: resource,
                      primary_key: id
                    )

                  if raise_on_error? do
                    raise Ash.Error.to_error_class(error)
                  else
                    {:error, Ash.Error.to_error_class(error)}
                  end

                %Ash.BulkResult{status: :error, errors: errors} ->
                  if raise_on_error? do
                    raise Ash.Error.to_error_class(errors)
                  else
                    {:error, Ash.Error.to_error_class(errors)}
                  end
              end

            {:error, error} ->
              if raise_on_error? do
                raise Ash.Error.to_error_class(error)
              else
                {:error, Ash.Error.to_error_class(error)}
              end
          end
        end

      changeset ->
        cleaned_opts = Keyword.delete(opts, :bulk_options)

        if raise_on_error? do
          Ash.update!(changeset, cleaned_opts)
        else
          Ash.update(changeset, cleaned_opts)
        end
    end
  end

  @doc """
  Executes a calculation, handling both bang and non-bang variants.

  This function is called at runtime by generated code interface calculation functions.
  It consolidates the logic for both safe and bang calculation functions, eliminating
  duplication between the two variants.

  ## Parameters
  - `resource` - The Ash resource module
  - `domain` - The Ash domain module
  - `calculation_name` - Name of the calculation to execute
  - `refs` - Referenced calculations (from process_calc_args)
  - `arguments` - Calculation arguments (from process_calc_args)
  - `record` - Optional record to calculate on (from process_calc_args)
  - `custom_input_errors` - List of custom validation errors (from process_calc_args)
  - `opts` - Additional calculation options
  - `raise_on_error?` - If true, raises on errors; if false, returns {:error, error}

  ## Returns
  - When `raise_on_error?` is `true`: Returns result or raises on error
  - When `raise_on_error?` is `false`: Returns `result` or `{:error, error}`

  ## Example
  ```elixir
  # Bang version (raises on error)
  Ash.CodeInterface.execute_calculation(
    MyResource, MyDomain, :calculate_total,
    refs, args, record, errors, opts, true
  )

  # Safe version (returns {:error, error})
  Ash.CodeInterface.execute_calculation(
    MyResource, MyDomain, :calculate_total,
    refs, args, record, errors, opts, false
  )
  ```
  """
  def execute_calculation(
        resource,
        domain,
        calculation_name,
        refs,
        arguments,
        record,
        custom_input_errors,
        opts,
        raise_on_error?
      ) do
    case custom_input_errors do
      [] ->
        calc_opts = [domain: domain, refs: refs, args: arguments, record: record] ++ opts

        if raise_on_error? do
          Ash.calculate!(resource, calculation_name, calc_opts)
        else
          Ash.calculate(resource, calculation_name, calc_opts)
        end

      errors ->
        if raise_on_error? do
          raise Ash.Error.to_error_class(errors)
        else
          {:error, Ash.Error.to_error_class(errors)}
        end
    end
  end

  @doc false
  def process_calc_args(
        arg_access,
        opts,
        exclude_inputs,
        custom_inputs,
        resource,
        interface_calculation,
        arg_bindings_count
      ) do
    {refs, arguments, record} =
      Enum.reduce(
        arg_access,
        {opts[:refs] || %{}, opts[:args] || %{}, nil},
        fn config, {refs, arguments, record} ->
          case config[:type] do
            :_record ->
              {refs, arguments, config[:value]}

            :both ->
              {Map.put(refs, config[:name], config[:value]),
               Map.put(arguments, config[:name], config[:value]), record}

            :ref ->
              {Map.put(refs, config[:name], config[:value]), arguments, record}

            :arg ->
              {refs, Map.put(arguments, config[:name], config[:value]), record}
          end
        end
      )

    case Enum.filter(exclude_inputs, fn input ->
           Map.has_key?(arguments, input) || Map.has_key?(arguments, to_string(input))
         end) do
      [] ->
        :ok

      inputs ->
        raise ArgumentError,
              "Input(s) `#{Enum.join(inputs, ", ")}` not accepted by #{inspect(resource)}.#{interface_calculation}/#{arg_bindings_count + 1}"
    end

    {arguments, custom_input_errors} =
      handle_custom_inputs(
        arguments,
        custom_inputs,
        resource
      )

    {refs, arguments, record, custom_input_errors}
  end

  @doc false
  # sobelow_skip ["DOS.BinToAtom", "DOS.StringToAtom"]
  def resolve_calc_method_names(name) do
    if name |> to_string() |> String.ends_with?("?") do
      safe_name = name |> to_string() |> String.trim_trailing("?") |> String.to_atom()
      bang_name = name
      {safe_name, bang_name}
    else
      safe_name = name
      bang_name = "#{name}!" |> String.to_atom()
      {safe_name, bang_name}
    end
  end

  # A common pattern is for a function to have both optional parameters and
  # optional options. This usually comes in the form of two defaults:
  #
  #   * An empty map for params.
  #   * An empty list for options.
  #
  # With those two defaults in mind, this function will decipher, from two inputs,
  # which should be parameters and which should be options.
  #
  # Parameters can take one of two primary forms:
  #
  #   1. A map.
  #   2. A list of maps for bulk operations.
  #
  # Additionally, if options are set explicitly (i.e. at least one option has
  # been set), a keyword list will be converted to a map.
  #
  # ## Examples
  #
  #     iex> params_and_opts(%{key: :param}, [key: :opt])
  #     {%{key: :param}, [key: :opt]}
  #
  #     iex> params_and_opts([key: :opt], [])
  #     {%{}, [key: :opt]}
  #
  #     iex> params_and_opts([], [])
  #     {[], []}
  #
  #     iex> params_and_opts([%{key: :param}], [])
  #     {[%{key: :param}], []}
  #
  #     iex> params_and_opts([key: :param], [key: :opt])
  #     {%{key: :param}, [key: :opt]}
  @doc false
  def resolve_params_opts_and_filters(
        params_or_opts,
        opts,
        default_options,
        interface_options,
        arg_params,
        exclude_inputs,
        custom_inputs,
        resource,
        interface_name,
        interface_arity,
        filter_params
      ) do
    {params, opts} =
      params_and_opts(
        params_or_opts,
        opts,
        fn opts ->
          default_opts =
            case default_options do
              fun when is_function(fun, 0) -> fun.()
              static_options -> static_options
            end

          opts
          |> merge_default_opts(default_opts)
          |> interface_options.validate!()
          |> interface_options.to_options()
        end
      )

    params =
      if is_list(params) do
        Enum.map(params, fn item ->
          if is_map(item) do
            Map.merge(item, arg_params)
          else
            raise ArgumentError, """
            Expected `params` to be a map or a list of maps.
            Got:  #{inspect(params)}
            """
          end
        end)
      else
        if is_map(params) do
          Map.merge(params, arg_params)
        else
          raise ArgumentError, """
          Expected `params` to be a map or a list of maps.
          Got:  #{inspect(params)}
          """
        end
      end

    case Enum.filter(exclude_inputs, fn input ->
           if is_list(params) do
             Enum.any?(
               params,
               &(Map.has_key?(&1, input) || Map.has_key?(&1, to_string(input)))
             )
           else
             Map.has_key?(params, input) || Map.has_key?(params, to_string(input))
           end
         end) do
      [] ->
        :ok

      inputs ->
        raise ArgumentError,
              "Input(s) `#{Enum.join(inputs, ", ")}` not accepted by #{inspect(resource)}.#{interface_name}/#{interface_arity}"
    end

    {params, custom_input_errors} =
      handle_custom_inputs(params, custom_inputs, resource)

    {params, custom_input_errors, opts, filter_params}
  end

  @doc false
  @spec params_and_opts(params_or_opts :: map() | [map()] | keyword(), keyword()) ::
          {params :: map() | [map()], opts :: keyword()}
  def params_and_opts(%{} = params, opts), do: {params, opts}

  def params_and_opts([], opts), do: {[], opts}

  def params_and_opts([%{} | _] = params_list, opts), do: {params_list, opts}

  def params_and_opts(opts, []), do: {%{}, opts}

  def params_and_opts(params_or_list, opts) do
    params =
      if Keyword.keyword?(params_or_list),
        do: Map.new(params_or_list),
        else: params_or_list

    {params, opts}
  end

  @doc """
  See `params_and_opts/2`.

  Adds a post process function that can takes the opts and can further process,
  validate, or transform them.
  """
  @spec params_and_opts(
          params_or_opts :: map() | [map()] | keyword(),
          keyword(),
          (keyword() -> keyword())
        ) ::
          {params :: map() | [map()], opts :: keyword()}
  def params_and_opts(params_or_opts, maybe_opts, post_process_opts_fn)
      when is_function(post_process_opts_fn, 1) do
    params_or_opts
    |> params_and_opts(maybe_opts)
    |> then(fn {params, opts} ->
      {params,
       opts
       |> post_process_opts_fn.()}
    end)
  end

  @deep_merge_keys [:bulk_options, :page]
  # Selectively merges default opts into client-provided opts. For most keys,
  # the value in opts will be used instead of the default if provided. However,
  # certain options have special behavior:
  #
  #   * #{Enum.map_join(@deep_merge_keys, ", ", &"`:#{&1}`")} - These
  #     options are deep merged, so if the default is a keyword list and the
  #     client value is a keyword list, they'll be merged.
  #   * `:load` - The default value and the client value will be combined in this
  #     case.
  #
  # ## Examples
  #
  #     iex> merge_default_opts([key1: 1], key2: 2)
  #     [key2: 2, key1: 1]
  #
  #     iex> merge_default_opts([key2: :client], key1: :default, key2: :default)
  #     [key2: :client, key1: :default]
  #
  #     iex> merge_default_opts([page: false], page: [limit: 100])
  #     [page: false]
  #
  #     iex> merge_default_opts([page: [offset: 2]], page: [limit: 100])
  #     [page: [limit: 100, offset: 2]]
  #
  #     iex> merge_default_opts([load: [:calc2, :rel4]], load: [:calc1, rel1: [:rel2, :rel3]])
  #     [load: [:calc1, {:rel1, [:rel2, :rel3]}, :calc2, :rel4]]
  @doc false
  @spec merge_default_opts(keyword(), keyword()) :: keyword()
  def merge_default_opts(opts, default_opts) do
    Enum.reduce(default_opts, opts, fn {k, default}, opts ->
      opts
      |> Keyword.fetch(k)
      |> case do
        :error -> default
        {:ok, value} -> merge_default_opt(k, default, value)
      end
      |> then(&Keyword.put(opts, k, &1))
    end)
  end

  defp merge_default_opt(:load, default, value) do
    List.wrap(default) ++ List.wrap(value)
  end

  defp merge_default_opt(key, default, value)
       when key in @deep_merge_keys and is_list(default) and is_list(value),
       do: Keyword.merge(default, value)

  defp merge_default_opt(_key, _default, value), do: value

  @doc """
  Defines the code interface for a given resource + domain combination in the current module. For example:

  ```elixir
  defmodule MyApp.Accounting do
    require Ash.CodeInterface

    Ash.CodeInterface.define_interface(MyApp.Accounting, MyApp.Accounting.Transaction)
    Ash.CodeInterface.define_interface(MyApp.Accounting, MyApp.Accounting.Account)
    Ash.CodeInterface.define_interface(MyApp.Accounting, MyApp.Accounting.Invoice)
  end
  ```
  """
  defmacro define_interface(domain, resource, definitions \\ nil) do
    quote bind_quoted: [domain: domain, resource: resource, definitions: definitions],
          generated: true,
          location: :keep do
      calculation_interfaces =
        case definitions do
          nil ->
            Ash.Resource.Info.calculation_interfaces(resource)

          definitions ->
            Enum.filter(definitions, &match?(%Ash.Resource.CalculationInterface{}, &1))
        end

      interfaces =
        case definitions do
          nil ->
            Ash.Resource.Info.interfaces(resource)

          definitions ->
            Enum.filter(definitions, &match?(%Ash.Resource.Interface{}, &1))
        end

      interfaces_for_defaults =
        Enum.group_by(calculation_interfaces, fn interface ->
          {interface.name, Enum.count(interface.args, &is_atom/1), Enum.count(interface.args)}
        end)

      calculation_lookup = fn calc_name ->
        Ash.Resource.Info.calculation(resource, calc_name)
      end

      for {{name, arity, optional_arity}, interfaces} <- interfaces_for_defaults do
        args =
          case interfaces do
            [%{args: args, calculation: calculation}] ->
              calculation = calculation_lookup.(calculation)

              {arg_bindings, _arg_access} =
                args
                |> Kernel.||([])
                |> Ash.CodeInterface.unwrap_calc_interface_args(
                  resource,
                  calculation.arguments,
                  true
                )

              arg_bindings

            multiple ->
              multiple
              |> Enum.map(fn interface ->
                calculation = calculation_lookup.(interface.calculation)

                interface.args
                |> Enum.flat_map(fn
                  {:optional, value} ->
                    [Ash.CodeInterface.default_calc_value(resource, calculation.arguments, value)]

                  {:optional, _, value} ->
                    [value]

                  _ ->
                    []
                end)
              end)
              |> Enum.uniq()
              |> case do
                [_] ->
                  interface = hd(multiple)
                  calculation = calculation_lookup.(interface.calculation)

                  {arg_bindings, _arg_access} =
                    interface.args
                    |> Kernel.||([])
                    |> Ash.CodeInterface.unwrap_calc_interface_args(
                      resource,
                      calculation.arguments,
                      true
                    )

                  arg_bindings

                _duplicates ->
                  raise """
                  The generated function #{name}/#{arity + optional_arity} would have
                  multiple different sets of default values for arguments. Please use a different
                  name for conflicting code interface functions.
                  """
              end
          end

        {safe_name, bang_name} = Ash.CodeInterface.resolve_calc_method_names(name)

        def unquote(bang_name)(unquote_splicing(args), opts \\ [])
        def unquote(safe_name)(unquote_splicing(args), opts \\ [])
      end

      for interface <- calculation_interfaces do
        calculation = calculation_lookup.(interface.calculation)
        custom_inputs = Macro.escape(interface.custom_inputs)
        exclude_inputs = interface.exclude_inputs || []
        interface_calculation = interface.calculation
        interface_args = interface.args

        {arg_bindings, arg_access} =
          interface_args
          |> Kernel.||([])
          |> Ash.CodeInterface.unwrap_calc_interface_args(resource, calculation.arguments)

        {safe_name, bang_name} = Ash.CodeInterface.resolve_calc_method_names(interface.name)

        opts_location = Enum.count(arg_bindings)
        interface_options = Ash.Resource.Interface.interface_options(:calculate, nil)

        bang_body =
          quote bind_quoted: [
                  arg_access: arg_access,
                  exclude_inputs: exclude_inputs,
                  custom_inputs: custom_inputs,
                  resource: resource,
                  interface_calculation: interface_calculation,
                  arg_bindings_count: Enum.count(arg_bindings),
                  domain: domain
                ] do
            {refs, arguments, record, custom_input_errors} =
              Ash.CodeInterface.process_calc_args(
                arg_access,
                opts,
                exclude_inputs,
                custom_inputs,
                resource,
                interface_calculation,
                arg_bindings_count
              )

            Ash.CodeInterface.execute_calculation(
              resource,
              domain,
              interface_calculation,
              refs,
              arguments,
              record,
              custom_input_errors,
              opts,
              true
            )
          end

        safe_body =
          quote bind_quoted: [
                  arg_access: arg_access,
                  exclude_inputs: exclude_inputs,
                  custom_inputs: custom_inputs,
                  resource: resource,
                  interface_calculation: interface_calculation,
                  arg_bindings_count: Enum.count(arg_bindings),
                  domain: domain
                ] do
            {refs, arguments, record, custom_input_errors} =
              Ash.CodeInterface.process_calc_args(
                arg_access,
                opts,
                exclude_inputs,
                custom_inputs,
                resource,
                interface_calculation,
                arg_bindings_count
              )

            Ash.CodeInterface.execute_calculation(
              resource,
              domain,
              interface_calculation,
              refs,
              arguments,
              record,
              custom_input_errors,
              opts,
              false
            )
          end

        @doc """
             #{calculation.description || "Calculates #{calculation.name} action on #{inspect(resource)}."}

             #{Ash.CodeInterface.describe_calculation(resource, calculation, interface_args, exclude_inputs, interface.custom_inputs)}

             ### Options

             #{interface_options.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        @doc spark_opts: [
               {opts_location, interface_options.schema()}
             ]
        def unquote(bang_name)(unquote_splicing(arg_bindings), opts) do
          unquote(bang_body)
        end

        @doc """
             #{calculation.description || "Calculates #{calculation.name} action on #{inspect(resource)}."}

             #{Ash.CodeInterface.describe_calculation(resource, calculation, interface_args, exclude_inputs, interface.custom_inputs)}

             ### Options

             #{interface_options.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        @doc spark_opts: [
               {opts_location, interface_options.schema()}
             ]
        def unquote(safe_name)(unquote_splicing(arg_bindings), opts) do
          unquote(safe_body)
        end
      end

      for interface <- interfaces do
        action = Ash.CodeInterface.require_action(resource, interface)
        action_type = action.type
        action_name = action.name
        action_description = action.description

        filter_keys =
          cond do
            action_type not in [:read, :update, :destroy] ->
              []

            interface.get_by_identity ->
              Ash.Resource.Info.identity(resource, interface.get_by_identity).keys

            interface.get_by ->
              interface.get_by

            true ->
              []
          end

        interface_args = interface.args || []
        arg_names = Ash.CodeInterface.without_optional(interface_args)

        all_args =
          List.wrap(filter_keys) ++ arg_names

        arg_vars = Enum.map(all_args, &{&1, [], Elixir})

        arg_params = {:%{}, [], Enum.map(arg_names, fn arg -> {arg, {arg, [], Elixir}} end)}
        filter_params = {:%{}, [], Enum.map(filter_keys, fn key -> {key, {key, [], Elixir}} end)}

        interface_exclude_inputs = interface.exclude_inputs || []
        interface_custom_inputs = Macro.escape(interface.custom_inputs)
        interface_name_atom = interface.name
        interface_get = interface.get?
        interface_not_found_error = interface.not_found_error?
        interface_require_reference = interface.require_reference?
        interface_default_options = Macro.escape(interface.default_options)

        arg_vars_function =
          filter_keys
          |> List.wrap()
          |> Enum.concat(interface_args)
          |> Enum.map(fn
            {:optional, key} ->
              default = Ash.CodeInterface.default_value(resource, action, key)
              {:\\, [], [{key, [], Elixir}, default]}

            key ->
              {key, [], Elixir}
          end)

        if Enum.uniq(all_args) != all_args do
          raise """
          Arguments #{inspect(all_args)} for #{interface_name_atom} are not unique!
          """
        end

        interface_get =
          if Map.get(action, :get?) do
            true
          else
            interface_get
          end

        interface_options =
          Ash.Resource.Interface.interface_options(action_type, %{interface | get?: interface_get})

        interface_args_count = Enum.count(interface_args)

        resolve_params_and_opts =
          quote bind_quoted: [
                  interface_default_options: interface_default_options,
                  interface_options: interface_options,
                  arg_params: arg_params,
                  interface_exclude_inputs: interface_exclude_inputs,
                  interface_custom_inputs: interface_custom_inputs,
                  resource: resource,
                  interface_name_atom: interface_name_atom,
                  interface_args_count_plus_2: interface_args_count + 2,
                  filter_params: filter_params
                ] do
            {params, custom_input_errors, opts, filter_params} =
              Ash.CodeInterface.resolve_params_opts_and_filters(
                params_or_opts,
                opts,
                interface_default_options,
                interface_options,
                arg_params,
                interface_exclude_inputs,
                interface_custom_inputs,
                resource,
                interface_name_atom,
                interface_args_count_plus_2,
                filter_params
              )
          end

        {subject, subject_args, resolve_subject, act, act!} =
          case action_type do
            :action ->
              subject = quote do: input

              resolve_subject =
                quote bind_quoted: [
                        resource: resource,
                        domain: domain,
                        action_name: action_name
                      ] do
                  {input, opts} =
                    Ash.CodeInterface.resolve_action_subject_for_action(
                      %{opts: opts},
                      params,
                      custom_input_errors,
                      resource,
                      domain,
                      action_name
                    )
                end

              act = quote do: Ash.run_action(input, opts)
              act! = quote do: Ash.run_action!(input, opts)

              {subject, [], resolve_subject, act, act!}

            :read ->
              subject = quote do: query

              resolve_subject =
                quote bind_quoted: [
                        resource: resource,
                        domain: domain,
                        action_name: action_name,
                        filter_keys: filter_keys
                      ] do
                  {query, opts} =
                    Ash.CodeInterface.resolve_action_subject_for_read(
                      %{opts: opts},
                      params,
                      custom_input_errors,
                      filter_params,
                      resource,
                      domain,
                      action_name,
                      filter_keys
                    )
                end

              resolve_not_found_error? =
                quote do
                  {not_found_error?, opts} = Keyword.pop(opts, :not_found_error?)

                  not_found_error? =
                    if not_found_error? != nil,
                      do: not_found_error?,
                      else: unquote(interface_not_found_error)
                end

              act =
                if interface_get do
                  quote do
                    unquote(resolve_not_found_error?)

                    Ash.read_one(query, Keyword.drop(opts, [:stream?, :stream_options]))
                    |> case do
                      {:ok, nil} when not_found_error? ->
                        {:error,
                         Ash.Error.to_error_class(
                           Ash.Error.Query.NotFound.exception(resource: query.resource)
                         )}

                      result ->
                        result
                    end
                  end
                else
                  quote do: Ash.read(query, Keyword.drop(opts, [:stream?, :stream_options]))
                end

              act! =
                if interface_get do
                  quote do
                    unquote(resolve_not_found_error?)

                    Ash.read_one!(query, Keyword.drop(opts, [:stream?, :stream_options]))
                    |> case do
                      nil when not_found_error? ->
                        raise Ash.Error.to_error_class(
                                Ash.Error.Query.NotFound.exception(resource: query.resource)
                              )

                      result ->
                        result
                    end
                  end
                else
                  quote do
                    if opts[:stream?] do
                      opts =
                        Keyword.merge(opts, opts[:stream_options] || [])
                        |> Keyword.drop([:stream?, :stream_options])

                      Ash.stream!(query, Keyword.drop(opts, [:stream?, :stream_options]))
                    else
                      Ash.read!(query, Keyword.drop(opts, [:stream?, :stream_options]))
                    end
                  end
                end

              {subject, [], resolve_subject, act, act!}

            :create ->
              subject = quote do: changeset

              resolve_subject =
                quote bind_quoted: [
                        resource: resource,
                        domain: domain,
                        action_name: action_name
                      ] do
                  {changeset, changeset_opts, opts} =
                    Ash.CodeInterface.resolve_action_subject_for_create(
                      %{opts: opts},
                      params,
                      custom_input_errors,
                      resource,
                      domain,
                      action_name
                    )
                end

              act =
                quote bind_quoted: [
                        resource: resource,
                        action_name: action_name
                      ] do
                  Ash.CodeInterface.execute_create_action(
                    changeset,
                    custom_input_errors,
                    changeset_opts,
                    opts,
                    resource,
                    action_name,
                    false
                  )
                end

              act! =
                quote bind_quoted: [
                        resource: resource,
                        action_name: action_name
                      ] do
                  Ash.CodeInterface.execute_create_action(
                    changeset,
                    custom_input_errors,
                    changeset_opts,
                    opts,
                    resource,
                    action_name,
                    true
                  )
                end

              {subject, [], resolve_subject, act, act!}

            :update ->
              subject = quote do: changeset

              subject_args =
                if interface_require_reference do
                  quote do: [record]
                else
                  []
                end

              resolve_subject =
                if interface_require_reference do
                  quote bind_quoted: [
                          resource: resource,
                          domain: domain,
                          action_name: action_name,
                          require_reference: interface_require_reference,
                          filter_keys: filter_keys
                        ] do
                    {changeset, changeset_opts, opts} =
                      Ash.CodeInterface.resolve_action_subject_for_update(
                        %{opts: opts, record: record},
                        params,
                        custom_input_errors,
                        filter_params,
                        resource,
                        domain,
                        action_name,
                        require_reference,
                        filter_keys
                      )
                  end
                else
                  quote bind_quoted: [
                          resource: resource,
                          domain: domain,
                          action_name: action_name,
                          require_reference: interface_require_reference,
                          filter_keys: filter_keys
                        ] do
                    {changeset, changeset_opts, opts} =
                      Ash.CodeInterface.resolve_action_subject_for_update(
                        %{opts: opts, record: nil},
                        params,
                        custom_input_errors,
                        filter_params,
                        resource,
                        domain,
                        action_name,
                        require_reference,
                        filter_keys
                      )
                  end
                end

              act =
                quote bind_quoted: [
                        resource: resource,
                        action_name: action_name,
                        interface_get: interface_get
                      ] do
                  Ash.CodeInterface.execute_update_action(
                    changeset,
                    custom_input_errors,
                    changeset_opts,
                    opts,
                    params,
                    filter_params,
                    resource,
                    action_name,
                    interface_get,
                    __MODULE__,
                    false
                  )
                end

              act! =
                quote bind_quoted: [
                        resource: resource,
                        action_name: action_name,
                        interface_get: interface_get
                      ] do
                  Ash.CodeInterface.execute_update_action(
                    changeset,
                    custom_input_errors,
                    changeset_opts,
                    opts,
                    params,
                    filter_params,
                    resource,
                    action_name,
                    interface_get,
                    __MODULE__,
                    true
                  )
                end

              {subject, subject_args, resolve_subject, act, act!}

            :destroy ->
              subject = quote do: changeset

              subject_args =
                if interface_require_reference do
                  quote do: [record]
                else
                  []
                end

              resolve_subject =
                if interface_require_reference do
                  quote bind_quoted: [
                          resource: resource,
                          domain: domain,
                          action_name: action_name,
                          require_reference: interface_require_reference
                        ] do
                    {changeset, changeset_opts, opts} =
                      Ash.CodeInterface.resolve_action_subject_for_destroy(
                        %{opts: opts, record: record},
                        params,
                        custom_input_errors,
                        filter_params,
                        resource,
                        domain,
                        action_name,
                        require_reference
                      )
                  end
                else
                  quote bind_quoted: [
                          resource: resource,
                          domain: domain,
                          action_name: action_name,
                          require_reference: interface_require_reference
                        ] do
                    {changeset, changeset_opts, opts} =
                      Ash.CodeInterface.resolve_action_subject_for_destroy(
                        %{opts: opts, record: nil},
                        params,
                        custom_input_errors,
                        filter_params,
                        resource,
                        domain,
                        action_name,
                        require_reference
                      )
                  end
                end

              act =
                quote bind_quoted: [
                        resource: resource,
                        action_name: action_name,
                        interface_get: interface_get
                      ] do
                  Ash.CodeInterface.execute_destroy_action(
                    changeset,
                    custom_input_errors,
                    changeset_opts,
                    opts,
                    params,
                    filter_params,
                    resource,
                    action_name,
                    interface_get,
                    __MODULE__,
                    false
                  )
                end

              act! =
                quote bind_quoted: [
                        resource: resource,
                        action_name: action_name,
                        interface_get: interface_get
                      ] do
                  Ash.CodeInterface.execute_destroy_action(
                    changeset,
                    custom_input_errors,
                    changeset_opts,
                    opts,
                    params,
                    filter_params,
                    resource,
                    action_name,
                    interface_get,
                    __MODULE__,
                    true
                  )
                end

              {subject, subject_args, resolve_subject, act, act!}
          end

        subject_name = elem(subject, 0)

        common_args =
          quote do: [
                  unquote_splicing(subject_args),
                  unquote_splicing(arg_vars_function)
                ]

        first_opts_location = Enum.count(subject_args) + Enum.count(arg_vars_function)

        params_handler =
          if action.type == :create do
            quote do: Ash.CodeInterface.handle_params_and_opts_for_create(params, opts)
          else
            quote do: Ash.CodeInterface.handle_params_and_opts(params, opts)
          end

        @dialyzer {:nowarn_function, {interface_name_atom, length(common_args) + 2}}
        @doc """
             #{action_description || "Calls the #{action_name} action on #{inspect(resource)}."}

             #{Ash.CodeInterface.describe_action(resource, action, interface_args, interface_exclude_inputs, interface.custom_inputs)}

             ## Options

             #{interface_options.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()

        @doc spark_opts: [
               {first_opts_location, interface_options.schema()},
               {first_opts_location + 1, interface_options.schema()}
             ]

        def unquote(interface_name_atom)(
              unquote_splicing(common_args),
              params \\ nil,
              opts \\ nil
            ) do
          {params_or_opts, opts} = unquote(params_handler)

          unquote(resolve_params_and_opts)
          unquote(resolve_subject)
          unquote(act)
        end

        # sobelow_skip ["DOS.BinToAtom"]
        @dialyzer {:nowarn_function, {:"#{interface_name_atom}!", length(common_args) + 2}}
        @doc """
             #{action_description || "Calls the #{action_name} action on #{inspect(resource)}."}

             Raises any errors instead of returning them

             #{Ash.CodeInterface.describe_action(resource, action, interface_args, interface_exclude_inputs, interface.custom_inputs)}

             ## Options

             #{interface_options.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()

        @doc spark_opts: [
               {first_opts_location, interface_options.schema()},
               {first_opts_location + 1, interface_options.schema()}
             ]
        def unquote(:"#{interface_name_atom}!")(
              unquote_splicing(common_args),
              params \\ nil,
              opts \\ nil
            ) do
          {params_or_opts, opts} = unquote(params_handler)
          unquote(resolve_params_and_opts)
          unquote(resolve_subject)
          unquote(act!)
        end

        # sobelow_skip ["DOS.BinToAtom"]
        if subject_name in [:changeset, :query, :input] do
          subject_opts =
            Keyword.take(interface_options.schema(), [
              :actor,
              :tenant,
              :scope,
              :authorize?,
              :tracer,
              :changeset,
              :query,
              :input
            ])

          @dialyzer {:nowarn_function,
                     {:"#{subject_name}_to_#{interface_name_atom}", length(common_args) + 2}}

          @doc spark_opts: [
                 {first_opts_location, interface_options.schema()},
                 {first_opts_location + 1, interface_options.schema()}
               ]
          @doc """
               Returns the #{subject_name} corresponding to the action.

               ## Options

               #{Spark.Options.docs(subject_opts)}
               """
               |> Ash.CodeInterface.trim_double_newlines()
          @doc spark_opts: [
                 {first_opts_location, subject_opts},
                 {first_opts_location + 1, subject_opts}
               ]
          def unquote(:"#{subject_name}_to_#{interface_name_atom}")(
                unquote_splicing(common_args),
                params_or_opts \\ %{},
                opts \\ []
              ) do
            unquote(resolve_params_and_opts)
            unquote(resolve_subject)
            unquote(subject)
          end
        end

        # sobelow_skip ["DOS.BinToAtom"]
        @doc """
             Runs authorization checks for `#{inspect(resource)}.#{action_name}`

             See `Ash.can/3` for more information

             ## Options

             #{Ash.Resource.Interface.CanOpts.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        @dialyzer {:nowarn_function, {:"can_#{interface_name_atom}", length(common_args) + 3}}
        @doc spark_opts: [
               {first_opts_location + 1, Ash.Resource.Interface.CanOpts.schema()},
               {first_opts_location + 2, Ash.Resource.Interface.CanOpts.schema()}
             ]
        def unquote(:"can_#{interface_name_atom}")(
              actor,
              unquote_splicing(common_args),
              params_or_opts \\ %{},
              opts \\ []
            ) do
          {params, opts} =
            Ash.CodeInterface.params_and_opts(params_or_opts, opts, fn opts ->
              opts
              |> Ash.Resource.Interface.CanOpts.validate!()
              |> unquote(interface_options).to_options()
              |> Keyword.put(:actor, actor)
            end)

          filter_params = unquote(filter_params)
          arg_params = unquote(arg_params)

          params =
            if is_list(params),
              do: Enum.map(params, &Map.merge(&1, arg_params)),
              else: Map.merge(params, arg_params)

          case Enum.filter(unquote(interface_exclude_inputs), fn input ->
                 Map.has_key?(params, input) || Map.has_key?(params, to_string(input))
               end) do
            [] ->
              :ok

            inputs ->
              raise ArgumentError,
                    "Input(s) `#{Enum.join(inputs, ", ")}` not accepted by #{inspect(unquote(resource))}.#{unquote(interface_name_atom)}/#{unquote(interface_args_count + 2)}"
          end

          {params, custom_input_errors} =
            Ash.CodeInterface.handle_custom_inputs(
              params,
              unquote(interface_custom_inputs),
              unquote(resource)
            )

          unquote(resolve_subject)

          case unquote(subject) do
            %struct{} when struct in [Ash.Changeset, Ash.Query, Ash.ActionInput] ->
              Ash.can(unquote(subject), actor, opts)

            {:atomic, _, %Ash.Query{} = query} = subj ->
              Ash.CodeInterface.atomic_can(
                query,
                unquote(action_name),
                actor,
                opts,
                params,
                false
              )

            {:atomic, _, input} ->
              raise "Ash.can_#{unquote(interface_name_atom)} does not support #{inspect(input)} as input."

            {:bulk, input} ->
              raise "Ash.can_#{unquote(interface_name_atom)} does not support #{inspect(input)} as input."

            other ->
              raise "Ash.can_#{unquote(interface.name)} does not support #{inspect(other)} as input."
          end
        end

        # sobelow_skip ["DOS.BinToAtom"]
        @dialyzer {:nowarn_function, {:"can_#{interface_name_atom}?", length(common_args) + 3}}
        @doc spark_opts: [
               {first_opts_location + 1, Ash.Resource.Interface.CanQuestionMarkOpts.schema()},
               {first_opts_location + 2, Ash.Resource.Interface.CanQuestionMarkOpts.schema()}
             ]
        @doc """
             Runs authorization checks for `#{inspect(resource)}.#{action_name}`, returning a boolean.

             See `Ash.can?/3` for more information

             ## Options

             #{Ash.Resource.Interface.CanQuestionMarkOpts.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        def unquote(:"can_#{interface_name_atom}?")(
              actor,
              unquote_splicing(common_args),
              params_or_opts \\ %{},
              opts \\ []
            ) do
          {params, opts} =
            Ash.CodeInterface.params_and_opts(params_or_opts, opts, fn opts ->
              opts
              |> Ash.Resource.Interface.CanOpts.validate!()
              |> unquote(interface_options).to_options()
              |> Keyword.put(:actor, actor)
            end)

          filter_params = unquote(filter_params)
          arg_params = unquote(arg_params)

          params =
            if is_list(params),
              do: Enum.map(params, &Map.merge(&1, arg_params)),
              else: Map.merge(params, arg_params)

          case Enum.filter(unquote(interface_exclude_inputs), fn input ->
                 Map.has_key?(params, input) || Map.has_key?(params, to_string(input))
               end) do
            [] ->
              :ok

            inputs ->
              raise ArgumentError,
                    "Input(s) `#{Enum.join(inputs, ", ")}` not accepted by #{inspect(unquote(resource))}.#{unquote(interface_name_atom)}/#{unquote(interface_args_count + 2)}"
          end

          {params, custom_input_errors} =
            Ash.CodeInterface.handle_custom_inputs(
              params,
              unquote(interface_custom_inputs),
              unquote(resource)
            )

          unquote(resolve_subject)

          case unquote(subject) do
            %struct{} when struct in [Ash.Changeset, Ash.Query, Ash.ActionInput] ->
              Ash.can?(unquote(subject), actor, opts)

            {:atomic, _, %Ash.Query{} = query} = subj ->
              Ash.CodeInterface.atomic_can(
                query,
                unquote(action_name),
                actor,
                opts,
                params,
                true
              )

            {:atomic, _, input} = subj ->
              raise "Ash.can_#{unquote(interface_name_atom)}? does not support #{inspect(input)} as input."

            {:bulk, input} ->
              raise "Ash.can_#{unquote(interface_name_atom)}? does not support #{inspect(input)} as input."

            other ->
              raise "Ash.can_#{unquote(interface.name)}? does not support #{inspect(other)} as input."
          end
        end
      end
    end
  end

  @doc false
  def atomic_can(query, action_name, actor, opts, params, question_mark?) do
    action_opts =
      opts
      |> Keyword.take([
        :tenant,
        :authorize?,
        :tracer,
        :context,
        :skip_unknown_inputs
      ])
      |> Keyword.put(:actor, actor)

    query =
      if query && !query.__validated_for_action__ do
        Ash.Query.for_read(
          query,
          Ash.Resource.Info.primary_action!(query.resource, :read).name,
          %{},
          action_opts
        )
      else
        query
      end

    changeset =
      case Ash.Changeset.fully_atomic_changeset(
             query.resource,
             action_name,
             params,
             action_opts
           ) do
        {:not_atomic, _} ->
          if !opts[:data] or Enum.count_until(List.wrap(opts[:data]), 2) == 2 do
            raise ArgumentError, """
            The action #{action_name} could not be done atomically with the provided inputs.
            You must pass the `data` option, containing a single record you are checking for authorization.
            """
          else
            Ash.Changeset.for_action(
              Enum.at(List.wrap(opts[:data]), 0),
              action_name,
              params,
              action_opts
            )
          end

        changeset ->
          changeset
      end

    case Ash.can(
           query,
           actor,
           Keyword.merge(opts,
             return_forbidden_error?: true,
             maybe_is: false,
             atomic_changeset: changeset,
             filter_with: :filter,
             alter_source?: true,
             no_check?: true
           )
         ) do
      {:ok, true} ->
        if question_mark? do
          Ash.can?(
            changeset,
            actor,
            opts
          )
        else
          Ash.can(
            changeset,
            actor,
            opts
          )
        end

      {:ok, true, _query} ->
        if question_mark? do
          Ash.can?(
            changeset,
            actor,
            opts
          )
        else
          Ash.can(
            changeset,
            actor,
            opts
          )
        end

      {:ok, false, error} ->
        if question_mark? do
          false
        else
          {:error, error}
        end

      {:error, error} ->
        if question_mark? do
          false
        else
          {:error, error}
        end
    end
  end

  @doc false
  def describe_action(resource, action, args, exclude_inputs, custom_inputs) do
    resource
    |> Ash.Resource.Info.action_inputs(action.name)
    |> Enum.filter(&is_atom/1)
    |> Enum.reject(&(&1 in exclude_inputs))
    |> Enum.uniq()
    |> case do
      [] ->
        ""

      inputs ->
        {arguments, inputs} = Enum.split_with(inputs, &(&1 in (args || [])))

        arguments = Enum.sort_by(arguments, fn arg -> Enum.find_index(args, &(&1 == arg)) end)

        arguments =
          Enum.map(arguments, &describe_input(resource, action, &1, custom_inputs))

        inputs =
          Enum.map(inputs, &describe_input(resource, action, &1, custom_inputs))

        case {arguments, inputs} do
          {[], []} ->
            ""

          {arguments, []} ->
            """
            # Arguments

            #{Enum.join(arguments, "\n")}
            """

          {[], inputs} ->
            """
            # Inputs

            #{Enum.join(inputs, "\n")}
            """

          {arguments, inputs} ->
            """
            # Arguments

            #{Enum.join(arguments, "\n")}

            # Inputs

            #{Enum.join(inputs, "\n")}
            """
        end
    end
  end

  @doc false
  def describe_calculation(resource, calculation, args, exclude_inputs, custom_inputs) do
    calculation.arguments
    |> Enum.map(& &1.name)
    |> Enum.reject(&(&1 in exclude_inputs))
    |> case do
      [] ->
        ""

      inputs ->
        {arguments, inputs} = Enum.split_with(inputs, &(&1 in args))

        arguments = Enum.sort_by(arguments, fn arg -> Enum.find_index(args, &(&1 == arg)) end)

        arguments =
          Enum.map(arguments, &describe_input(resource, calculation, &1, custom_inputs))

        inputs =
          Enum.map(inputs, &describe_input(resource, calculation, &1, custom_inputs))

        case {arguments, inputs} do
          {[], []} ->
            ""

          {arguments, []} ->
            """
            # Arguments

            #{Enum.join(arguments, "\n")}
            """

          {[], inputs} ->
            """
            # Inputs

            #{Enum.join(inputs, "\n")}
            """

          {arguments, inputs} ->
            """
            # Arguments

            #{Enum.join(arguments, "\n")}

            # Inputs

            #{Enum.join(inputs, "\n")}
            """
        end
    end
  end

  defp describe_input(resource, %{arguments: arguments}, name, custom_inputs) do
    case Enum.find(custom_inputs, &(&1.name == name)) do
      nil ->
        case Enum.find(arguments, &(&1.name == name)) do
          nil ->
            case Ash.Resource.Info.field(resource, name) do
              nil ->
                "* #{name}"

              field ->
                describe(field)
            end

          argument ->
            describe(argument)
        end

      custom_input ->
        describe(custom_input)
    end
  end

  defp describe(%{name: name, description: description}) when not is_nil(description) do
    "* #{name} - #{description}"
  end

  defp describe(%{name: name}) do
    "* #{name}"
  end

  @doc false
  def handle_custom_inputs(params, [], _resource) do
    {params, []}
  end

  def handle_custom_inputs(params, custom_inputs, resource) do
    Enum.reduce(custom_inputs, {params, []}, fn custom_input, {params, errors} ->
      case fetch_key(params, custom_input.name) do
        {:ok, key, value} ->
          value = Ash.Type.Helpers.handle_indexed_maps(custom_input.type, value)

          with {:ok, casted} <-
                 Ash.Type.cast_input(custom_input.type, value, custom_input.constraints),
               {:ok, casted} <-
                 Ash.Type.apply_constraints(custom_input.type, casted, custom_input.constraints) do
            if is_nil(casted) && !custom_input.allow_nil? do
              error =
                Ash.Error.Changes.Required.exception(
                  resource: resource,
                  field: custom_input.name,
                  type: :custom_input
                )

              {params, [error | errors]}
            else
              params = apply_custom_input_transform(params, casted, key, custom_input)
              {params, errors}
            end
          else
            :error ->
              error =
                Ash.Error.Invalid.InvalidCustomInput.exception(
                  field: custom_input.name,
                  message: "is invalid",
                  value: value
                )

              {params, [error | errors]}

            {:error, error} when is_binary(error) ->
              error =
                Ash.Error.Invalid.InvalidCustomInput.exception(
                  field: custom_input.name,
                  message: error,
                  value: value
                )

              {params, [error | errors]}

            {:error, keyword} when is_list(keyword) ->
              if Keyword.keyword?(keyword) do
                error =
                  if keyword[:field] do
                    Ash.Error.Invalid.InvalidCustomInput.exception(
                      field: keyword[:field],
                      message: keyword[:message],
                      value: keyword[:value],
                      vars: keyword
                    )
                  else
                    Ash.Error.Invalid.InvalidCustomInput.exception(
                      fields: keyword[:fields] || [],
                      message: keyword[:message],
                      value: keyword[:value],
                      vars: keyword
                    )
                  end

                if keyword[:path] do
                  Ash.Error.set_path(error, keyword[:path])
                else
                  error
                end
              else
                error = Ash.Error.to_ash_error(keyword)

                {params, [error | errors]}
              end

            {:error, error} ->
              error = Ash.Error.to_ash_error(error)

              {params, [error | errors]}
          end

        :error ->
          if custom_input.allow_nil? do
            {params, errors}
          else
            error =
              Ash.Error.Changes.Required.exception(
                resource: resource,
                field: custom_input.name,
                type: :custom_input
              )

            {params, [error | errors]}
          end
      end
    end)
  end

  defp apply_custom_input_transform(params, casted, key, %{transform: nil}) do
    Map.put(params, key, casted)
  end

  defp apply_custom_input_transform(params, casted, key, %{
         transform: %{to: nil, using: nil}
       }) do
    Map.put(params, key, casted)
  end

  defp apply_custom_input_transform(params, casted, key, %{
         transform: %{to: to, using: nil}
       }) do
    params |> Map.delete(key) |> Map.put(to, casted)
  end

  defp apply_custom_input_transform(params, casted, key, %{
         transform: %{to: nil, using: using}
       }) do
    Map.put(params, key, using.(casted))
  end

  defp apply_custom_input_transform(params, casted, key, %{
         transform: %{to: to, using: using}
       }) do
    params |> Map.delete(key) |> Map.put(to, using.(casted))
  end

  defp fetch_key(map, key) do
    with {_key, :error} <- {key, Map.fetch(map, key)},
         string_key = to_string(key),
         {_key, :error} <- {string_key, Map.fetch(map, string_key)} do
      :error
    else
      {key, {:ok, value}} ->
        {:ok, key, value}
    end
  end

  @doc false
  def trim_double_newlines(str) do
    str
    |> String.replace(~r/\n{2,}/, "\n")
    |> String.trim_trailing()
  end

  @doc false
  def bulk_query(resource, method, id) do
    case method do
      :query ->
        {:ok, id}

      :stream ->
        {:ok, id}

      :id ->
        case Ash.Filter.get_filter(resource, id) do
          {:ok, filter} ->
            {:ok, resource |> Ash.Query.do_filter(filter) |> Ash.Query.limit(1)}

          {:error, error} ->
            {:error, error}
        end
    end
  end
end
