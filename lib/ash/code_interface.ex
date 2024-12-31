defmodule Ash.CodeInterface do
  @moduledoc """
  Used to define the functions of a code interface for a resource.
  """

  @doc false
  def require_action(resource, interface) do
    action = Ash.Resource.Info.action(resource, interface.action || interface.name)

    if !action do
      raise Spark.Error.DslError,
        module: resource,
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

  def without_optional(keys) do
    Enum.map(keys, fn
      {:optional, key} ->
        key

      key ->
        key
    end)
  end

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

  @doc """
  A common pattern is for a function to have both optional parameters and
  optional options. This usually comes in the form of two defaults:

    * An empty map for params.
    * An empty list for options.

  With those two defaults in mind, this function will decipher, from two inputs,
  which should be parameters and which should be options.

  Parameters can take one of two primary forms:

    1. A map.
    2. A list of maps for bulk operations.

  Additionally, if options are set explicitly (i.e. at least one option has
  been set), a keyword list will be converted to a map.

  ## Examples

      iex> params_and_opts(%{key: :param}, [key: :opt])
      {%{key: :param}, [key: :opt]}

      iex> params_and_opts([key: :opt], [])
      {%{}, [key: :opt]}

      iex> params_and_opts([], [])
      {[], []}

      iex> params_and_opts([%{key: :param}], [])
      {[%{key: :param}], []}

      iex> params_and_opts([key: :param], [key: :opt])
      {%{key: :param}, [key: :opt]}
  """
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

  Merges default options if provided (see `merge_default_opts/2`) and  dds a
  post process function that can takes the opts and can further process,
  validate, or transform them.
  """
  @spec params_and_opts(
          params_or_opts :: map() | [map()] | keyword(),
          keyword(),
          keyword(),
          (keyword() -> keyword())
        ) ::
          {params :: map() | [map()], opts :: keyword()}
  def params_and_opts(params_or_opts, maybe_opts, default_opts \\ [], post_process_opts_fn)
      when is_function(post_process_opts_fn, 1) do
    params_or_opts
    |> params_and_opts(maybe_opts)
    |> then(fn {params, opts} ->
      {params,
       opts
       |> merge_default_opts(default_opts)
       |> post_process_opts_fn.()}
    end)
  end

  @deep_merge_keys [:bulk_options, :page]
  @doc """
  Selectively merges default opts into client-provided opts. For most keys,
  the value in opts will be used instead of the default if provided. However,
  certain options have special behavior:

    * #{@deep_merge_keys |> Enum.map(&"`:#{&1}`") |> Enum.join(", ")} - These
      options are deep merged, so if the default is a keyword list and the
      client value is a keyword list, they'll be merged.
    * `:load` - The default value and the client value will be combined in this
      case.

  ## Examples

      iex> merge_default_opts([key1: 1], key2: 2)
      [key2: 2, key1: 1]

      iex> merge_default_opts([key2: :client], key1: :default, key2: :default)
      [key2: :client, key1: :default]

      iex> merge_default_opts([page: false], page: [limit: 100])
      [page: false]

      iex> merge_default_opts([page: [offset: 2]], page: [limit: 100])
      [page: [limit: 100, offset: 2]]

      iex> merge_default_opts([load: [:calc2, :rel4]], load: [:calc1, rel1: [:rel2, :rel3]])
      [load: [:calc1, {:rel1, [:rel2, :rel3]}, :calc2, :rel4]]
  """
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

      for {{name, arity, optional_arity}, interfaces} <- interfaces_for_defaults do
        args =
          case interfaces do
            [%{args: args, calculation: calculation}] ->
              calculation = Ash.Resource.Info.calculation(resource, calculation)

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
                interface.args
                |> Enum.flat_map(fn
                  {:optional, value} ->
                    calculation = Ash.Resource.Info.calculation(resource, interface.calculation)
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
                  calculation = Ash.Resource.Info.calculation(resource, interface.calculation)

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
        calculation = Ash.Resource.Info.calculation(resource, interface.calculation)

        {arg_bindings, arg_access} =
          interface.args
          |> Kernel.||([])
          |> Ash.CodeInterface.unwrap_calc_interface_args(resource, calculation.arguments)

        {safe_name, bang_name} = Ash.CodeInterface.resolve_calc_method_names(interface.name)

        opts_location = Enum.count(arg_bindings)
        interface_options = Ash.Resource.Interface.interface_options(:calculate, nil)

        @doc """
             #{calculation.description || "Calculates #{calculation.name} action on #{inspect(resource)}."}

             #{Ash.CodeInterface.describe_calculation(resource, calculation, interface.args)}

             ### Options

             #{interface_options.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        @doc spark_opts: [
               {opts_location, interface_options.schema()}
             ]
        def unquote(bang_name)(unquote_splicing(arg_bindings), opts) do
          {refs, arguments, record} =
            Enum.reduce(
              [unquote_splicing(arg_access)],
              {%{}, %{}, nil},
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

          opts = [domain: unquote(domain), refs: refs, args: arguments, record: record] ++ opts
          Ash.calculate!(unquote(resource), unquote(interface.calculation), opts)
        end

        @doc """
             #{calculation.description || "Calculates #{calculation.name} action on #{inspect(resource)}."}

             #{Ash.CodeInterface.describe_calculation(resource, calculation, interface.args)}

             ### Options

             #{interface_options.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        @doc spark_opts: [
               {opts_location, interface_options.schema()}
             ]
        def unquote(safe_name)(unquote_splicing(arg_bindings), opts) do
          {refs, arguments, record} =
            Enum.reduce(
              [unquote_splicing(arg_access)],
              {%{}, %{}, nil},
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

          opts = [domain: unquote(domain), refs: refs, args: arguments, record: record] ++ opts
          Ash.calculate(unquote(resource), unquote(interface.calculation), opts)
        end
      end

      for interface <- interfaces do
        action = Ash.CodeInterface.require_action(resource, interface)

        filter_keys =
          cond do
            action.type not in [:read, :update, :destroy] ->
              []

            interface.get_by_identity ->
              Ash.Resource.Info.identity(resource, interface.get_by_identity).keys

            interface.get_by ->
              interface.get_by

            true ->
              []
          end

        args = List.wrap(filter_keys) ++ Ash.CodeInterface.without_optional(interface.args || [])

        arg_vars = Enum.map(args, &{&1, [], Elixir})

        arg_vars_function =
          filter_keys
          |> List.wrap()
          |> Enum.concat(interface.args || [])
          |> Enum.map(fn
            {:optional, key} ->
              default = Ash.CodeInterface.default_value(resource, action, key)
              {:\\, [], [{key, [], Elixir}, default]}

            key ->
              {key, [], Elixir}
          end)

        if Enum.uniq(args) != args do
          raise """
          Arguments #{inspect(args)} for #{interface.name} are not unique!
          """
        end

        interface =
          if Map.get(action, :get?) do
            %{interface | get?: true}
          else
            interface
          end

        interface_options = Ash.Resource.Interface.interface_options(action.type, interface)

        resolve_params_and_opts =
          quote do
            {params, opts} =
              Ash.CodeInterface.params_and_opts(
                params_or_opts,
                opts,
                unquote(Macro.escape(interface.default_options)),
                fn opts ->
                  opts
                  |> unquote(interface_options).validate!()
                  |> unquote(interface_options).to_options()
                end
              )

            arg_params =
              unquote(args)
              |> Enum.zip([unquote_splicing(arg_vars)])
              |> Map.new()

            params =
              if is_list(params),
                do: Enum.map(params, &Map.merge(&1, arg_params)),
                else: Map.merge(params, arg_params)
          end

        {subject, subject_args, resolve_subject, act, act!} =
          case action.type do
            :action ->
              subject = quote do: input

              resolve_subject =
                quote do
                  {input_opts, opts} =
                    Keyword.split(opts, [:input, :actor, :tenant, :authorize?, :tracer])

                  {input, input_opts} = Keyword.pop(input_opts, :input)

                  input_opts = Keyword.put(input_opts, :domain, unquote(domain))

                  case input do
                    %Ash.ActionInput{resource: unquote(resource)} ->
                      input

                    %Ash.ActionInput{resource: other_resource} ->
                      raise ArgumentError,
                            "Action input resource #{inspect(other_resource)} does not match expected resource #{inspect(unquote(resource))}."

                    input ->
                      input
                  end

                  input =
                    input
                    |> Kernel.||(unquote(resource))
                    |> Ash.ActionInput.for_action(unquote(action.name), params, input_opts)
                end

              act = quote do: Ash.run_action(input, opts)
              act! = quote do: Ash.run_action!(input, opts)

              {subject, [], resolve_subject, act, act!}

            :read ->
              subject = quote do: query

              resolve_subject =
                quote do
                  {query_opts, opts} =
                    Keyword.split(opts, [:query, :actor, :tenant, :authorize?, :tracer, :context])

                  {query, query_opts} = Keyword.pop(query_opts, :query)

                  query_opts = Keyword.put(query_opts, :domain, unquote(domain))

                  query =
                    case query do
                      %Ash.Query{resource: unquote(resource)} = query ->
                        query

                      %Ash.Query{resource: other_resource} ->
                        raise ArgumentError,
                              "Query resource #{inspect(other_resource)} does not match expected resource #{inspect(unquote(resource))}."

                      unquote(resource) ->
                        unquote(resource)
                        |> Ash.Query.new()

                      other_resource
                      when is_atom(other_resource) and not is_nil(other_resource) ->
                        raise ArgumentError,
                              "Query resource #{inspect(other_resource)} does not match expected resource #{inspect(unquote(resource))}."

                      query ->
                        Ash.Query.build(unquote(resource), query || [])
                    end

                  query =
                    if unquote(filter_keys) && !Enum.empty?(unquote(filter_keys)) do
                      require Ash.Query
                      {filters, params} = Map.split(params, unquote(filter_keys))

                      query
                      |> Ash.Query.for_read(unquote(action.name), params, query_opts)
                      |> Ash.Query.filter(filters)
                    else
                      Ash.Query.for_read(query, unquote(action.name), params, query_opts)
                    end
                end

              resolve_not_found_error? =
                quote do
                  {not_found_error?, opts} = Keyword.pop(opts, :not_found_error?)

                  not_found_error? =
                    if not_found_error? != nil,
                      do: not_found_error?,
                      else: unquote(interface.not_found_error?)
                end

              act =
                if interface.get? do
                  quote do
                    unquote(resolve_not_found_error?)

                    Ash.read_one(query, Keyword.drop(opts, [:stream?, :stream_options]))
                    |> case do
                      {:ok, nil} when not_found_error? ->
                        {:error, Ash.Error.Query.NotFound.exception(resource: query.resource)}

                      result ->
                        result
                    end
                  end
                else
                  quote do: Ash.read(query, Keyword.drop(opts, [:stream?, :stream_options]))
                end

              act! =
                if interface.get? do
                  quote do
                    unquote(resolve_not_found_error?)

                    Ash.read_one!(query, Keyword.drop(opts, [:stream?, :stream_options]))
                    |> case do
                      nil when not_found_error? ->
                        raise Ash.Error.Query.NotFound, resource: query.resource

                      result ->
                        result
                    end
                  end
                else
                  quote do
                    if opts[:stream?] do
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
                quote do
                  {changeset, opts} = Keyword.pop(opts, :changeset)

                  {changeset_opts, opts} =
                    Keyword.split(opts, [
                      :actor,
                      :tenant,
                      :authorize?,
                      :tracer,
                      :context,
                      :skip_unknown_inputs
                    ])

                  changeset_opts = Keyword.put(changeset_opts, :domain, unquote(domain))

                  changeset =
                    if is_map(params) do
                      changeset
                      |> Kernel.||(unquote(resource))
                      |> case do
                        %Ash.Changeset{resource: unquote(resource)} ->
                          changeset

                        %Ash.Changeset{resource: other_resource} ->
                          raise ArgumentError,
                                "Changeset #{inspect(changeset)} does not match expected resource #{inspect(unquote(resource))}."

                        other_resource
                        when is_atom(other_resource) and other_resource != unquote(resource) ->
                          raise ArgumentError,
                                "Resource #{inspect(other_resource)} does not match expected resource #{inspect(unquote(resource))}."

                        changeset ->
                          changeset
                      end
                      |> Ash.Changeset.for_create(unquote(action.name), params, changeset_opts)
                    else
                      {:bulk, params}
                    end
                end

              act =
                quote do
                  case changeset do
                    {:bulk, inputs} ->
                      bulk_opts =
                        opts
                        |> Keyword.delete(:bulk_options)
                        |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
                        |> Enum.concat(changeset_opts)

                      Ash.bulk_create(
                        inputs,
                        unquote(resource),
                        unquote(action.name),
                        bulk_opts
                      )

                    changeset ->
                      Ash.create(changeset, Keyword.delete(opts, :bulk_options))
                  end
                end

              act! =
                quote do
                  case changeset do
                    {:bulk, inputs} ->
                      bulk_opts =
                        opts
                        |> Keyword.delete(:bulk_options)
                        |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
                        |> Enum.concat(changeset_opts)

                      Ash.bulk_create!(
                        inputs,
                        unquote(resource),
                        unquote(action.name),
                        bulk_opts
                      )

                    changeset ->
                      Ash.create!(changeset, Keyword.delete(opts, :bulk_options))
                  end
                end

              {subject, [], resolve_subject, act, act!}

            :update ->
              subject = quote do: changeset

              subject_args =
                if interface.require_reference? do
                  quote do: [record]
                else
                  []
                end

              resolve_subject =
                if Enum.empty?(filter_keys) and interface.require_reference? do
                  quote do
                    {changeset_opts, opts} =
                      Keyword.split(opts, [
                        :actor,
                        :tenant,
                        :authorize?,
                        :tracer,
                        :context,
                        :skip_unknown_inputs
                      ])

                    changeset_opts = Keyword.put(changeset_opts, :domain, unquote(domain))

                    changeset =
                      record
                      |> case do
                        %Ash.Changeset{resource: unquote(resource)} ->
                          {filters, params} = Map.split(params, unquote(filter_keys))

                          record
                          |> Ash.Changeset.filter(filters)
                          |> Ash.Changeset.for_update(
                            unquote(action.name),
                            params,
                            changeset_opts
                          )

                        %Ash.Changeset{resource: other_resource} ->
                          raise ArgumentError,
                                "Changeset #{inspect(record)} does not match expected resource #{inspect(unquote(resource))}."

                        %struct{} = record when struct == unquote(resource) ->
                          {filters, params} = Map.split(params, unquote(filter_keys))

                          record
                          |> Ash.Changeset.new()
                          |> Ash.Changeset.filter(filters)
                          |> Ash.Changeset.for_update(
                            unquote(action.name),
                            params,
                            changeset_opts
                          )

                        %Ash.Query{} = query ->
                          {:atomic, :query, query}

                        %other_resource{} when other_resource != unquote(resource) ->
                          raise ArgumentError,
                                "Record #{inspect(record)} does not match expected resource #{inspect(unquote(resource))}."

                        [{_key, _val} | _] = id ->
                          {:atomic, :id, id}

                        list when is_list(list) ->
                          {:atomic, :stream, list}

                        other ->
                          {:atomic, :id, other}
                      end
                  end
                else
                  quote do
                    filters = Map.take(params, unquote(filter_keys))

                    {changeset_opts, opts} =
                      Keyword.split(opts, [
                        :actor,
                        :tenant,
                        :authorize?,
                        :tracer,
                        :context,
                        :skip_unknown_inputs
                      ])

                    changeset_opts = Keyword.put(changeset_opts, :domain, unquote(domain))

                    changeset =
                      {:atomic, :query, Ash.Query.do_filter(unquote(resource), filters)}
                  end
                end

              act =
                quote do
                  {filters, params} = Map.split(params, unquote(filter_keys))

                  case changeset do
                    {:atomic, method, id} ->
                      bulk_opts =
                        opts
                        |> Keyword.drop([:bulk_options, :atomic_upgrade?])
                        |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
                        |> Enum.concat(changeset_opts)
                        |> Keyword.put(:resource, unquote(resource))
                        |> then(fn bulk_opts ->
                          if method == :id || unquote(interface.get?) do
                            bulk_opts
                            |> Keyword.put(:return_records?, true)
                            |> Keyword.put(:return_errors?, true)
                            |> Keyword.put_new(:authorize_with, :error)
                            |> Keyword.put(:notify?, true)
                          else
                            bulk_opts
                          end
                        end)
                        |> Keyword.put_new(:strategy, [:atomic, :stream, :atomic_batches])

                      bulk_opts =
                        if method in [:stream, :query] do
                          Keyword.put(bulk_opts, :filter, filters)
                        else
                          bulk_opts
                        end

                      case Ash.CodeInterface.bulk_query(unquote(resource), method, id) do
                        {:ok, query} ->
                          query
                          |> Ash.bulk_update(unquote(action.name), params, bulk_opts)
                          |> case do
                            %Ash.BulkResult{} = result
                            when method in [:stream, :query] and not unquote(interface.get?) ->
                              result

                            %Ash.BulkResult{status: :success, records: [record]} = result ->
                              if opts[:return_notifications?] do
                                {:ok, record, result.notifications}
                              else
                                {:ok, record}
                              end

                            %Ash.BulkResult{status: :success, records: []} = result ->
                              {:error,
                               Ash.Error.to_error_class(
                                 Ash.Error.Query.NotFound.exception(
                                   resource: unquote(resource),
                                   primary_key: id
                                 )
                               )}

                            %Ash.BulkResult{status: :error, errors: errors} ->
                              {:error, Ash.Error.to_error_class(errors)}
                          end

                        {:error, error} ->
                          {:error, Ash.Error.to_error_class(error)}
                      end

                    changeset ->
                      Ash.update(changeset, Keyword.delete(opts, :bulk_options))
                  end
                end

              act! =
                quote do
                  case changeset do
                    {:atomic, method, id} ->
                      {filters, params} = Map.split(params, unquote(filter_keys))

                      bulk_opts =
                        opts
                        |> Keyword.drop([:bulk_options, :atomic_upgrade?])
                        |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
                        |> Enum.concat(changeset_opts)
                        |> Keyword.put(:resource, unquote(resource))
                        |> then(fn bulk_opts ->
                          if method == :id || unquote(interface.get?) do
                            bulk_opts
                            |> Keyword.put(:return_records?, true)
                            |> Keyword.put(:return_errors?, true)
                            |> Keyword.put_new(:authorize_with, :error)
                            |> Keyword.put(:notify?, true)
                          else
                            bulk_opts
                          end
                        end)
                        |> Keyword.put_new(:strategy, [:atomic, :stream, :atomic_batches])

                      bulk_opts =
                        if method in [:stream] do
                          Keyword.put(bulk_opts, :filter, filters)
                        else
                          bulk_opts
                        end

                      case Ash.CodeInterface.bulk_query(unquote(resource), method, id) do
                        {:ok, query} ->
                          query
                          |> Ash.bulk_update!(unquote(action.name), params, bulk_opts)
                          |> case do
                            %Ash.BulkResult{} = result
                            when method in [:stream, :query] and not unquote(interface.get?) ->
                              result

                            %Ash.BulkResult{status: :success, records: [record]} = result ->
                              if opts[:return_notifications?] do
                                {record, result.notifications}
                              else
                                record
                              end

                            %Ash.BulkResult{status: :success, records: []} = result ->
                              raise Ash.Error.to_error_class(
                                      Ash.Error.Query.NotFound.exception(
                                        resource: unquote(resource),
                                        primary_key: id
                                      )
                                    )
                          end

                        {:error, error} ->
                          raise Ash.Error.to_error_class(error)
                      end

                    changeset ->
                      Ash.update!(changeset, Keyword.delete(opts, :bulk_options))
                  end
                end

              {subject, subject_args, resolve_subject, act, act!}

            :destroy ->
              subject = quote do: changeset

              subject_args =
                if interface.require_reference? do
                  quote do: [record]
                else
                  []
                end

              resolve_subject =
                if interface.require_reference? do
                  quote do
                    {changeset_opts, opts} =
                      Keyword.split(opts, [
                        :actor,
                        :tenant,
                        :authorize?,
                        :tracer,
                        :context,
                        :skip_unknown_inputs
                      ])

                    changeset_opts = Keyword.put(changeset_opts, :domain, unquote(domain))

                    changeset =
                      record
                      |> case do
                        %Ash.Changeset{resource: unquote(resource)} ->
                          {filters, params} = Map.split(params, unquote(filter_keys))

                          record
                          |> Ash.Changeset.filter(filters)
                          |> Ash.Changeset.for_destroy(
                            unquote(action.name),
                            params,
                            changeset_opts
                          )

                        %Ash.Changeset{resource: other_resource} ->
                          raise ArgumentError,
                                "Changeset #{inspect(record)} does not match expected resource #{inspect(unquote(resource))}."

                        %struct{} = record when struct == unquote(resource) ->
                          {filters, params} = Map.split(params, unquote(filter_keys))

                          record
                          |> Ash.Changeset.new()
                          |> Ash.Changeset.filter(filters)
                          |> Ash.Changeset.for_destroy(
                            unquote(action.name),
                            params,
                            changeset_opts
                          )

                        %Ash.Query{} = query ->
                          {:atomic, :query, query}

                        %other_resource{} when other_resource != unquote(resource) ->
                          raise ArgumentError,
                                "Record #{inspect(record)} does not match expected resource #{inspect(unquote(resource))}."

                        [{_key, _val} | _] = id ->
                          {:atomic, :id, id}

                        list when is_list(list) ->
                          {:atomic, :stream, list}

                        other ->
                          {:atomic, :id, other}
                      end
                  end
                else
                  quote do
                    filters = Map.take(params, unquote(filter_keys))

                    {changeset_opts, opts} =
                      Keyword.split(opts, [
                        :actor,
                        :tenant,
                        :authorize?,
                        :tracer,
                        :context,
                        :skip_unknown_inputs
                      ])

                    changeset_opts = Keyword.put(changeset_opts, :domain, unquote(domain))

                    changeset =
                      {:atomic, :query, Ash.Query.do_filter(unquote(resource), filters)}
                  end
                end

              act =
                quote do
                  case changeset do
                    {:atomic, method, id} ->
                      {filters, params} = Map.split(params, unquote(filter_keys))

                      bulk_opts =
                        opts
                        |> Keyword.drop([:bulk_options, :return_destroyed?])
                        |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
                        |> Enum.concat(changeset_opts)
                        |> Keyword.put(:resource, unquote(resource))
                        |> then(fn bulk_opts ->
                          if method == :id || unquote(interface.get?) do
                            bulk_opts
                            |> Keyword.put(:return_records?, opts[:return_destroyed?])
                            |> Keyword.put(:return_errors?, true)
                            |> Keyword.put_new(:authorize_with, :error)
                            |> Keyword.put(:notify?, true)
                          else
                            Keyword.put(bulk_opts, :return_records?, opts[:return_destroyed?])
                          end
                        end)
                        |> Keyword.put_new(:strategy, [:atomic, :stream, :atomic_batches])

                      bulk_opts =
                        if method in [:stream, :query] do
                          Keyword.put(bulk_opts, :filter, filters)
                        else
                          bulk_opts
                        end

                      case Ash.CodeInterface.bulk_query(unquote(resource), method, id) do
                        {:ok, query} ->
                          query
                          |> Ash.bulk_destroy(unquote(action.name), params, bulk_opts)
                          |> case do
                            %Ash.BulkResult{} = result
                            when method in [:stream, :query] and not unquote(interface.get?) ->
                              result

                            %Ash.BulkResult{status: :success, records: [record]} = result ->
                              if opts[:return_destroyed?] do
                                if opts[:return_notifications?] do
                                  {:ok, record, result.notifications}
                                else
                                  {:ok, record}
                                end
                              else
                                if opts[:return_notifications?] do
                                  {:ok, result.notifications}
                                else
                                  :ok
                                end
                              end

                            %Ash.BulkResult{status: :success, records: empty} = result
                            when empty in [[], nil] ->
                              if opts[:return_destroyed?] do
                                {:error,
                                 Ash.Error.to_error_class(
                                   Ash.Error.Query.NotFound.exception(
                                     resource: unquote(resource),
                                     primary_key: id
                                   )
                                 )}
                              else
                                if opts[:return_notifications?] do
                                  {:ok, result.notifications}
                                else
                                  :ok
                                end
                              end

                            %Ash.BulkResult{status: :error, errors: errors} ->
                              {:error, Ash.Error.to_error_class(errors)}
                          end

                        {:error, error} ->
                          {:error, Ash.Error.to_error_class(error)}
                      end

                    changeset ->
                      Ash.destroy(changeset, Keyword.delete(opts, :bulk_options))
                  end
                end

              act! =
                quote do
                  case changeset do
                    {:atomic, method, id} ->
                      {filters, params} = Map.split(params, unquote(filter_keys))

                      bulk_opts =
                        opts
                        |> Keyword.drop([:bulk_options, :return_destroyed?])
                        |> Keyword.merge(Keyword.get(opts, :bulk_options, []))
                        |> Enum.concat(changeset_opts)
                        |> Keyword.put(:resource, unquote(resource))
                        |> then(fn bulk_opts ->
                          if method == :id || unquote(interface.get?) do
                            bulk_opts
                            |> Keyword.put(:return_records?, opts[:return_destroyed?])
                            |> Keyword.put(:return_errors?, true)
                            |> Keyword.put_new(:authorize_with, :error)
                            |> Keyword.put(:notify?, true)
                          else
                            Keyword.put(bulk_opts, :return_records?, opts[:return_destroyed?])
                          end
                        end)
                        |> Keyword.put_new(:strategy, [:atomic, :stream, :atomic_batches])

                      bulk_opts =
                        if method in [:stream, :query] do
                          Keyword.put(bulk_opts, :filter, filters)
                        else
                          bulk_opts
                        end

                      case Ash.CodeInterface.bulk_query(unquote(resource), method, id) do
                        {:ok, query} ->
                          query
                          |> Ash.bulk_destroy!(unquote(action.name), params, bulk_opts)
                          |> case do
                            %Ash.BulkResult{} = result
                            when method in [:stream, :query] and not unquote(interface.get?) ->
                              result

                            %Ash.BulkResult{status: :success, records: [record]} = result ->
                              if opts[:return_destroyed?] do
                                if opts[:return_notifications?] do
                                  {record, result.notifications}
                                else
                                  record
                                end
                              else
                                if opts[:return_notifications?] do
                                  result.notifications
                                else
                                  :ok
                                end
                              end

                            %Ash.BulkResult{status: :success, records: empty} = result
                            when empty in [[], nil] ->
                              if opts[:return_destroyed?] do
                                raise Ash.Error.to_error_class(
                                        Ash.Error.Query.NotFound.exception(
                                          resource: unquote(resource),
                                          primary_key: id
                                        )
                                      )
                              else
                                if opts[:return_notifications?] do
                                  {:ok, result.notifications}
                                else
                                  :ok
                                end
                              end
                          end

                        {:error, error} ->
                          raise Ash.Error.to_error_class(error)
                      end

                    changeset ->
                      Ash.destroy!(changeset, Keyword.delete(opts, :bulk_options))
                  end
                end

              {subject, subject_args, resolve_subject, act, act!}
          end

        subject_name = elem(subject, 0)

        resolve_subject =
          quote do
            unquote(resolve_subject)
          end

        common_args =
          quote do: [
                  unquote_splicing(subject_args),
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ]

        first_opts_location = Enum.count(subject_args) + Enum.count(arg_vars_function)

        @doc """
             #{action.description || "Calls the #{action.name} action on #{inspect(resource)}."}

             #{Ash.CodeInterface.describe_action(resource, action, interface.args)}

             ## Options

             #{interface_options.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()

        @dialyzer {:nowarn_function, {interface.name, length(common_args)}}
        @doc spark_opts: [
               {first_opts_location, interface_options.schema()},
               {first_opts_location + 1, interface_options.schema()}
             ]
        def unquote(interface.name)(unquote_splicing(common_args)) do
          unquote(resolve_params_and_opts)
          unquote(resolve_subject)
          unquote(act)
        end

        @doc """
             #{action.description || "Calls the #{action.name} action on #{inspect(resource)}."}

             Raises any errors instead of returning them

             #{Ash.CodeInterface.describe_action(resource, action, interface.args)}

             ## Options

             #{interface_options.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        # sobelow_skip ["DOS.BinToAtom"]
        @dialyzer {:nowarn_function, {:"#{interface.name}!", length(common_args)}}
        @doc spark_opts: [
               {first_opts_location, interface_options.schema()},
               {first_opts_location + 1, interface_options.schema()}
             ]
        def unquote(:"#{interface.name}!")(unquote_splicing(common_args)) do
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
              :authorize?,
              :tracer,
              :changeset,
              :query,
              :input
            ])

          @dialyzer {:nowarn_function,
                     {:"#{subject_name}_to_#{interface.name}", length(common_args)}}

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
          def unquote(:"#{subject_name}_to_#{interface.name}")(unquote_splicing(common_args)) do
            unquote(resolve_params_and_opts)
            unquote(resolve_subject)
            unquote(subject)
          end
        end

        # sobelow_skip ["DOS.BinToAtom"]
        @doc """
             Runs authorization checks for `#{inspect(resource)}.#{action.name}`

             See `Ash.can/3` for more information

             ## Options

             #{Ash.Resource.Interface.CanOpts.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        @dialyzer {:nowarn_function, {:"can_#{interface.name}", length(common_args) + 1}}
        @doc spark_opts: [
               {first_opts_location + 1, Ash.Resource.Interface.CanOpts.schema()},
               {first_opts_location + 2, Ash.Resource.Interface.CanOpts.schema()}
             ]
        def unquote(:"can_#{interface.name}")(actor, unquote_splicing(common_args)) do
          {params, opts} =
            Ash.CodeInterface.params_and_opts(params_or_opts, opts, fn opts ->
              opts
              |> Ash.Resource.Interface.CanOpts.validate!()
              |> unquote(interface_options).to_options()
              |> Keyword.put(:actor, actor)
            end)

          unquote(resolve_subject)

          case unquote(subject) do
            %struct{} when struct in [Ash.Changeset, Ash.Query, Ash.ActionInput] ->
              Ash.can(unquote(subject), actor, opts)

            {:atomic, _, input} ->
              raise "Ash.can_#{unquote(interface.name)} does not support #{inspect(input)} as input."

            {:bulk, input} ->
              raise "Ash.can_#{unquote(interface.name)} does not support #{inspect(input)} as input."

            other ->
              raise "Ash.can_#{unquote(interface.name)} does not support #{inspect(other)} as input."
          end
        end

        # sobelow_skip ["DOS.BinToAtom"]
        @dialyzer {:nowarn_function, {:"can_#{interface.name}?", length(common_args) + 1}}
        @doc spark_opts: [
               {first_opts_location + 1, Ash.Resource.Interface.CanQuestionMarkOpts.schema()},
               {first_opts_location + 2, Ash.Resource.Interface.CanQuestionMarkOpts.schema()}
             ]
        @doc """
             Runs authorization checks for `#{inspect(resource)}.#{action.name}`, returning a boolean.

             See `Ash.can?/3` for more information

             ## Options

             #{Ash.Resource.Interface.CanQuestionMarkOpts.docs()}
             """
             |> Ash.CodeInterface.trim_double_newlines()
        def unquote(:"can_#{interface.name}?")(actor, unquote_splicing(common_args)) do
          {params, opts} =
            Ash.CodeInterface.params_and_opts(params_or_opts, opts, fn opts ->
              opts
              |> Ash.Resource.Interface.CanOpts.validate!()
              |> unquote(interface_options).to_options()
              |> Keyword.put(:actor, actor)
            end)

          unquote(resolve_subject)

          case unquote(subject) do
            %struct{} when struct in [Ash.Changeset, Ash.Query, Ash.ActionInput] ->
              Ash.can?(unquote(subject), actor, opts)

            {:atomic, _, input} ->
              raise "Ash.can_#{unquote(interface.name)}? does not support #{inspect(input)} as input."

            {:bulk, input} ->
              raise "Ash.can_#{unquote(interface.name)}? does not support #{inspect(input)} as input."

            other ->
              raise "Ash.can_#{unquote(interface.name)}? does not support #{inspect(other)} as input."
          end
        end
      end
    end
  end

  def describe_action(resource, action, args) do
    resource
    |> Ash.Resource.Info.action_inputs(action.name)
    |> Enum.filter(&is_atom/1)
    |> Enum.uniq()
    |> case do
      [] ->
        ""

      inputs ->
        {arguments, inputs} = Enum.split_with(inputs, &(&1 in (args || [])))

        arguments =
          Enum.map(arguments, &describe_input(resource, action, &1))

        inputs =
          Enum.map(inputs, &describe_input(resource, action, &1))

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

  def describe_calculation(resource, calculation, args) do
    calculation.arguments
    |> Enum.map(& &1.name)
    |> case do
      [] ->
        ""

      inputs ->
        {arguments, inputs} = Enum.split_with(inputs, &(&1 in args))

        arguments = Enum.sort_by(arguments, fn arg -> Enum.find_index(args, &(&1 == arg)) end)

        arguments =
          Enum.map(arguments, &describe_input(resource, calculation, &1))

        inputs =
          Enum.map(inputs, &describe_input(resource, calculation, &1))

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

  defp describe_input(resource, %{arguments: arguments}, name) do
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
  end

  defp describe(%{name: name, description: description}) when not is_nil(description) do
    "* #{name} - #{description}"
  end

  defp describe(%{name: name}) do
    "* #{name}"
  end

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
            {:ok, Ash.Query.do_filter(resource, filter)}

          {:error, error} ->
            {:error, error}
        end
    end
  end
end
