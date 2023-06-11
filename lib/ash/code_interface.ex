defmodule Ash.CodeInterface do
  @moduledoc """
  Used to define the functions of a code interface for a resource.
  """

  @doc false
  def require_action(resource, interface) do
    action = Ash.Resource.Info.action(resource, interface.action || interface.name)

    unless action do
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

      true ->
        raise "Code interface for #{action.name} has optional argument #{key} but it is not optional"
    end

    default =
      if field_type == :argument do
        field.default
      else
        if action.type == :update || (action.type == :destroy && action.soft?) do
          if is_nil(action.update_default) do
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

  @doc """
  Defines the code interface for a given resource + api combination in the current module. For example:

  ```elixir
  defmodule MyApp.Accounting do
    require Ash.CodeInterface

    Ash.CodeInterface.define_interface(MyApp.Accounting, MyApp.Accounting.Transaction)
    Ash.CodeInterface.define_interface(MyApp.Accounting, MyApp.Accounting.Account)
    Ash.CodeInterface.define_interface(MyApp.Accounting, MyApp.Accounting.Invoice)
  end
  ```

  Keep in mind that you can have this "automatically" defined in your resources by using the `define_for`
  flag in a resource.

  For example:

  ```elixir
  defmodule MyApp.Accounting.Transaction do
    use Ash.Resource

    ...

    code_interface do
      define_for MyApp.Accounting

      define :start do
        args [:invoice_id]
      end
    end
  end

  # Which can now be used like so:

  MyApp.Accounting.Transaction.start!(invoice.id)
  ```
  """
  defmacro define_interface(api, resource) do
    quote bind_quoted: [api: api, resource: resource], generated: true, location: :keep do
      interfaces_for_defaults =
        Enum.group_by(Ash.Resource.Info.calculation_interfaces(resource), fn interface ->
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

        # sobelow_skip ["DOS.BinToAtom"]
        def unquote(:"#{name}!")(unquote_splicing(args), opts \\ [])
        def unquote(name)(unquote_splicing(args), opts \\ [])
      end

      for interface <- Ash.Resource.Info.calculation_interfaces(resource) do
        calculation = Ash.Resource.Info.calculation(resource, interface.calculation)

        {arg_bindings, arg_access} =
          interface.args
          |> Kernel.||([])
          |> Ash.CodeInterface.unwrap_calc_interface_args(resource, calculation.arguments)

        @doc "Calculate `#{calculation.name}`, raising any errors. See `#{interface.name}/#{Enum.count(interface.args) + 1}` for more."
        # sobelow_skip ["DOS.BinToAtom"]
        def unquote(:"#{interface.name}!")(unquote_splicing(arg_bindings), opts) do
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

          unquote(api).calculate!(unquote(resource), unquote(interface.calculation),
            refs: refs,
            args: arguments,
            actor: opts[:actor],
            record: record
          )
        end

        @doc """
        Calculate `#{calculation.name}`, returning `{:ok, result}` or `{:error, error}`.
        #{if calculation.description, do: "\nDescription:" <> calculation.description}
        """
        # sobelow_skip ["DOS.BinToAtom"]
        def unquote(interface.name)(unquote_splicing(arg_bindings), opts) do
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

          unquote(api).calculate(unquote(resource), unquote(interface.calculation),
            refs: refs,
            args: arguments,
            actor: opts[:actor],
            record: record
          )
        end
      end

      for interface <- Ash.Resource.Info.interfaces(resource) do
        action = Ash.CodeInterface.require_action(resource, interface)

        filter_keys =
          if action.type == :read do
            if interface.get_by_identity do
              Ash.Resource.Info.identity(resource, interface.get_by_identity).keys
            else
              if interface.get_by do
                interface.get_by
              end
            end
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

        unless Enum.uniq(args) == args do
          raise """
          Arguments #{inspect(args)} for #{interface.name} are not unique!
          """
        end

        doc = """
        #{action.description || "Calls the #{action.name} action on the #{inspect(resource)} resource."}

        ## Options

        #{Spark.OptionsHelpers.docs(Ash.Resource.Interface.interface_options(action.type))}
        """

        resolve_opts_params =
          quote do
            {params, opts} =
              if opts == [] && Keyword.keyword?(params_or_opts),
                do: {%{}, params_or_opts},
                else: {params_or_opts, opts}

            params =
              unquote(args)
              |> Enum.zip([unquote_splicing(arg_vars)])
              |> Enum.reduce(params, fn {key, value}, params ->
                Map.put(params, key, value)
              end)
          end

        case action.type do
          :action ->
            resolve_opts_params_input =
              quote do
                unquote(resolve_opts_params)

                {input_opts, opts} =
                  Keyword.split(opts, [:input, :actor, :tenant, :authorize?, :tracer])

                {input, input_opts} = Keyword.pop(input_opts, :input)

                input =
                  input
                  |> Kernel.||(unquote(resource))
                  |> Ash.ActionInput.for_action(unquote(action.name), params, input_opts)
              end

            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_input)
              unquote(api).run_action(input, opts)
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 2}}
            def unquote(:"#{interface.name}!")(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_input)
              unquote(api).run_action!(input, opts)
            end

          :read ->
            resolve_opts_params_query =
              quote do
                unquote(resolve_opts_params)

                {query_opts, opts} =
                  Keyword.split(opts, [:query, :actor, :tenant, :authorize?, :tracer])

                {query, query_opts} = Keyword.pop(query_opts, :query)

                query =
                  if unquote(filter_keys) do
                    require Ash.Query
                    {filters, params} = Map.split(params, unquote(filter_keys))

                    query
                    |> Kernel.||(unquote(resource))
                    |> Ash.Query.for_read(unquote(action.name), params, query_opts)
                    |> Ash.Query.filter(filters)
                  else
                    query
                    |> Kernel.||(unquote(resource))
                    |> Ash.Query.for_read(unquote(action.name), params, query_opts)
                  end

                query = %{query | api: unquote(api)}
              end

            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_query)

              if unquote(interface.get? || action.get?) do
                query
                |> unquote(api).read_one(Keyword.delete(opts, :not_found_error?))
                |> case do
                  {:ok, nil} ->
                    if unquote(interface.not_found_error?) == false ||
                         Keyword.get(opts, :not_found_error?) == false do
                      {:ok, nil}
                    else
                      {:error, Ash.Error.Query.NotFound.exception(resource: query.resource)}
                    end

                  {:ok, result} ->
                    {:ok, result}

                  {:error, error} ->
                    {:error, error}
                end
              else
                unquote(api).read(query, opts)
              end
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 2}}
            def unquote(:"#{interface.name}!")(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_query)

              if unquote(interface.get? || action.get?) do
                query
                |> unquote(api).read_one!(Keyword.delete(opts, :not_found_error?))
                |> case do
                  nil ->
                    if unquote(interface.not_found_error?) == false ||
                         Keyword.get(opts, :not_found_error?) == false do
                      nil
                    else
                      raise Ash.Error.Query.NotFound, resource: query.resource
                    end

                  result ->
                    result
                end
              else
                unquote(api).read!(query, opts)
              end
            end

          :create ->
            resolve_opts_params_changeset =
              quote do
                unquote(resolve_opts_params)

                {changeset_opts, opts} =
                  Keyword.split(opts, [:changeset, :actor, :tenant, :authorize?, :tracer])

                {changeset, changeset_opts} = Keyword.pop(changeset_opts, :changeset)

                changeset =
                  changeset
                  |> Kernel.||(unquote(resource))
                  |> Ash.Changeset.for_create(unquote(action.name), params, changeset_opts)
              end

            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_changeset)
              unquote(api).create(changeset, opts)
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 2}}
            def unquote(:"#{interface.name}!")(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_changeset)
              unquote(api).create!(changeset, opts)
            end

          :update ->
            resolve_opts_params_changeset =
              quote do
                unquote(resolve_opts_params)

                {changeset_opts, opts} =
                  Keyword.split(opts, [:actor, :tenant, :authorize?, :tracer])

                changeset =
                  record
                  |> Ash.Changeset.for_update(unquote(action.name), params, changeset_opts)
              end

            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 3}}
            def unquote(interface.name)(
                  record,
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_changeset)
              unquote(api).update(changeset, opts)
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 3}}
            def unquote(:"#{interface.name}!")(
                  record,
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_changeset)
              unquote(api).update!(changeset, opts)
            end

          :destroy ->
            resolve_opts_params_changeset =
              quote do
                unquote(resolve_opts_params)

                {changeset_opts, opts} =
                  Keyword.split(opts, [:actor, :tenant, :authorize?, :tracer])

                changeset =
                  record
                  |> Ash.Changeset.for_destroy(unquote(action.name), params, changeset_opts)
              end

            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 3}}
            def unquote(interface.name)(
                  record,
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_changeset)
              unquote(api).destroy(changeset, opts)
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 3}}
            def unquote(:"#{interface.name}!")(
                  record,
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              unquote(resolve_opts_params_changeset)
              unquote(api).destroy!(changeset, opts)
            end
        end
      end
    end
  end
end
