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
        opt_schema = Ash.Resource.Interface.interface_options(:calculate)

        @doc """
        Calculate `#{calculation.name}`, raising any errors.

        #{if calculation.description, do: "\n### Description:" <> calculation.description}

        ### Options

        #{Spark.Options.docs(Spark.Options.docs(Ash.Resource.Interface.interface_options(:calculate)))}
        """
        @doc spark_opts: [
               {opts_location, opt_schema}
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

          Ash.calculate!(unquote(resource), unquote(interface.calculation),
            domain: unquote(domain),
            refs: refs,
            args: arguments,
            actor: opts[:actor],
            record: record
          )
        end

        @doc """
        Calculate `#{calculation.name}`, returning `{:ok, result}` or `{:error, error}`.

        #{if calculation.description, do: "\n### Description:" <> calculation.description}

        ### Options

        #{Spark.Options.docs(Spark.Options.docs(Ash.Resource.Interface.interface_options(:calculate)))}
        """
        @doc spark_opts: [
               {opts_location, opt_schema}
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

          Ash.calculate(unquote(resource), unquote(interface.calculation),
            domain: unquote(domain),
            refs: refs,
            args: arguments,
            actor: opts[:actor],
            record: record
          )
        end
      end

      for interface <- interfaces do
        action = Ash.CodeInterface.require_action(resource, interface)

        filter_keys =
          cond do
            action.type != :read ->
              []

            interface.get_by_identity ->
              Ash.Resource.Info.identity(resource, interface.get_by_identity).keys

            interface.get_by ->
              interface.get_by

            interface.get? ->
              Ash.Resource.Info.primary_key(resource)

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

        unless Enum.uniq(args) == args do
          raise """
          Arguments #{inspect(args)} for #{interface.name} are not unique!
          """
        end

        doc = """
        #{action.description || "Calls the #{action.name} action on the #{inspect(resource)} resource."}

        ## Options

        #{Spark.Options.docs(Ash.Resource.Interface.interface_options(action.type))}
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
                    Keyword.split(opts, [:query, :actor, :tenant, :authorize?, :tracer])

                  {query, query_opts} = Keyword.pop(query_opts, :query)
                  query_opts = Keyword.put(query_opts, :domain, unquote(domain))

                  query =
                    if unquote(filter_keys) && !Enum.empty?(unquote(filter_keys)) do
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
                if interface.get? || action.get? do
                  quote do
                    unquote(resolve_not_found_error?)

                    Ash.read_one(query, opts)
                    |> case do
                      {:ok, nil} when not_found_error? ->
                        {:error, Ash.Error.Query.NotFound.exception(resource: query.resource)}

                      result ->
                        result
                    end
                  end
                else
                  quote do: Ash.read(query, opts)
                end

              act! =
                if interface.get? || action.get? do
                  quote do
                    unquote(resolve_not_found_error?)

                    Ash.read_one!(query, opts)
                    |> case do
                      nil when not_found_error? ->
                        raise Ash.Error.Query.NotFound, resource: query.resource

                      result ->
                        result
                    end
                  end
                else
                  quote do: Ash.read!(query, opts)
                end

              {subject, [], resolve_subject, act, act!}

            :create ->
              subject = quote do: changeset

              resolve_subject =
                quote do
                  {changeset_opts, opts} =
                    Keyword.split(opts, [:changeset, :actor, :tenant, :authorize?, :tracer])

                  {changeset, changeset_opts} = Keyword.pop(changeset_opts, :changeset)
                  changeset_opts = Keyword.put(changeset_opts, :domain, unquote(domain))

                  changeset =
                    changeset
                    |> Kernel.||(unquote(resource))
                    |> Ash.Changeset.for_create(unquote(action.name), params, changeset_opts)
                end

              act = quote do: Ash.create(changeset, opts)
              act! = quote do: Ash.create!(changeset, opts)

              {subject, [], resolve_subject, act, act!}

            :update ->
              subject = quote do: changeset
              subject_args = quote do: [record]

              resolve_subject =
                quote do
                  {changeset_opts, opts} =
                    Keyword.split(opts, [:actor, :tenant, :authorize?, :tracer])

                  changeset_opts = Keyword.put(changeset_opts, :domain, unquote(domain))

                  changeset =
                    record
                    |> Ash.Changeset.for_update(unquote(action.name), params, changeset_opts)
                end

              act = quote do: Ash.update(changeset, opts)
              act! = quote do: Ash.update!(changeset, opts)

              {subject, subject_args, resolve_subject, act, act!}

            :destroy ->
              subject = quote do: changeset
              subject_args = quote do: [record]

              resolve_subject =
                quote do
                  {changeset_opts, opts} =
                    Keyword.split(opts, [:actor, :tenant, :authorize?, :tracer])

                  changeset_opts = Keyword.put(changeset_opts, :domain, unquote(domain))

                  changeset =
                    record
                    |> Ash.Changeset.for_destroy(unquote(action.name), params, changeset_opts)
                end

              act = quote do: Ash.destroy(changeset, opts)
              act! = quote do: Ash.destroy!(changeset, opts)

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
        opt_schema = Ash.Resource.Interface.interface_options(action.type)

        @doc doc
        @dialyzer {:nowarn_function, {interface.name, length(common_args)}}
        @doc spark_opts: [
               {first_opts_location, opt_schema},
               {first_opts_location + 1, opt_schema}
             ]
        def unquote(interface.name)(unquote_splicing(common_args)) do
          unquote(resolve_opts_params)
          unquote(resolve_subject)
          unquote(act)
        end

        @doc doc
        # sobelow_skip ["DOS.BinToAtom"]
        @dialyzer {:nowarn_function, {:"#{interface.name}!", length(common_args)}}
        @doc spark_opts: [
               {first_opts_location, opt_schema},
               {first_opts_location + 1, opt_schema}
             ]
        def unquote(:"#{interface.name}!")(unquote_splicing(common_args)) do
          unquote(resolve_opts_params)
          unquote(resolve_subject)
          unquote(act!)
        end

        # sobelow_skip ["DOS.BinToAtom"]
        @dialyzer {:nowarn_function,
                   {:"#{subject_name}_to_#{interface.name}", length(common_args)}}

        @doc spark_opts: [
               {first_opts_location, opt_schema},
               {first_opts_location + 1, opt_schema}
             ]
        def unquote(:"#{subject_name}_to_#{interface.name}")(unquote_splicing(common_args)) do
          unquote(resolve_opts_params)
          unquote(resolve_subject)
          unquote(subject)
        end

        # sobelow_skip ["DOS.BinToAtom"]
        @dialyzer {:nowarn_function, {:"can_#{interface.name}", length(common_args) + 1}}
        @doc spark_opts: [
               {first_opts_location + 1, opt_schema},
               {first_opts_location + 2, opt_schema}
             ]
        def unquote(:"can_#{interface.name}")(actor, unquote_splicing(common_args)) do
          unquote(resolve_opts_params)
          opts = Keyword.put(opts, :actor, actor)
          unquote(resolve_subject)
          Ash.can(unquote(subject), actor, opts)
        end

        # sobelow_skip ["DOS.BinToAtom"]
        @dialyzer {:nowarn_function, {:"can_#{interface.name}?", length(common_args) + 1}}
        @doc spark_opts: [
               {first_opts_location + 1, opt_schema},
               {first_opts_location + 2, opt_schema}
             ]
        def unquote(:"can_#{interface.name}?")(actor, unquote_splicing(common_args)) do
          unquote(resolve_opts_params)
          opts = Keyword.put(opts, :actor, actor)
          unquote(resolve_subject)
          Ash.can?(unquote(subject), actor, opts)
        end
      end
    end
  end
end
