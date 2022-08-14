defmodule Ash.CodeInterface do
  @moduledoc """
  Used to define the functions of a code interface for a resource.

  For more information on defining code interfaces, see: `Ash.Resource.Dsl.html#module-code_interface`
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
    field =
      case Ash.Resource.Info.attribute(resource, key) do
        nil ->
          Enum.find(action.arguments, fn argument ->
            argument.name == key
          end)

        attribute ->
          attribute
      end

    if !field.allow_nil? do
      raise "Code interface for #{action.name} has optional argument #{key} but it is not optional"
    end

    default =
      if action.type == :update || (action.type == :destroy && action.soft?) do
        if is_nil(action.update_default) do
          field.default
        else
          field.update_default
        end
      else
        field.default
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

        case action.type do
          :read ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(
                  __MODULE__,
                  elem(__ENV__.function, 0),
                  [
                    unquote_splicing(arg_vars_function),
                    %{},
                    params_or_opts
                  ]
                )
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                query =
                  if unquote(filter_keys) do
                    require Ash.Query
                    {filters, input} = Map.split(input, unquote(filter_keys))

                    opts[:query]
                    |> Kernel.||(unquote(resource))
                    |> Ash.Query.for_read(
                      unquote(action.name),
                      input,
                      Keyword.take(opts, [:actor, :tenant, :authorize?])
                    )
                    |> Ash.Query.filter(filters)
                  else
                    opts[:query]
                    |> Kernel.||(unquote(resource))
                    |> Ash.Query.for_read(
                      unquote(action.name),
                      input,
                      Keyword.take(opts, [:actor, :tenant, :authorize?])
                    )
                  end

                if unquote(interface.get? || action.get?) do
                  query
                  |> unquote(api).read_one(
                    Keyword.drop(opts, [:query, :tenant, :authorize?, :actor])
                  )
                  |> case do
                    {:ok, nil} ->
                      {:error, Ash.Error.Query.NotFound.exception(resource: query.resource)}

                    {:ok, result} ->
                      {:ok, result}

                    {:error, error} ->
                      {:error, error}
                  end
                else
                  unquote(api).read(
                    query,
                    Keyword.drop(opts, [:query, :tenant, :actor, :authorize?])
                  )
                end
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
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(
                  __MODULE__,
                  elem(__ENV__.function, 0),
                  [
                    unquote_splicing(arg_vars),
                    %{},
                    params_or_opts
                  ]
                )
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                query =
                  if unquote(filter_keys) do
                    require Ash.Query
                    {filters, input} = Map.split(input, unquote(filter_keys))

                    opts[:query]
                    |> Kernel.||(unquote(resource))
                    |> Ash.Query.for_read(
                      unquote(action.name),
                      input,
                      Keyword.take(opts, [:actor, :tenant, :authorize?])
                    )
                    |> Ash.Query.filter(filters)
                  else
                    opts[:query]
                    |> Kernel.||(unquote(resource))
                    |> Ash.Query.for_read(
                      unquote(action.name),
                      input,
                      Keyword.take(opts, [:actor, :tenant, :authorize?])
                    )
                  end

                if unquote(interface.get? || action.get?) do
                  query
                  |> unquote(api).read_one!(
                    Keyword.drop(opts, [:query, :tenant, :authorize?, :actor])
                  )
                  |> case do
                    nil ->
                      raise Ash.Error.Query.NotFound, resource: query.resource

                    result ->
                      result
                  end
                else
                  unquote(api).read!(
                    query,
                    Keyword.drop(opts, [:query, :tenant, :actor, :authorize?])
                  )
                end
              end
            end

          :create ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  opts[:changeset]
                  |> Kernel.||(unquote(resource))
                  |> Ash.Changeset.for_create(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant, :authorize?])
                  )

                unquote(api).create(
                  changeset,
                  Keyword.drop(opts, [:actor, :changeset, :tenant, :authorize?])
                )
              end
            end

            @doc doc
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 2}}
            # sobelow_skip ["DOS.BinToAtom"]
            def unquote(:"#{interface.name}!")(
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  (opts[:changeset] || unquote(resource))
                  |> Ash.Changeset.for_create(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant, :authorize?])
                  )

                unquote(api).create!(
                  changeset,
                  Keyword.drop(opts, [:actor, :changeset, :authorize?])
                )
              end
            end

          :update ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 3}}
            def unquote(interface.name)(
                  record,
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  record,
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  record
                  |> Ash.Changeset.for_update(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant, :authorize?])
                  )

                unquote(api).update(changeset, Keyword.drop(opts, [:actor, :tenant, :authorize?]))
              end
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
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  record,
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  record
                  |> Ash.Changeset.for_update(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant, :authorize?])
                  )

                unquote(api).update!(
                  changeset,
                  Keyword.drop(opts, [:actor, :tenant, :authorize?])
                )
              end
            end

          :destroy ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 3}}
            def unquote(interface.name)(
                  record,
                  unquote_splicing(arg_vars_function),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  record,
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  record
                  |> Ash.Changeset.for_destroy(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant, :authorize?])
                  )

                unquote(api).destroy(
                  changeset,
                  Keyword.drop(opts, [:actor, :tenant, :authorize?])
                )
              end
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
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  record,
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  record
                  |> Ash.Changeset.for_destroy(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant, :authorize?])
                  )

                unquote(api).destroy!(
                  changeset,
                  Keyword.drop(opts, [:actor, :tenant, :authorize?])
                )
              end
            end
        end
      end
    end
  end
end
