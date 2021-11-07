defmodule Ash.CodeInterface do
  @moduledoc """
  Used to define the functions of a code interface for a resource.

  For more information on defining code interfaces, see: `Ash.Resource.Dsl.html#module-code_interface`
  """

  @doc false
  def require_action(resource, interface) do
    action = Ash.Resource.Info.action(resource, interface.action || interface.name)

    unless action do
      raise Ash.Error.Dsl.DslError,
        module: resource,
        message:
          "The interface of #{inspect(resource)} refers to a non-existent action #{interface.action || interface.name}",
        path: [:interfaces, :interface, interface.name]
    end

    action
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

        args = List.wrap(filter_keys) ++ (interface.args || [])
        arg_vars = Enum.map(args, &{&1, [], Elixir})

        unless Enum.uniq(args) == args do
          raise """
          Arguments #{inspect(args)} for #{interface.name} are not unique!
          """
        end

        doc = """
        #{action.description || "Calls the #{action.name} action on the #{inspect(resource)} resource."}

        ## Options

        #{Ash.OptionsHelpers.docs(Ash.Resource.Interface.interface_options(action.type))}
        """

        case action.type do
          :read ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars),
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
                      Keyword.take(opts, [:actor, :tenant])
                    )
                    |> Ash.Query.filter(filters)
                  else
                    opts[:query]
                    |> Kernel.||(unquote(resource))
                    |> Ash.Query.for_read(
                      unquote(action.name),
                      input,
                      Keyword.take(opts, [:actor, :tenant])
                    )
                  end

                if unquote(interface.get?) do
                  query
                  |> unquote(api).read_one(Keyword.drop(opts, [:query, :tenant]))
                  |> case do
                    {:ok, nil} ->
                      {:error, Ash.Error.Query.NotFound.exception(resource: query.resource)}

                    {:ok, result} ->
                      {:ok, result}

                    {:error, error} ->
                      {:error, error}
                  end
                else
                  unquote(api).read(query, Keyword.drop(opts, [:query, :tenant]))
                end
              end
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 2}}
            def unquote(:"#{interface.name}!")(
                  unquote_splicing(arg_vars),
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
                      Keyword.take(opts, [:actor, :tenant])
                    )
                    |> Ash.Query.filter(filters)
                  else
                    opts[:query]
                    |> Kernel.||(unquote(resource))
                    |> Ash.Query.for_read(
                      unquote(action.name),
                      input,
                      Keyword.take(opts, [:actor, :tenant])
                    )
                  end

                if unquote(interface.get?) do
                  query
                  |> unquote(api).read_one!(Keyword.drop(opts, [:query, :tenant]))
                  |> case do
                    nil ->
                      raise Ash.Error.Query.NotFound, resource: query.resource

                    result ->
                      result
                  end
                else
                  unquote(api).read!(query, Keyword.drop(opts, [:query, :tenant]))
                end
              end
            end

          :create ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars),
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
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).create(changeset, Keyword.drop(opts, [:actor, :changeset, :tenant]))
              end
            end

            @doc doc
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 2}}
            # sobelow_skip ["DOS.BinToAtom"]
            def unquote(:"#{interface.name}!")(
                  unquote_splicing(arg_vars),
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
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).create!(changeset, Keyword.drop(opts, [:actor, :changeset]))
              end
            end

          :update ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 3}}
            def unquote(interface.name)(
                  record,
                  unquote_splicing(arg_vars),
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
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).update(changeset, Keyword.drop(opts, [:actor, :tenant]))
              end
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 3}}
            def unquote(:"#{interface.name}!")(
                  record,
                  unquote_splicing(arg_vars),
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
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).update!(changeset, Keyword.drop(opts, [:actor, :tenant]))
              end
            end

          :destroy ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 3}}
            def unquote(interface.name)(
                  record,
                  unquote_splicing(arg_vars),
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
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).destroy(changeset, Keyword.drop(opts, [:actor, :tenant]))
              end
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 3}}
            def unquote(:"#{interface.name}!")(
                  record,
                  unquote_splicing(arg_vars),
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
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).destroy!(changeset, Keyword.drop(opts, [:actor, :tenant]))
              end
            end
        end
      end
    end
  end
end
