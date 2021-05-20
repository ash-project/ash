defmodule Ash.Api.Interface do
  @moduledoc false

  @doc false
  def require_action(resource, interface) do
    action = Ash.Resource.Info.action(resource, interface.action || interface.name)

    unless action do
      raise Ash.Error.Dsl.DslError,
        module: resource,
        message:
          "The interface of #{inspect(resource)} refers to a non-existent action #{
            interface.action || interface.name
          }",
        path: [:interfaces, :interface, interface.name]
    end

    action
  end

  defmacro define_interface(api, resource) do
    quote bind_quoted: [api: api, resource: resource], generated: true, location: :keep do
      for interface <- Ash.Resource.Info.interfaces(resource) do
        action = Ash.Api.Interface.require_action(resource, interface)

        args = interface.args || []
        arg_vars = Enum.map(args, &{&1, [], Elixir})

        doc = """
        #{
          action.description ||
            "Calls the #{action.name} action on the #{inspect(resource)} resource."
        }

        ## Options

        #{Ash.OptionsHelpers.docs(Ash.Resource.Interface.interface_options())}
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
                  opts[:query]
                  |> Kernel.||(unquote(resource))
                  |> Ash.Query.for_read(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

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
                  opts[:query]
                  |> Kernel.||(unquote(resource))
                  |> Ash.Query.for_read(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

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
                  unquote(resource)
                  |> Ash.Changeset.for_create(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).create(changeset, opts)
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
                  unquote(resource)
                  |> Ash.Changeset.for_create(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).create!(changeset, Keyword.drop(opts, [:actor, :tenant]))
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

  defmacro __using__(_) do
    quote bind_quoted: [], generated: true do
      alias Ash.Api

      for resource <- Ash.Api.resources(__MODULE__) do
        Ash.Api.Interface.define_interface(__MODULE__, resource)
      end

      def get!(resource, id_or_filter, params \\ []) do
        Ash.Api.Interface.enforce_resource!(resource)

        Api.get!(__MODULE__, resource, id_or_filter, params)
      end

      def get(resource, id_or_filter, params \\ []) do
        Ash.Api.Interface.enforce_resource!(resource)
        Ash.Api.Interface.enforce_keyword_list!(params)

        case Api.get(__MODULE__, resource, id_or_filter, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def read!(query, opts \\ [])

      def read!(query, opts) do
        Ash.Api.Interface.enforce_query_or_resource!(query)
        Ash.Api.Interface.enforce_keyword_list!(opts)

        Api.read!(__MODULE__, query, opts)
      end

      def read(query, opts \\ [])

      def read(query, opts) do
        Ash.Api.Interface.enforce_query_or_resource!(query)
        Ash.Api.Interface.enforce_keyword_list!(opts)

        case Api.read(__MODULE__, query, opts) do
          {:ok, results, query} -> {:ok, results, query}
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def read_one!(query, opts \\ [])

      def read_one!(query, opts) do
        Ash.Api.Interface.enforce_query_or_resource!(query)
        Ash.Api.Interface.enforce_keyword_list!(opts)

        Api.read_one!(__MODULE__, query, opts)
      end

      def read_one(query, opts \\ [])

      def read_one(query, opts) do
        Ash.Api.Interface.enforce_query_or_resource!(query)
        Ash.Api.Interface.enforce_keyword_list!(opts)

        case Api.read_one(__MODULE__, query, opts) do
          {:ok, result} -> {:ok, result}
          {:ok, result, query} -> {:ok, result, query}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def page!(page, request) do
        Api.page!(__MODULE__, page, request)
      end

      def page(page, request) do
        case Api.page(__MODULE__, page, request) do
          {:ok, page} -> {:ok, page}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def load!(data, query, opts \\ []) do
        Api.load!(__MODULE__, data, query, opts)
      end

      def load(data, query, opts \\ []) do
        case Api.load(__MODULE__, data, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def create!(changeset, params \\ []) do
        Api.create!(__MODULE__, changeset, params)
      end

      def create(changeset, params \\ []) do
        case Api.create(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:ok, instance, notifications} -> {:ok, instance, notifications}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def update!(changeset, params \\ []) do
        Api.update!(__MODULE__, changeset, params)
      end

      def update(changeset, params \\ []) do
        case Api.update(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:ok, instance, notifications} -> {:ok, instance, notifications}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def destroy!(record, params \\ []) do
        Api.destroy!(__MODULE__, record, params)
      end

      def destroy(record, params \\ []) do
        case Api.destroy(__MODULE__, record, params) do
          :ok -> :ok
          {:ok, notifications} -> {:ok, notifications}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def reload!(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.Resource.Info.primary_key(resource)) |> Enum.to_list()
        params = Keyword.put_new(params, :tenant, Map.get(record.__metadata__, :tenant))

        get!(resource, id, params)
      end

      def reload(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.Resource.Info.primary_key(resource)) |> Enum.to_list()
        params = Keyword.put_new(params, :tenant, Map.get(record.__metadata__, :tenant))
        get(resource, id, params)
      end
    end
  end

  @doc false
  def set_tenant(query_or_changeset, opts) do
    case Keyword.fetch(opts, :tenant) do
      {:ok, tenant} ->
        case query_or_changeset do
          %Ash.Query{} = query ->
            Ash.Query.set_tenant(query, tenant)

          %Ash.Changeset{} = changeset ->
            Ash.Changeset.set_tenant(changeset, tenant)

          other ->
            other
        end

      :error ->
        query_or_changeset
    end
  end

  @doc false
  def action_interface(api) do
    api
    |> Ash.Api.resource_references()
    |> Enum.flat_map(fn %{resource: resource} = reference ->
      resource_name = name(reference)

      resource
      |> Ash.Resource.Info.actions()
      |> Enum.map(fn action ->
        {resource, resource_name, action}
      end)
    end)
  end

  @doc false
  def getters(api) do
    api
    |> Ash.Api.resource_references()
    |> Enum.flat_map(fn %{resource: resource} = reference ->
      if Ash.Resource.Info.primary_action(resource, :read) do
        resource_name = name(reference)

        resource
        |> Ash.Resource.Info.identities()
        |> Enum.map(fn identity ->
          {resource, resource_name, identity}
        end)
      else
        []
      end
    end)
  end

  @doc false
  def resources_with_names(api) do
    api
    |> Ash.Api.resource_references()
    |> Enum.map(fn ref ->
      {ref.resource, name(ref)}
    end)
  end

  @doc false
  def name(reference) do
    reference
    |> Map.get(:as)
    |> Kernel.||(
      reference.resource
      |> Module.split()
      |> List.last()
      |> to_string()
      |> Macro.underscore()
    )
    |> to_string()
  end

  defmacro enforce_query_or_resource!(query_or_resource) do
    quote generated: true do
      case Ash.Api.Interface.do_enforce_query_or_resource!(unquote(query_or_resource)) do
        :ok ->
          :ok

        _ ->
          {fun, arity} = __ENV__.function
          mfa = "#{inspect(__ENV__.module)}.#{fun}/#{arity}"

          raise "#{mfa} expected an %Ash.Query{} or an Ash Resource but instead got #{
                  inspect(unquote(query_or_resource))
                }"
      end
    end
  end

  def do_enforce_query_or_resource!(query_or_resource)
  def do_enforce_query_or_resource!(%Ash.Query{}), do: :ok

  def do_enforce_query_or_resource!(resource) when is_atom(resource) do
    if Ash.Resource.Info.resource?(resource), do: :ok, else: :error
  end

  def do_enforce_query_or_resource!(_something), do: :error

  defmacro enforce_resource!(resource) do
    quote generated: true do
      if Ash.Resource.Info.resource?(unquote(resource)) do
        :ok
      else
        {fun, arity} = __ENV__.function
        mfa = "#{inspect(__ENV__.module)}.#{fun}/#{arity}"

        raise Ash.Error.Invalid.NoSuchResource,
          message: "#{mfa} expected an Ash Resource but instead got #{inspect(unquote(resource))}"
      end
    end
  end

  defmacro enforce_keyword_list!(list) do
    quote generated: true do
      if Keyword.keyword?(unquote(list)) do
        :ok
      else
        {fun, arity} = __ENV__.function
        mfa = "#{inspect(__ENV__.module)}.#{fun}/#{arity}"
        raise "#{mfa} expected a keyword list, but instead got #{inspect(unquote(list))}"
      end
    end
  end
end
