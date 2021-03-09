defmodule Ash.Api.Interface do
  @moduledoc false

  defmacro __using__(_) do
    quote bind_quoted: [] do
      alias Ash.Api

      default_opts = [
        tenant: [
          type: :any,
          doc: "A tenant to use for the action."
        ],
        context: [
          type: :map,
          doc: "Context to set for the action"
        ]
      ]

      for {resource, resource_name, action} <- Ash.Api.Interface.action_interface(__MODULE__) do
        name = action.as || :"#{resource_name}_#{action.name}"

        doc =
          case action.type do
            :create ->
              opts =
                Ash.Api.create_opts_schema()
                |> Keyword.drop([:action])
                |> Keyword.merge(default_opts)

              """
              #{
                action.description ||
                  "Create a #{resource_name} with the `#{action.name}` action."
              }

              ## Options

              #{Ash.OptionsHelpers.docs(opts)}
              """

            :read ->
              opts =
                Ash.Api.read_opts_schema()
                |> Keyword.drop([:action])
                |> Keyword.merge(default_opts)

              """
              #{
                action.description ||
                  "Read a list of #{resource_name} with the `#{action.name}` action."
              }

              ## Options

              #{Ash.OptionsHelpers.docs(opts)}
              """

            :update ->
              opts =
                Ash.Api.update_opts_schema()
                |> Keyword.drop([:action])
                |> Keyword.merge(default_opts)

              """
              #{
                action.description ||
                  "Update a #{resource_name} with the `#{action.name}` action."
              }

              ## Options

              #{Ash.OptionsHelpers.docs(opts)}
              """

            :destroy ->
              opts =
                Ash.Api.destroy_opts_schema()
                |> Keyword.drop([:action])
                |> Keyword.merge(default_opts)

              """
              #{
                action.description ||
                  "Destroy a #{resource_name} with the `#{action.name}` action."
              }

              ## Options

              #{Ash.OptionsHelpers.docs(opts)}
              """
          end

        case action.type do
          :create ->
            @doc doc
            @spec unquote(name)(Keyword.t() | map, Keyword.t()) ::
                    {:ok, unquote(resource).t()} | {:error, Ash.Error.t()}
            def unquote(name)(params \\ [], opts \\ []) do
              opts
              |> Keyword.get(:changeset, unquote(resource))
              |> Ash.Changeset.for_create(unquote(action.name), params, opts)
              |> Ash.Api.Interface.set_tenant(opts)
              |> Ash.Changeset.set_context(opts[:context] || %{})
              |> __MODULE__.create()
            end

            @doc doc
            @spec unquote(:"#{name}!")(Keyword.t() | map, Keyword.t()) ::
                    unquote(resource).t() | no_return
            # sobelow_skip ["DOS.BinToAtom"]
            def unquote(:"#{name}!")(params \\ [], opts \\ []) do
              unquote(resource)
              |> Ash.Changeset.for_create(unquote(action.name), params, opts)
              |> Ash.Api.Interface.set_tenant(opts)
              |> Ash.Changeset.set_context(opts[:context] || %{})
              |> __MODULE__.create!()
            end

          :update ->
            @doc doc
            @spec unquote(name)(
                    unquote(resource).t() | Ash.Changeset.t(),
                    map | Keyword.t(),
                    Keyword.t()
                  ) ::
                    {:ok, unquote(resource).t()} | {:error, Ash.Error.t()}
            def unquote(name)(record, params \\ [], opts \\ []) do
              record
              |> Ash.Changeset.for_update(unquote(action.name), params, opts)
              |> Ash.Api.Interface.set_tenant(opts)
              |> Ash.Changeset.set_context(opts[:context] || %{})
              |> __MODULE__.update()
            end

            @doc doc
            @spec unquote(:"#{name}!")(
                    unquote(resource).t() | Ash.Changeset.t(),
                    Keyword.t() | map,
                    Keyword.t()
                  ) ::
                    unquote(resource).t() | no_return
            # sobelow_skip ["DOS.BinToAtom"]
            def unquote(:"#{name}!")(record, params \\ [], opts \\ []) do
              record
              |> Ash.Changeset.for_update(unquote(action.name), params, opts)
              |> Ash.Api.Interface.set_tenant(opts)
              |> Ash.Changeset.set_context(opts[:context] || %{})
              |> __MODULE__.update!()
            end

          :destroy ->
            @doc doc
            @spec unquote(name)(
                    unquote(resource).t() | Ash.Changeset.t(),
                    map | Keyword.t(),
                    Keyword.t()
                  ) ::
                    :ok | {:error, Ash.Error.t()}
            def unquote(name)(record, params \\ [], opts \\ []) do
              record
              |> Ash.Changeset.for_destroy(unquote(action.name), params, opts)
              |> Ash.Api.Interface.set_tenant(opts)
              |> Ash.Changeset.set_context(opts[:context] || %{})
              |> __MODULE__.destroy()
            end

            @doc doc
            @spec unquote(:"#{name}!")(
                    unquote(resource).t() | Ash.Changeset.t(),
                    Keyword.t() | map,
                    Keyword.t()
                  ) ::
                    :ok | no_return
            # sobelow_skip ["DOS.BinToAtom"]
            def unquote(:"#{name}!")(record, params \\ [], opts \\ []) do
              record
              |> Ash.Changeset.for_destroy(unquote(action.name), params, opts)
              |> Ash.Api.Interface.set_tenant(opts)
              |> Ash.Changeset.set_context(opts[:context] || %{})
              |> __MODULE__.destroy!()
            end

          :read ->
            @doc doc
            @spec unquote(name)(map | Keyword.t(), Keyword.t(), Ash.Query.t() | nil) ::
                    list(unquote(resource).t())
                    | list(unquote(resource).t())
                    | {:error, Ash.Error.t()}
            def unquote(name)(params, opts \\ [], query \\ nil) do
              query
              |> Kernel.||(unquote(resource))
              |> Ash.Query.for_read(unquote(action.name), params, opts)
              |> Ash.Api.Interface.set_tenant(opts)
              |> Ash.Query.set_context(opts[:context] || %{})
              |> __MODULE__.read()
            end

            @doc doc
            @spec unquote(:"#{name}!")(Keyword.t() | map, Keyword.t(), Ash.Query.t() | nil) ::
                    list(unquote(resource).t())
                    | list(unquote(resource).t())
                    | no_return
            # sobelow_skip ["DOS.BinToAtom"]
            def unquote(:"#{name}!")(params, opts \\ [], query \\ nil) do
              query
              |> Kernel.||(unquote(resource))
              |> Ash.Query.for_read(unquote(action.name), params, opts)
              |> Ash.Api.Interface.set_tenant(opts)
              |> Ash.Query.set_context(opts[:context] || %{})
              |> __MODULE__.read!()
            end
        end
      end

      opts =
        Ash.Api.read_opts_schema()
        |> Keyword.merge(default_opts)

      for {resource, resource_name} <- Ash.Api.Interface.resources_with_names(__MODULE__) do
        if Ash.Resource.Info.primary_action(resource, :read) do
          @doc """
          Get a #{resource_name} by primary key

          ## Options

          #{Ash.OptionsHelpers.docs(opts)}
          """
          def unquote(:"get_#{resource_name}")(key, params \\ []) do
            __MODULE__.get(unquote(resource), key, params)
          end

          @doc """
          Get a #{resource_name} by primary key

          ## Options

          #{Ash.OptionsHelpers.docs(opts)}
          """
          def unquote(:"get_#{resource_name}!")(key, params \\ []) do
            __MODULE__.get!(unquote(resource), key, params)
          end
        end
      end

      for {resource, resource_name, identity} <- Ash.Api.Interface.getters(__MODULE__) do
        @doc """
        Get a #{resource_name} by #{String.replace(to_string(identity.name), "_", " ")}

        ## Options

        #{Ash.OptionsHelpers.docs(opts)}
        """
        vars = Enum.map(identity.keys, &{&1, [], Elixir})

        def unquote(:"get_#{resource_name}_by_#{identity.name}")(
              unquote_splicing(vars),
              params \\ []
            ) do
          id = Enum.zip(unquote(identity.keys), [unquote_splicing(vars)])
          __MODULE__.get(unquote(resource), id, params)
        end

        def unquote(:"get_#{resource_name}_by_#{identity.name}!")(
              unquote_splicing(vars),
              params \\ []
            ) do
          id = Enum.zip(unquote(identity.keys), [unquote_splicing(vars)])
          __MODULE__.get!(unquote(resource), id, params)
        end
      end

      def get!(resource, id, params \\ []) do
        Api.get!(__MODULE__, resource, id, params)
      end

      def get(resource, id, params \\ []) do
        case Api.get(__MODULE__, resource, id, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def read!(query, opts \\ [])

      def read!(query, opts) do
        Api.read!(__MODULE__, query, opts)
      end

      def read(query, opts \\ [])

      def read(query, opts) do
        case Api.read(__MODULE__, query, opts) do
          {:ok, results, query} -> {:ok, results, query}
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def read_one!(query, opts \\ [])

      def read_one!(query, opts) do
        Api.read_one!(__MODULE__, query, opts)
      end

      def read_one(query, opts \\ [])

      def read_one(query, opts) do
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
end
