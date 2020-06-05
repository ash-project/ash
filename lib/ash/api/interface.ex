defmodule Ash.Api.Interface do
  @moduledoc false

  import Ash.OptionsHelpers, only: [merge_schemas: 3]

  alias Ash.Actions.{Create, Destroy, Read, SideLoad, Update}
  alias Ash.Error.Interface.NoSuchResource

  @global_opts [
    verbose?: [
      type: :boolean,
      default: false,
      doc: "Log engine operations (very verbose?)"
    ],
    action: [
      type: :any,
      doc: "The action to use, either an Action struct or the name of the action"
    ],
    authorize?: [
      type: :boolean,
      default: false,
      doc:
        "If an actor is provided, authorization happens automatically. If not, this flag can be used to authorize with no user."
    ],
    actor: [
      type: :any,
      doc:
        "If an actor is provided, it will be used in conjunction with the authorizers of a resource to authorize access"
    ]
  ]

  @read_opts_schema merge_schemas([], @global_opts, "Global Options")

  @side_load_opts_schema merge_schemas([], @global_opts, "Global Options")

  @get_opts_schema [
                     side_load: [
                       type: :any,
                       doc:
                         "Side loads to include in the query, same as you would pass to `Ash.Query.side_load/2`"
                     ]
                   ]
                   |> merge_schemas(@global_opts, "Global Options")

  @shared_create_and_update_opts_schema [
                                          attributes: [
                                            type: {:custom, Ash.OptionsHelpers, :map, []},
                                            default: %{},
                                            doc: "Changes to be applied to attribute values"
                                          ],
                                          relationships: [
                                            type: {:custom, Ash.OptionsHelpers, :map, []},
                                            default: %{},
                                            doc: "Changes to be applied to relationship values"
                                          ]
                                        ]
                                        |> merge_schemas(@global_opts, "Global Options")

  @create_opts_schema [
                        upsert?: [
                          type: :boolean,
                          default: false,
                          doc:
                            "If a conflict is found based on the primary key, the record is updated in the database (requires upsert support)"
                        ]
                      ]
                      |> merge_schemas(@global_opts, "Global Options")
                      |> merge_schemas(
                        @shared_create_and_update_opts_schema,
                        "Shared Create/Edit Options"
                      )

  @update_opts_schema []
                      |> merge_schemas(@global_opts, "Global Options")
                      |> merge_schemas(
                        @shared_create_and_update_opts_schema,
                        "Shared Create/Edit Options"
                      )

  @destroy_opts_schema merge_schemas([], @global_opts, "Global Opts")

  @doc """
  #{NimbleOptions.docs(@get_opts_schema)}
  """
  @callback get!(resource :: Ash.resource(), id_or_filter :: term(), params :: Keyword.t()) ::
              Ash.record() | no_return

  @doc """
  #{NimbleOptions.docs(@get_opts_schema)}
  """
  @callback get(resource :: Ash.resource(), id_or_filter :: term(), params :: Keyword.t()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  #{NimbleOptions.docs(@read_opts_schema)}
  """
  @callback read!(resource :: Ash.resource(), params :: Keyword.t()) ::
              list(Ash.resource()) | no_return

  @doc """
  #{NimbleOptions.docs(@read_opts_schema)}
  """
  @callback read(resource :: Ash.resource(), params :: Keyword.t()) ::
              {:ok, list(Ash.resource())} | {:error, Ash.error()}

  @doc """
  #{NimbleOptions.docs(@side_load_opts_schema)}
  """
  @callback side_load!(resource :: Ash.resource(), params :: Keyword.t()) ::
              list(Ash.resource()) | no_return

  @doc """
  #{NimbleOptions.docs(@side_load_opts_schema)}
  """
  @callback side_load(resource :: Ash.resource(), params :: Keyword.t()) ::
              {:ok, list(Ash.resource())} | {:error, Ash.error()}

  @doc """
  #{NimbleOptions.docs(@create_opts_schema)}
  """
  @callback create!(resource :: Ash.resource(), params :: Keyword.t()) ::
              Ash.record() | no_return

  @doc """
  #{NimbleOptions.docs(@create_opts_schema)}
  """
  @callback create(resource :: Ash.resource(), params :: Keyword.t()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  #{NimbleOptions.docs(@update_opts_schema)}
  """
  @callback update!(record :: Ash.record(), params :: Keyword.t()) ::
              Ash.record() | no_return

  @doc """
  #{NimbleOptions.docs(@update_opts_schema)}
  """
  @callback update(record :: Ash.record(), params :: Keyword.t()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  #{NimbleOptions.docs(@destroy_opts_schema)}
  """
  @callback destroy!(record :: Ash.record(), params :: Keyword.t()) :: :ok | no_return

  @doc """
  #{NimbleOptions.docs(@destroy_opts_schema)}
  """
  @callback destroy(record :: Ash.record(), params :: Keyword.t()) ::
              :ok | {:error, Ash.error()}

  @doc """
  Refetches a record from the database
  """
  @callback reload(record :: Ash.record(), params :: Keyword.t()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  Refetches a record from the database, raising on error.

  See `reload/1`.
  """
  @callback reload!(record :: Ash.record(), params :: Keyword.t()) :: Ash.record() | no_return

  @doc """
  Refetches a record from the database
  """
  @callback reload(record :: Ash.record()) :: {:ok, Ash.record()} | {:error, Ash.error()}

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Ash.Api.Interface

      alias Ash.Api.Interface

      @impl true
      def get!(resource, id, params \\ []) do
        Interface.get!(__MODULE__, resource, id, params)
      end

      @impl true
      def get(resource, id, params \\ []) do
        case Interface.get(__MODULE__, resource, id, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def read!(query, opts \\ [])

      # sobelow_skip ["SQL.Query"]
      def read!(resource, opts) when is_atom(resource) do
        read!(query(resource), opts)
      end

      def read!(query, opts) do
        Interface.read!(__MODULE__, query, opts)
      end

      @impl true
      def read(query, opts \\ [])

      # sobelow_skip ["SQL.Query"]
      def read(resource, opts) when is_atom(resource) do
        read(query(resource), opts)
      end

      def read(query, opts) do
        case Interface.read(__MODULE__, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def side_load!(data, query, opts \\ []) do
        Interface.side_load!(__MODULE__, data, query, opts)
      end

      @impl true
      def side_load(data, query, opts \\ []) do
        case Interface.side_load(__MODULE__, data, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def create!(resource, params \\ []) do
        Interface.create!(__MODULE__, resource, params)
      end

      @impl true
      def create(resource, params \\ []) do
        case Interface.create(__MODULE__, resource, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def update!(record, params \\ []) do
        Interface.update!(__MODULE__, record, params)
      end

      @impl true
      def update(record, params \\ []) do
        case Interface.update(__MODULE__, record, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def destroy!(record, params \\ []) do
        Interface.destroy!(__MODULE__, record, params)
      end

      @impl true
      def destroy(record, params \\ []) do
        case Interface.destroy(__MODULE__, record, params) do
          :ok -> :ok
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def reload!(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.primary_key(resource)) |> Enum.to_list()
        get!(resource, id, params)
      end

      @impl true
      def reload(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.primary_key(resource)) |> Enum.to_list()
        get(resource, id, params)
      end

      def query(resource) do
        Ash.Query.new(__MODULE__, resource)
      end
    end
  end

  @doc false
  @spec get!(Ash.api(), Ash.resource(), term(), Keyword.t()) :: Ash.record() | no_return
  def get!(api, resource, id, opts \\ []) do
    opts = NimbleOptions.validate!(opts, @get_opts_schema)

    api
    |> get(resource, id, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec get(Ash.api(), Ash.resource(), term(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def get(api, resource, id, opts) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @get_opts_schema),
         {:resource, {:ok, resource}} <- {:resource, api.get_resource(resource)},
         {:pkey, primary_key} when primary_key != [] <- {:pkey, Ash.primary_key(resource)},
         {:ok, filter} <- get_filter(primary_key, id) do
      resource
      |> api.query()
      |> Ash.Query.filter(filter)
      |> Ash.Query.side_load(opts[:side_load] || [])
      |> api.read(Keyword.delete(opts, :side_load))
      |> case do
        {:ok, [single_result]} ->
          {:ok, single_result}

        {:ok, []} ->
          {:ok, nil}

        {:error, error} ->
          {:error, error}

        {:ok, results} when is_list(results) ->
          {:error, :too_many_results}
      end
    else
      {:error, error} ->
        {:error, error}

      {:resource, :error} ->
        {:error, NoSuchResource.exception(resource: resource)}

      {:pkey, _} ->
        {:error, "Resource has no primary key"}
    end
  end

  defp get_filter(primary_key, id) do
    case {primary_key, id} do
      {[field], [{field, value}]} ->
        {:ok, [{field, value}]}

      {[field], value} ->
        {:ok, [{field, value}]}

      {fields, value} ->
        if Keyword.keyword?(value) and Enum.sort(Keyword.keys(value)) == Enum.sort(fields) do
          {:ok, value}
        else
          {:error, "invalid primary key provided to `get/3`"}
        end
    end
  end

  @doc false
  @spec side_load!(
          Ash.api(),
          Ash.record() | list(Ash.record()),
          Ash.query() | list(atom | {atom, list()}),
          Keyword.t()
        ) ::
          list(Ash.record()) | Ash.record() | no_return
  def side_load!(api, data, query, opts \\ []) do
    opts = NimbleOptions.validate!(opts, @side_load_opts_schema)

    api
    |> side_load(data, query, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec side_load(Ash.api(), Ash.query(), Keyword.t()) ::
          {:ok, list(Ash.resource())} | {:error, Ash.error()}
  def side_load(api, data, query, opts \\ [])
  def side_load(_, [], _, _), do: {:ok, []}
  def side_load(_, nil, _, _), do: {:ok, nil}

  def side_load(api, data, query, opts) when not is_list(data) do
    api
    |> side_load(List.wrap(data), query, opts)
    |> case do
      {:ok, [data]} -> {:ok, data}
      {:error, error} -> {:error, error}
    end
  end

  def side_load(api, [%resource{} | _] = data, query, opts) do
    query =
      case query do
        %Ash.Query{} = query ->
          query

        keyword ->
          resource
          |> api.query()
          |> Ash.Query.side_load(keyword)
      end

    with %{valid?: true} <- query,
         {:ok, opts} <- NimbleOptions.validate(opts, @side_load_opts_schema) do
      SideLoad.side_load(data, query, opts)
    else
      {:error, error} ->
        {:error, error}

      %{errors: errors} ->
        {:error, errors}
    end
  end

  @doc false
  @spec read!(Ash.api(), Ash.query(), Keyword.t()) ::
          list(Ash.record()) | no_return
  def read!(api, query, opts \\ []) do
    opts = NimbleOptions.validate!(opts, @read_opts_schema)

    api
    |> read(query, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec read(Ash.api(), Ash.query(), Keyword.t()) ::
          {:ok, list(Ash.resource())} | {:error, Ash.error()}
  def read(_api, query, opts \\ []) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @read_opts_schema),
         {:ok, action} <- get_action(query.resource, opts, :read) do
      Read.run(query, action, opts)
    else
      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  @spec create!(Ash.api(), Ash.resource(), Keyword.t()) ::
          Ash.record() | {:error, Ash.error()}
  def create!(api, resource, opts) do
    opts = NimbleOptions.validate!(opts, @create_opts_schema)

    api
    |> create(resource, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec create(Ash.api(), Ash.resource(), Keyword.t()) ::
          {:ok, Ash.resource()} | {:error, Ash.error()}
  def create(api, resource, opts) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @create_opts_schema),
         {:ok, resource} <- api.get_resource(resource),
         {:ok, action} <- get_action(resource, opts, :create) do
      Create.run(api, resource, action, opts)
    end
  end

  @doc false
  @spec update!(Ash.api(), Ash.record(), Keyword.t()) :: Ash.resource() | no_return()
  def update!(api, record, opts) do
    opts = NimbleOptions.validate!(opts, @update_opts_schema)

    api
    |> update(record, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec update(Ash.api(), Ash.record(), Keyword.t()) ::
          {:ok, Ash.resource()} | {:error, Ash.error()}
  def update(api, %resource{} = record, opts) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @update_opts_schema),
         {:ok, resource} <- api.get_resource(resource),
         {:ok, action} <- get_action(resource, opts, :update) do
      Update.run(api, record, action, opts)
    end
  end

  @doc false
  @spec destroy!(Ash.api(), Ash.record(), Keyword.t()) :: :ok | no_return
  def destroy!(api, record, opts) do
    opts = NimbleOptions.validate!(opts, @destroy_opts_schema)

    api
    |> destroy(record, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec destroy(Ash.api(), Ash.record(), Keyword.t()) :: :ok | {:error, Ash.error()}
  def destroy(api, %resource{} = record, opts) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @destroy_opts_schema),
         {:ok, resource} <- api.get_resource(resource),
         {:ok, action} <- get_action(resource, opts, :destroy) do
      Destroy.run(api, record, action, opts)
    end
  end

  defp get_action(resource, params, type) do
    case params[:action] || Ash.primary_action(resource, type) do
      nil ->
        {:error, "no action provided, and no primary action found for #{to_string(type)}"}

      action ->
        {:ok, action}
    end
  end

  defp unwrap_or_raise!(:ok), do: :ok
  defp unwrap_or_raise!({:ok, result}), do: result

  defp unwrap_or_raise!({:error, error}) do
    exception = Ash.Error.to_ash_error(error)
    raise exception
  end
end
