defmodule Ash.Api do
  @moduledoc """
  An Api allows you to interact with your resources, and holds non-resource-specific configuration.

  Your Api can also house config that is not resource specific. For example, the json api extension
  adds an api extension that lets you toggle authorization on/off for all resources in that Api.
  You include them in an Api like so:

  ```elixir
  defmodule MyApp.Api do
    use Ash.Api

    resources do
      resource OneResource
      resource SecondResource
    end
  end
  ```

  Then you can interact through that Api with the actions that those resources expose.
  For example: `MyApp.Api.create(changeset)`, or `MyApp.Api.read(query)`. Corresponding
  actions must be defined in your resources in order to call them through the Api.
  """

  import Ash.OptionsHelpers, only: [merge_schemas: 3]

  alias Ash.Actions.{Create, Destroy, Read, Update}
  alias Ash.Error.Invalid.{InvalidPrimaryKey, NoSuchAction, NoSuchResource}

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

  @doc false
  def read_opts_schema, do: @read_opts_schema

  @load_opts_schema merge_schemas([], @global_opts, "Global Options")

  @get_opts_schema [
                     load: [
                       type: :any,
                       doc: "Fields or relationships to load in the query. See `Ash.Query.load/2`"
                     ]
                   ]
                   |> merge_schemas(@global_opts, "Global Options")

  @shared_create_and_update_opts_schema []
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
  Get a record by a primary key. See `c:get/3` for more.

  #{NimbleOptions.docs(@get_opts_schema)}
  """
  @callback get!(resource :: Ash.resource(), id_or_filter :: term(), params :: Keyword.t()) ::
              Ash.record() | no_return

  @doc """
  Get a record by a primary key.

  For a resource with a composite primary key, pass a keyword list, e.g
  `MyApi.get(MyResource, first_key: 1, second_key: 2)`

  #{NimbleOptions.docs(@get_opts_schema)}
  """
  @callback get(resource :: Ash.resource(), id_or_filter :: term(), params :: Keyword.t()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  Run an ash query. See `c:read/2` for more.

  #{NimbleOptions.docs(@read_opts_schema)}
  """
  @callback read!(Ash.query(), params :: Keyword.t()) ::
              list(Ash.resource()) | no_return

  @doc """
  Run a query on a resource.

  For more information, on building a query, see `Ash.Query`.

  #{NimbleOptions.docs(@read_opts_schema)}
  """
  @callback read(Ash.query(), params :: Keyword.t()) ::
              {:ok, list(Ash.resource())} | {:error, Ash.error()}

  @doc """
  Load fields or relationships on already fetched records. See `c:load/2` for more information.

  #{NimbleOptions.docs(@load_opts_schema)}
  """
  @callback load!(
              record_or_records :: Ash.record() | [Ash.record()],
              params :: Keyword.t() | Ash.query()
            ) ::
              Ash.record() | [Ash.record()] | no_return

  @doc """
  Load fields or relationships on already fetched records.

  Accepts a list of non-loaded fields and loads them on the provided records or a query, in
  which case the loaded fields of the query are used. Relationship loads can be nested, for
  example: `MyApi.load(record, [posts: [:comments]])`. See `Ash.Query.side_load/2` for more
  information on specifically loading relationships.

  #{NimbleOptions.docs(@load_opts_schema)}
  """
  @callback load(
              record_or_records :: Ash.record() | [Ash.record()],
              params :: Keyword.t() | Ash.query()
            ) ::
              {:ok, Ash.record() | [Ash.record()]} | {:error, Ash.error()}

  @doc """
  Create a record. See `c:create/2` for more information.

  #{NimbleOptions.docs(@create_opts_schema)}
  """
  @callback create!(Ash.changeset(), params :: Keyword.t()) ::
              Ash.record() | no_return

  @doc """
  Create a record.

  #{NimbleOptions.docs(@create_opts_schema)}
  """
  @callback create(Ash.changeset(), params :: Keyword.t()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  Update a record. See `c:update/2` for more information.

  #{NimbleOptions.docs(@update_opts_schema)}
  """
  @callback update!(Ash.changeset(), params :: Keyword.t()) ::
              Ash.record() | no_return

  @doc """
  Update a record.

  #{NimbleOptions.docs(@update_opts_schema)}
  """
  @callback update(Ash.changeset(), params :: Keyword.t()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  Destroy a record. See `c:destroy/2` for more information.

  #{NimbleOptions.docs(@destroy_opts_schema)}
  """
  @callback destroy!(Ash.changeset() | Ash.record(), params :: Keyword.t()) :: :ok | no_return

  @doc """
  Destroy a record.

  #{NimbleOptions.docs(@destroy_opts_schema)}
  """
  @callback destroy(Ash.changeset() | Ash.record(), params :: Keyword.t()) ::
              :ok | {:error, Ash.error()}

  @doc """
  Refetches a record by primary key. See `c:reload/1` for more.
  """
  @callback reload!(record :: Ash.record(), params :: Keyword.t()) :: Ash.record() | no_return

  @doc """
  Refetches a record by primary key.
  """
  @callback reload(record :: Ash.record()) :: {:ok, Ash.record()} | {:error, Ash.error()}

  alias Ash.Dsl.Extension

  defmacro __using__(opts) do
    extensions = [Ash.Api.Dsl | opts[:extensions] || []]

    body =
      quote do
        @before_compile Ash.Api
        @behaviour Ash.Api
      end

    preparations = Extension.prepare(extensions)

    [body | preparations]
  end

  defmacro __before_compile__(_env) do
    quote generated: true do
      alias Ash.Dsl.Extension

      @on_load :on_load

      @ash_dsl_config Extension.set_state()

      def on_load do
        Extension.load()
      end

      use Ash.Api.Interface
    end
  end

  alias Ash.Dsl.Extension

  def resource(api, resource) do
    api
    |> resources()
    |> Enum.find(&(&1 == resource))
    |> case do
      nil -> {:error, NoSuchResource.exception(resource: resource)}
      resource -> {:ok, resource}
    end
  end

  @spec resources(Ash.api()) :: [Ash.resource()]
  def resources(api) do
    api
    |> Extension.get_entities([:resources])
    |> Enum.map(& &1.resource)
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
          {:ok, Ash.record() | nil} | {:error, Ash.error()}
  def get(api, resource, id, opts) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @get_opts_schema),
         {:ok, resource} <- Ash.Api.resource(api, resource),
         primary_key <- Ash.Resource.primary_key(resource),
         {:ok, filter} <- get_filter(resource, primary_key, id) do
      resource
      |> Ash.Query.new(api)
      |> Ash.Query.filter(filter)
      |> Ash.Query.load(opts[:load] || [])
      |> api.read(Keyword.delete(opts, :load))
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
    end
  end

  defp get_filter(resource, primary_key, id) do
    case {primary_key, id} do
      {[field], [{field, value}]} ->
        {:ok, [{field, value}]}

      {[field], value} ->
        {:ok, [{field, value}]}

      {fields, value} ->
        if Keyword.keyword?(value) and Enum.sort(Keyword.keys(value)) == Enum.sort(fields) do
          {:ok, value}
        else
          {:error, InvalidPrimaryKey.exception(resource: resource, value: id)}
        end
    end
  end

  @doc false
  @spec load!(
          Ash.api(),
          Ash.record() | list(Ash.record()),
          Ash.query() | list(atom | {atom, list()}),
          Keyword.t()
        ) ::
          list(Ash.record()) | Ash.record() | no_return
  def load!(api, data, query, opts \\ []) do
    opts = NimbleOptions.validate!(opts, @load_opts_schema)

    api
    |> load(data, query, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec load(
          Ash.api(),
          Ash.record() | list(Ash.record()),
          Ash.query() | list(atom | {atom, list()}),
          Keyword.t()
        ) ::
          {:ok, list(Ash.record()) | Ash.record()} | {:error, Ash.error()}
  def load(api, data, query, opts \\ [])
  def load(_, [], _, _), do: {:ok, []}
  def load(_, nil, _, _), do: {:ok, nil}
  def load(_, {:error, error}, _, _), do: {:error, error}

  def load(api, {:ok, values}, query, opts) do
    load(api, values, query, opts)
  end

  def load(api, data, query, opts) when not is_list(data) do
    api
    |> load(List.wrap(data), query, opts)
    |> case do
      {:ok, [data]} -> {:ok, data}
      {:error, error} -> {:error, error}
    end
  end

  def load(api, [%resource{} | _] = data, query, opts) do
    query =
      case query do
        %Ash.Query{} = query ->
          query

        keyword ->
          resource
          |> Ash.Query.new(api)
          |> Ash.Query.load(keyword)
      end

    with %{valid?: true} <- query,
         {:ok, action} <- get_action(query.resource, opts, :read),
         {:ok, opts} <- NimbleOptions.validate(opts, @load_opts_schema) do
      Read.run(query, action, Keyword.put(opts, :initial_data, data))
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
  def read(api, query, opts \\ []) do
    query = Ash.Query.set_api(query, api)

    with {:ok, opts} <- NimbleOptions.validate(opts, @read_opts_schema),
         {:ok, action} <- get_action(query.resource, opts, :read) do
      Read.run(query, action, opts)
    else
      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  @spec create!(Ash.api(), Ash.changeset(), Keyword.t()) ::
          Ash.record() | no_return
  def create!(api, changeset, opts) do
    opts = NimbleOptions.validate!(opts, @create_opts_schema)

    api
    |> create(changeset, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec create(Ash.api(), Ash.changeset(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def create(api, changeset, opts) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @create_opts_schema),
         {:ok, resource} <- Ash.Api.resource(api, changeset.resource),
         {:ok, action} <- get_action(resource, opts, :create) do
      Create.run(api, changeset, action, opts)
    end
  end

  @doc false
  def update!(api, changeset, opts) do
    opts = NimbleOptions.validate!(opts, @update_opts_schema)

    api
    |> update(changeset, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec update(Ash.api(), Ash.record(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def update(api, changeset, opts) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @update_opts_schema),
         {:ok, resource} <- Ash.Api.resource(api, changeset.resource),
         {:ok, action} <- get_action(resource, opts, :update) do
      Update.run(api, changeset, action, opts)
    end
  end

  @doc false
  @spec destroy!(Ash.api(), Ash.changeset() | Ash.record(), Keyword.t()) :: :ok | no_return
  def destroy!(api, changeset, opts) do
    opts = NimbleOptions.validate!(opts, @destroy_opts_schema)

    api
    |> destroy(changeset, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec destroy(Ash.api(), Ash.changeset() | Ash.record(), Keyword.t()) ::
          :ok | {:error, Ash.error()}
  def destroy(api, %Ash.Changeset{resource: resource} = changeset, opts) do
    with {:ok, opts} <- NimbleOptions.validate(opts, @destroy_opts_schema),
         {:ok, resource} <- Ash.Api.resource(api, resource),
         {:ok, action} <- get_action(resource, opts, :destroy) do
      Destroy.run(api, changeset, action, opts)
    end
  end

  def destroy(api, record, opts) do
    destroy(api, Ash.Changeset.new(record), opts)
  end

  defp get_action(resource, params, type) do
    case Keyword.fetch(params, :action) do
      {:ok, %_{} = action} ->
        {:ok, action}

      {:ok, action} ->
        case Ash.Resource.action(resource, action, type) do
          nil ->
            {:error, NoSuchAction.exception(resource: resource, action: action, type: type)}

          action ->
            {:ok, action}
        end

      :error ->
        case Ash.Resource.primary_action(resource, type) do
          nil ->
            {:error,
             "no action provided, and no primary #{to_string(type)} action found for resource #{
               inspect(resource)
             }"}

          action ->
            {:ok, action}
        end
    end
  end

  defp unwrap_or_raise!(:ok), do: :ok
  defp unwrap_or_raise!({:ok, result}), do: result

  defp unwrap_or_raise!({:error, error}) do
    exception = Ash.Error.to_ash_error(error)
    raise exception
  end
end
