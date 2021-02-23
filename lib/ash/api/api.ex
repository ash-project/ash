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

  alias Ash.Error.Invalid.{
    NoPrimaryAction,
    NoSuchAction,
    NoSuchResource
  }

  require Ash.Query

  @type t() :: module

  @type page_request ::
          :next | :prev | :first | :last | integer

  @global_opts [
    verbose?: [
      type: :boolean,
      default: false,
      doc: "Log engine operations (very verbose!)"
    ],
    action: [
      type: :any,
      doc: "The action to use, either an Action struct or the name of the action"
    ],
    authorize?: [
      type: {:in, [true, false, nil]},
      doc:
        "If an actor is provided, authorization happens automatically. If not, this flag can be used to authorize with no user."
    ],
    stacktraces?: [
      type: :boolean,
      default: false,
      doc:
        "For Ash errors, can be set to true to get a stacktrace for each error that occured. See the error_handling guide for more."
    ],
    actor: [
      type: :any,
      doc:
        "If an actor is provided, it will be used in conjunction with the authorizers of a resource to authorize access"
    ]
  ]

  @read_opts_schema merge_schemas(
                      [
                        page: [
                          doc:
                            "Nested pagination options, see the section on pagination for more",
                          type: {:custom, __MODULE__, :page_opts, []}
                        ],
                        return_query?: [
                          type: :boolean,
                          doc: """
                          If `true`, the query that was ultimately used is returned as a third tuple element.

                          The query goes through many potential changes during a request, potentially adding
                          authorization filters, or replacing relationships for other data layers with their
                          corresponding ids. This option can be used to get the true query that was sent to
                          the data layer.
                          """,
                          default: false
                        ]
                      ],
                      @global_opts,
                      "Global Options"
                    )

  @offset_page_opts [
    offset: [
      type: :non_neg_integer,
      doc: "The number of records to skip from the beginning of the query"
    ],
    limit: [
      type: :pos_integer,
      doc: "The number of records to include in the page"
    ],
    filter: [
      type: :any,
      doc: """
      A filter to apply for pagination purposes, that should not be considered in the full count.

      This is used by the liveview paginator to only fetch the records that were *already* on the
      page when refreshing data, to avoid pages jittering.
      """
    ],
    count: [
      type: :boolean,
      doc: "Whether or not to return the page with a full count of all records"
    ]
  ]

  @keyset_page_opts [
    before: [
      type: :string,
      doc: "Get records that appear before the provided keyset (mutually exclusive with `after`)"
    ],
    after: [
      type: :string,
      doc: "Get records that appear after the provided keyset (mutually exclusive with `before`)"
    ],
    limit: [
      type: :pos_integer,
      doc: "How many records to include in the page"
    ],
    filter: [
      type: :any,
      doc: "See the `filter` option for offset pagination, this behaves the same."
    ],
    count: [
      type: :boolean,
      doc: "Whether or not to return the page with a full count of all records"
    ]
  ]

  @doc false
  def page_opts(page_opts) do
    if page_opts == false do
      {:ok, false}
    else
      if page_opts[:after] || page_opts[:before] do
        validate_or_error(page_opts, @keyset_page_opts)
      else
        if page_opts[:offset] do
          validate_or_error(page_opts, @offset_page_opts)
        else
          validate_or_error(page_opts, @keyset_page_opts)
        end
      end
    end
  end

  defp validate_or_error(opts, schema) do
    case Ash.OptionsHelpers.validate(opts, schema) do
      {:ok, value} -> {:ok, value}
      {:error, error} -> {:error, Exception.message(error)}
    end
  end

  @load_opts_schema merge_schemas([], @global_opts, "Global Options")

  @get_opts_schema [
                     load: [
                       type: :any,
                       doc: "Fields or relationships to load in the query. See `Ash.Query.load/2`"
                     ],
                     tenant: [
                       type: :any,
                       doc: "The tenant to set on the query being run"
                     ],
                     action: [
                       type: :atom,
                       doc: "The action to use for reading the data"
                     ],
                     context: [
                       type: :any,
                       doc: "Context to be set on the query being run"
                     ]
                   ]
                   |> merge_schemas(@global_opts, "Global Options")

  @shared_created_update_and_destroy_opts_schema [
    return_notifications?: [
      type: :boolean,
      default: false,
      doc: """
      Use this if you're running ash actions in your own transaction and you want notifications to happen still.

      If a transaction is ongoing, and this is false, notifications will be discarded, otherwise
      the return value is `{:ok, result, notifications}` (or `{:ok, notifications}`)

      To send notifications later, use `Ash.Notifier.notify(notifications)`. It sends any notifications
      that can be sent, and returns the rest.
      """
    ]
  ]

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
                        @shared_created_update_and_destroy_opts_schema,
                        "Shared create/update/destroy Options"
                      )

  @update_opts_schema []
                      |> merge_schemas(@global_opts, "Global Options")
                      |> merge_schemas(
                        @shared_created_update_and_destroy_opts_schema,
                        "Shared create/update/destroy Options"
                      )

  @destroy_opts_schema []
                       |> merge_schemas(@global_opts, "Global Opts")
                       |> merge_schemas(
                         @shared_created_update_and_destroy_opts_schema,
                         "Shared create/update/destroy Options"
                       )

  @doc """
  Get a record by a primary key. See `c:get/3` for more.
  """
  @callback get!(
              resource :: Ash.Resource.t(),
              id_or_filter :: term(),
              params :: Keyword.t()
            ) ::
              Ash.Resource.record() | no_return

  @doc """
  Get a record by a primary key.

  For a resource with a composite primary key, pass a keyword list, e.g
  `MyApi.get(MyResource, first_key: 1, second_key: 2)`

  #{Ash.OptionsHelpers.docs(@get_opts_schema)}
  """
  @callback get(
              resource :: Ash.Resource.t(),
              id_or_filter :: term(),
              params :: Keyword.t()
            ) ::
              {:ok, Ash.Resource.record()} | {:error, term}

  @doc """
  Run an ash query, raising on more than one result. See `c:read_one/2` for more.
  """
  @callback read_one!(Ash.Query.t() | Ash.Resource.t(), params :: Keyword.t()) ::
              Ash.Resource.record() | {Ash.Resource.record(), Ash.Query.t()} | no_return

  @doc """
  Run a query on a resource, but fail on more than one result

  This is useful if you have a query that doesn't include a primary key
  but you know that it will only ever return a single result
  """
  @callback read_one(Ash.Query.t() | Ash.Resource.t(), params :: Keyword.t()) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), Ash.Query.t()}
              | {:error, term}
  @doc """
  Run an ash query. See `c:read/2` for more.
  """
  @callback read!(Ash.Query.t() | Ash.Resource.t(), params :: Keyword.t()) ::
              list(Ash.Resource.record())
              | {list(Ash.Resource.record()), Ash.Query.t()}
              | no_return

  @doc """
  Run a query on a resource.

  For more information, on building a query, see `Ash.Query`.

  #{Ash.OptionsHelpers.docs(@read_opts_schema)}

  ## Pagination

  #### Limit/offset pagination
  #{Ash.OptionsHelpers.docs(@offset_page_opts)}

  #### Keyset pagination
  #{Ash.OptionsHelpers.docs(@keyset_page_opts)}
  """
  @callback read(Ash.Query.t(), params :: Keyword.t()) ::
              {:ok, list(Ash.Resource.record())}
              | {:ok, list(Ash.Resource.record()), Ash.Query.t()}
              | {:error, term}

  @doc """
  Fetch a page relative to the provided page.
  """
  @callback page!(Ash.Page.page(), page_request) ::
              Ash.Page.page() | no_return

  @doc """
  Fetch a page relative to the provided page.

  A page is the return value of a paginated action called via `c:read/2`.
  """
  @callback page(Ash.Page.page(), page_request) ::
              {:ok, Ash.Page.page()} | {:error, term}

  @doc """
  Load fields or relationships on already fetched records. See `c:load/2` for more information.
  """
  @callback load!(
              record_or_records :: Ash.Resource.record() | [Ash.Resource.record()],
              query :: Ash.Query.t(),
              opts :: Keyword.t()
            ) ::
              Ash.Resource.record() | [Ash.Resource.record()] | no_return

  @doc """
  Load fields or relationships on already fetched records.

  Accepts a list of non-loaded fields and loads them on the provided records or a query, in
  which case the loaded fields of the query are used. Relationship loads can be nested, for
  example: `MyApi.load(record, [posts: [:comments]])`.

  #{Ash.OptionsHelpers.docs(@load_opts_schema)}
  """
  @callback load(
              record_or_records :: Ash.Resource.record() | [Ash.Resource.record()],
              query :: Ash.Query.t(),
              opts :: Keyword.t()
            ) ::
              {:ok, Ash.Resource.record() | [Ash.Resource.record()]} | {:error, term}

  @doc """
  Create a record. See `c:create/2` for more information.
  """
  @callback create!(Ash.Changeset.t(), params :: Keyword.t()) ::
              Ash.Resource.record() | no_return

  @doc """
  Create a record.

  #{Ash.OptionsHelpers.docs(@create_opts_schema)}
  """
  @callback create(Ash.Changeset.t(), params :: Keyword.t()) ::
              {:ok, Ash.Resource.record()} | {:error, term}

  @doc """
  Update a record. See `c:update/2` for more information.
  """
  @callback update!(Ash.Changeset.t(), params :: Keyword.t()) ::
              Ash.Resource.record() | no_return

  @doc """
  Update a record.

  #{Ash.OptionsHelpers.docs(@update_opts_schema)}
  """
  @callback update(Ash.Changeset.t(), params :: Keyword.t()) ::
              {:ok, Ash.Resource.record()} | {:error, term}

  @doc """
  Destroy a record. See `c:destroy/2` for more information.
  """
  @callback destroy!(Ash.Changeset.t() | Ash.Resource.record(), params :: Keyword.t()) ::
              :ok | no_return

  @doc """
  Destroy a record.

  #{Ash.OptionsHelpers.docs(@destroy_opts_schema)}
  """
  @callback destroy(Ash.Changeset.t() | Ash.Resource.record(), params :: Keyword.t()) ::
              :ok | {:error, term}

  @doc """
  Refetches a record by primary key. See `c:reload/1` for more.
  """
  @callback reload!(record :: Ash.Resource.record(), params :: Keyword.t()) ::
              Ash.Resource.record() | no_return

  @doc """
  Refetches a record by primary key.
  """
  @callback reload(record :: Ash.Resource.record()) ::
              {:ok, Ash.Resource.record()} | {:error, term}

  alias Ash.Dsl.Extension

  use Ash.Dsl, default_extensions: [extensions: [Ash.Api.Dsl]]

  def handle_opts(_) do
    quote do
      @behaviour Ash.Api
    end
  end

  def handle_before_compile(_) do
    quote do
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

  @spec resources(Ash.Api.t()) :: [Ash.Resource.t()]
  def resources(api) do
    api
    |> Extension.get_entities([:resources])
    |> Enum.map(& &1.resource)
  end

  @doc false
  @spec get!(Ash.Api.t(), Ash.Resource.t(), term(), Keyword.t()) ::
          Ash.Resource.record() | no_return
  def get!(api, resource, id, opts \\ []) do
    opts = Ash.OptionsHelpers.validate!(opts, @get_opts_schema)

    api
    |> get(resource, id, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec get(Ash.Api.t(), Ash.Resource.t(), term(), Keyword.t()) ::
          {:ok, Ash.Resource.record() | nil} | {:error, term}
  def get(api, resource, id, opts) do
    with {:ok, opts} <- Ash.OptionsHelpers.validate(opts, @get_opts_schema),
         {:ok, resource} <- Ash.Api.resource(api, resource),
         {:ok, filter} <- Ash.Filter.get_filter(resource, id) do
      query =
        resource
        |> Ash.Query.new(api)
        |> Ash.Query.set_tenant(opts[:tenant])
        |> Ash.Query.filter(^filter)
        |> Ash.Query.load(opts[:load] || [])
        |> Ash.Query.set_context(opts[:context] || %{})

      query
      |> api.read(Keyword.delete(opts, :load))
      |> case do
        {:ok, %{results: [single_result]}} ->
          {:ok, single_result}

        {:ok, %{results: []}} ->
          {:ok, nil}

        {:ok, %{results: results}} ->
          {:error,
           Ash.Error.Invalid.MultipleResults.exception(
             count: Enum.count(results),
             query: query,
             at_least?: true
           )}

        {:ok, [single_result]} ->
          {:ok, single_result}

        {:ok, []} ->
          {:ok, nil}

        {:error, error} ->
          {:error, error}

        {:ok, results} when is_list(results) ->
          {:error,
           Ash.Error.Invalid.MultipleResults.exception(count: Enum.count(results), query: query)}
      end
    else
      {:error, error} ->
        {:error, error}
    end
  end

  def page!(api, keyset, request) do
    {_, opts} = keyset.rerun

    api
    |> page(keyset, request)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  def page(_, %Ash.Page.Keyset{results: []} = page, :next) do
    {:ok, page}
  end

  def page(_, %Ash.Page.Keyset{results: []} = page, :prev) do
    {:ok, page}
  end

  def page(_, %Ash.Page.Keyset{}, n) when is_integer(n) do
    {:error, "Cannot seek to a specific page with keyset based pagination"}
  end

  def page(
        api,
        %Ash.Page.Keyset{results: results, rerun: {query, opts}},
        :next
      ) do
    last_keyset =
      results
      |> :lists.last()
      |> Map.get(:__metadata__)
      |> Map.get(:keyset)

    new_page_opts =
      opts[:page]
      |> Keyword.delete(:before)
      |> Keyword.put(:after, last_keyset)

    read(api, query, Keyword.put(opts, :page, new_page_opts))
  end

  def page(api, %Ash.Page.Keyset{results: results, rerun: {query, opts}}, :prev) do
    first_keyset =
      results
      |> List.first()
      |> Map.get(:__metadata__)
      |> Map.get(:keyset)

    new_page_opts =
      opts[:page]
      |> Keyword.put(:before, first_keyset)
      |> Keyword.delete(:after)

    read(api, query, Keyword.put(opts, :page, new_page_opts))
  end

  def page(api, %Ash.Page.Keyset{rerun: {query, opts}}, :first) do
    page_opts =
      if opts[:page][:count] do
        [count: true]
      else
        []
      end

    read(api, query, Keyword.put(opts, :page, page_opts))
  end

  def page(
        api,
        %Ash.Page.Offset{count: count, limit: limit, offset: offset, rerun: {query, opts}},
        request
      ) do
    page_opts =
      case request do
        :next ->
          [offset: offset + limit, limit: limit]

        :prev ->
          [offset: max(offset - limit, 0), limit: limit]

        :first ->
          [offset: 0, limit: limit]

        :last ->
          if count do
            [offset: count - limit, limit: limit]
          else
            [offset: 0, limit: limit]
          end

        page_num when is_integer(page_num) ->
          [offset: (page_num - 1) * limit, limit: limit]
      end

    page_opts =
      if opts[:page][:count] do
        Keyword.put(page_opts, :count, true)
      else
        page_opts
      end

    if request == :last && !count do
      {:error, "Cannot fetch last page without counting"}
    else
      read(api, query, Keyword.put(opts, :page, page_opts))
    end
  end

  @doc false
  @spec load!(
          Ash.Api.t(),
          Ash.Resource.record() | list(Ash.Resource.record()),
          Ash.Query.t() | list(atom | {atom, list()}),
          Keyword.t()
        ) ::
          list(Ash.Resource.record()) | Ash.Resource.record() | no_return
  def load!(api, data, query, opts \\ []) do
    opts = Ash.OptionsHelpers.validate!(opts, @load_opts_schema)

    api
    |> load(data, query, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec load(
          Ash.Api.t(),
          Ash.Resource.record() | list(Ash.Resource.record()),
          Ash.Query.t() | list(atom | {atom, list()}),
          Keyword.t()
        ) ::
          {:ok, list(Ash.Resource.record()) | Ash.Resource.record()} | {:error, term}
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
      {:ok, %{results: [data]}} -> {:ok, data}
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
         {:ok, action} <- get_action(query.resource, opts, :read, query.action),
         {:ok, opts} <- Ash.OptionsHelpers.validate(opts, @load_opts_schema) do
      Read.run(query, action, Keyword.put(opts, :initial_data, data))
    else
      {:error, error} ->
        {:error, error}

      %{errors: errors} ->
        {:error, errors}
    end
  end

  @doc false
  @spec read!(Ash.Api.t(), Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          list(Ash.Resource.record()) | no_return
  def read!(api, query, opts \\ []) do
    opts = Ash.OptionsHelpers.validate!(opts, @read_opts_schema)

    api
    |> read(query, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec read(Ash.Api.t(), Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          {:ok, list(Ash.Resource.record()) | Ash.Page.page()} | {:error, term}
  def read(api, query, opts \\ [])

  def read(api, resource, opts) when is_atom(resource) do
    read(api, Ash.Query.new(resource, api), opts)
  end

  def read(api, query, opts) do
    query = Ash.Query.set_api(query, api)

    with {:ok, opts} <- Ash.OptionsHelpers.validate(opts, @read_opts_schema),
         {:ok, action} <- get_action(query.resource, opts, :read, query.action) do
      Read.run(query, action, opts)
    else
      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  def read_one!(api, query, opts) do
    api
    |> read_one(query, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  def read_one(api, query, opts) do
    api
    |> read(query, opts)
    |> unwrap_one()
  end

  defp unwrap_one({:error, error}) do
    {:error, error}
  end

  defp unwrap_one({:ok, result, query}) do
    case unwrap_one({:ok, result}) do
      {:ok, result} ->
        {:ok, result, query}

      {:error, %Ash.Error.Invalid.MultipleResults{} = error} ->
        {:error, %{error | query: query}}

      {:error, error} ->
        {:error, error}
    end
  end

  defp unwrap_one({:ok, result}) do
    case unwrap_one(result) do
      {:ok, result} ->
        {:ok, result}

      {:error, error} ->
        {:error, error}
    end
  end

  defp unwrap_one(%{results: results}) do
    unwrap_one(results)
  end

  defp unwrap_one([]), do: {:ok, nil}
  defp unwrap_one([result]), do: {:ok, result}

  defp unwrap_one([_ | _] = results) do
    error =
      Ash.Error.Invalid.MultipleResults.exception(
        count: Enum.count(results),
        at_least?: true
      )

    {:error, error}
  end

  @doc false
  @spec create!(Ash.Api.t(), Ash.Changeset.t(), Keyword.t()) ::
          Ash.Resource.record() | no_return
  def create!(api, changeset, opts) do
    opts = Ash.OptionsHelpers.validate!(opts, @create_opts_schema)

    api
    |> create(changeset, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec create(Ash.Api.t(), Ash.Changeset.t(), Keyword.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def create(api, changeset, opts) do
    with {:ok, opts} <- Ash.OptionsHelpers.validate(opts, @create_opts_schema),
         {:ok, resource} <- Ash.Api.resource(api, changeset.resource),
         {:ok, action} <- get_action(resource, opts, :create, changeset.action) do
      Create.run(api, changeset, action, opts)
    end
  end

  @doc false
  def update!(api, changeset, opts) do
    opts = Ash.OptionsHelpers.validate!(opts, @update_opts_schema)

    api
    |> update(changeset, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec update(Ash.Api.t(), Ash.Resource.record(), Keyword.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def update(api, changeset, opts) do
    with {:ok, opts} <- Ash.OptionsHelpers.validate(opts, @update_opts_schema),
         {:ok, resource} <- Ash.Api.resource(api, changeset.resource),
         {:ok, action} <- get_action(resource, opts, :update, changeset.action) do
      Update.run(api, changeset, action, opts)
    end
  end

  @doc false
  @spec destroy!(Ash.Api.t(), Ash.Changeset.t() | Ash.Resource.record(), Keyword.t()) ::
          :ok | no_return
  def destroy!(api, changeset, opts) do
    opts = Ash.OptionsHelpers.validate!(opts, @destroy_opts_schema)

    api
    |> destroy(changeset, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec destroy(Ash.Api.t(), Ash.Changeset.t() | Ash.Resource.record(), Keyword.t()) ::
          :ok | {:error, term}
  def destroy(api, %Ash.Changeset{resource: resource} = changeset, opts) do
    with {:ok, opts} <- Ash.OptionsHelpers.validate(opts, @destroy_opts_schema),
         {:ok, resource} <- Ash.Api.resource(api, resource),
         {:ok, action} <- get_action(resource, opts, :destroy, changeset.action) do
      Destroy.run(api, changeset, action, opts)
    end
  end

  def destroy(api, record, opts) do
    destroy(api, Ash.Changeset.new(record), opts)
  end

  defp get_action(resource, params, type, preset \\ nil) do
    case Keyword.fetch(params, :action) do
      {:ok, %_{} = action} ->
        {:ok, action}

      {:ok, nil} ->
        if preset do
          get_action(resource, Keyword.put(params, :action, preset), type)
        else
          get_action(resource, Keyword.delete(params, :action), type)
        end

      {:ok, action} ->
        case Ash.Resource.Info.action(resource, action, type) do
          nil ->
            {:error, NoSuchAction.exception(resource: resource, action: action, type: type)}

          action ->
            {:ok, action}
        end

      :error ->
        if preset do
          get_action(resource, Keyword.put(params, :action, preset), type)
        else
          case Ash.Resource.Info.primary_action(resource, type) do
            nil ->
              {:error, NoPrimaryAction.exception(resource: resource, type: type)}

            action ->
              {:ok, action}
          end
        end
    end
  end

  defp unwrap_or_raise!(:ok, _), do: :ok
  defp unwrap_or_raise!({:ok, result}, _), do: result
  defp unwrap_or_raise!({:ok, result, other}, _), do: {result, other}

  defp unwrap_or_raise!({:error, error}, stacktraces?) do
    exception = Ash.Error.to_ash_error(error)

    exception =
      if stacktraces? do
        exception
      else
        Ash.Error.clear_stacktraces(exception)
      end

    case exception do
      %{stacktraces?: _} ->
        if stacktraces? do
          reraise %{exception | stacktraces?: stacktraces?},
                  Map.get(exception.stacktrace || %{}, :stacktrace)
        else
          raise %{exception | stacktraces?: stacktraces?}
        end

      _ ->
        raise exception
    end
  end
end
