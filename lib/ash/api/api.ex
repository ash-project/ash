defmodule Ash.Api do
  @moduledoc """
  An Api allows you to interact with your resources, and holds non-resource-specific configuration.

  For example, the json api extension adds an api extension that lets you toggle authorization on/off
  for all resources in that Api. You include them in an Api like so:

  ```elixir
  defmodule MyApp.Registry do
    use Ash.Registry,
      extensions: [Ash.Registry.ResourceValidations]

    entries do
      entry OneResource
      entry SecondResource
    end
  end

  defmodule MyApp.Api do
    use Ash.Api

    resources do
      registry MyApp.Registry
    end
  end
  ```

  Then you can interact through that Api with the actions that those resources expose.
  For example: `MyApp.Api.create(changeset)`, or `MyApp.Api.read(query)`. Corresponding
  actions must be defined in your resources in order to call them through the Api.

  ## Interface

  The functions documented here can be used to call any action on any resource in the Api.
  For example, `MyApi.read(Myresource, [...])`.

  Additionally, you can define a `code_interface` on each resource. See the code interface guide for more.
  """

  use Spark.Dsl, default_extensions: [extensions: [Ash.Api.Dsl]]

  import Spark.OptionsHelpers, only: [merge_schemas: 3]

  alias Ash.Actions.{Create, Destroy, Read, Update}

  alias Ash.Error.Invalid.{
    NoPrimaryAction,
    NoSuchAction,
    NoSuchResource,
    PageRequiresPagination
  }

  alias Ash.Error.Query.NotFound

  require Ash.Query

  @dialyzer {:nowarn_function, unwrap_or_raise!: 3}

  @type t() :: module

  @type page_request ::
          :next | :prev | :first | :last | integer

  @global_opts [
    internal?: [
      type: :boolean,
      hide: true
    ],
    timeout: [
      type: :timeout,
      doc: """
      A positive integer, or `:infinity`. If none is provided, the timeout configured on the api is used (which defaults to `30_000`).
      """
    ],
    tracer: [
      type: :atom,
      doc: """
      A tracer that implements the `Ash.Tracer` behaviour. See that module for more.
      """
    ],
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
        "If an actor option is provided (even if it is `nil`), authorization happens automatically. If not, this flag can be used to authorize with no user."
    ],
    stacktraces?: [
      type: :boolean,
      default: true,
      doc:
        "For Ash errors, whether or not each error has a stacktrace. See the error_handling guide for more."
    ],
    tenant: [
      type: :any,
      doc: "A tenant to set on the query or changeset"
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
                          doc: "Pagination options, see the pagination docs for more",
                          type: {:custom, __MODULE__, :page_opts, []}
                        ],
                        load: [
                          type: :any,
                          doc: "A load statement to add onto the query"
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

  @doc false
  def read_opts_schema, do: @read_opts_schema

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
    if page_opts in [false, nil] do
      {:ok, page_opts}
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
    case Spark.OptionsHelpers.validate(opts, schema) do
      {:ok, value} -> {:ok, value}
      {:error, error} -> {:error, Exception.message(error)}
    end
  end

  @load_opts_schema merge_schemas(
                      [
                        lazy?: [
                          type: :boolean,
                          doc:
                            "If set to true, values will only be loaded if the related value isn't currently loaded.",
                          default: false
                        ]
                      ],
                      @global_opts,
                      "Global Options"
                    )

  @get_opts_schema [
                     error?: [
                       type: :boolean,
                       default: true,
                       doc:
                         "Whether or not an error should be returned or raised when the record is not found. If set to false, `nil` will be returned."
                     ],
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
    ],
    notification_metadata: [
      type: :any,
      default: %{},
      doc: """
      Metadata to be merged into the metadata field for all notifications sent from this operation.
      """
    ]
  ]

  @create_update_opts_schema [
    after_action: [
      type: :any,
      doc: """
      A hook to be run just before the action returns, but before fields are selected (still inside the same transaction, if your data layer
      supports transactions). This is mostly important if you want to load calculations after the action, which depend on having fields
      selected, but you want to authorize with the minimal set of fields that are actually being selected. Runs only if the action is
      successful, and is passed the changeset and result of the action. Should return `{:ok, result}` or `{:error, error}`.

      For example, if you had a `full_name` calculation, but were only selecting, `first_name` and `full_name`, you might do
      something like this:
      ```elixir
      MyApp.User
      |> Ash.Changeset.for_create(:create, %{first_name: "first_name", last_name: "last_name"}
      |> Ash.Changeset.select(:first_name))
      |> Api.create(after_action: fn _changeset, user -> Api.load(user, :full_name) end)
      ```

      If you tried to load that `:full_name` calculation after receiving the data, the `last_name` would not be selected and as such would not be
      usable in the calculation, regardless of whether or not the calculation includes that field in its select list.
      """
    ]
  ]

  @create_opts_schema [
                        upsert?: [
                          type: :boolean,
                          default: false,
                          doc:
                            "If a conflict is found based on the primary key, the record is updated in the database (requires upsert support)"
                        ],
                        upsert_identity: [
                          type: :atom,
                          doc:
                            "The identity to use when detecting conflicts for `upsert?`, e.g. `upsert_identity: :full_name`. By default, the primary key is used. Has no effect if `upsert?: true` is not provided"
                        ]
                      ]
                      |> merge_schemas(@global_opts, "Global Options")
                      |> merge_schemas(
                        @create_update_opts_schema,
                        "Shared create/update Options"
                      )
                      |> merge_schemas(
                        @shared_created_update_and_destroy_opts_schema,
                        "Shared create/update/destroy Options"
                      )

  @doc false
  def create_opts_schema, do: @create_opts_schema

  @update_opts_schema []
                      |> merge_schemas(@global_opts, "Global Options")
                      |> merge_schemas(
                        @create_update_opts_schema,
                        "Shared create/update Options"
                      )
                      |> merge_schemas(
                        @shared_created_update_and_destroy_opts_schema,
                        "Shared create/update/destroy Options"
                      )

  @doc false
  def update_opts_schema, do: @update_opts_schema

  @destroy_opts_schema [
                         return_destroyed?: [
                           type: :boolean,
                           default: false,
                           doc:
                             "If true, the destroyed record is included in the return result, e.g `{:ok, destroyed}` or `{:ok, destroyed, notifications}`"
                         ]
                       ]
                       |> merge_schemas(@global_opts, "Global Opts")
                       |> merge_schemas(
                         @shared_created_update_and_destroy_opts_schema,
                         "Shared create/update/destroy Options"
                       )

  def destroy_opts_schema, do: @destroy_opts_schema

  @aggregate_opts [] |> Spark.OptionsHelpers.merge_schemas(@global_opts, "Global Options")

  @doc false
  def aggregate_opts, do: @aggregate_opts

  @doc """
  Runs an aggregate or aggregates over a resource query

  #{Spark.OptionsHelpers.docs(@aggregate_opts)}
  """
  def aggregate(api, query, aggregate_or_aggregates, opts \\ []) do
    query = Ash.Query.new(query)
    opts = Spark.OptionsHelpers.validate!(opts, @aggregate_opts)

    Ash.Actions.Aggregate.run(api, query, List.wrap(aggregate_or_aggregates), opts)
  end

  @calculate_opts [
    args: [
      type: :map,
      doc: """
      Values for arguments referenced by the calculation.
      """,
      default: %{}
    ],
    refs: [
      type: :map,
      doc: """
      Values for references used by the calculation.
      """,
      default: %{}
    ],
    actor: [
      type: :any,
      doc: """
      The actor for handling `^actor/1` templates.
      """
    ]
  ]

  @doc false
  def calculate_opts, do: @calculate_opts

  @doc """
  Evaluates the calculation on the resource.

  #{Spark.OptionsHelpers.docs(@calculate_opts)}
  """
  def calculate(resource, calculation, opts \\ []) do
    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, Ash.Api.calculate_opts()),
         {:calc,
          %{
            arguments: arguments,
            calculation: {module, calc_opts},
            type: type,
            constraints: constraints
          }} <-
           {:calc, Ash.Resource.Info.calculation(resource, calculation)},
         record <- struct(resource, opts[:refs] || %{}) do
      calc_context =
        opts[:context]
        |> Kernel.||(%{})
        |> Map.merge(opts[:args] || %{})
        |> Map.put(:ash, %{type: type, constraints: constraints})

      calc_context =
        Enum.reduce(arguments, calc_context, fn arg, context ->
          if Map.has_key?(context, arg.name) do
            context
          else
            if is_nil(arg.default) do
              context
            else
              Map.put(context, arg.name, arg.default)
            end
          end
        end)
        |> Map.put(:actor, opts[:actor])

      if function_exported?(module, :expression, 2) do
        expr =
          case module.expression(calc_opts, calc_context) do
            {:ok, result} -> {:ok, result}
            {:error, error} -> {:error, error}
            result -> {:ok, result}
          end

        with {:ok, expr} <- expr do
          case Ash.Expr.eval(expr) do
            {:ok, result} ->
              {:ok, result}

            :unknown ->
              case module.calculate([record], calc_opts, calc_context) do
                [result] ->
                  result

                {:ok, [result]} ->
                  {:ok, result}

                {:ok, _} ->
                  {:error, "Invalid calculation return"}

                {:error, error} ->
                  {:error, error}
              end

            {:error, error} ->
              {:error, error}
          end
        end
      else
        case module.calculate([record], calc_opts, calc_context) do
          [result] ->
            {:ok, result}

          {:ok, [result]} ->
            {:ok, result}

          {:ok, _} ->
            {:error, "Invalid calculation return"}

          {:error, error} ->
            {:error, error}
        end
      end
    else
      {:calc, nil} ->
        {:error, "No such calculation"}

      {:error, error} ->
        {:error, error}
    end
  end

  @callback calculate(resource :: Ash.Resource.t(), calculation :: atom, opts :: Keyword.t()) ::
              {:ok, term} | {:error, term}

  @callback calculate!(resource :: Ash.Resource.t(), calculation :: atom, opts :: Keyword.t()) ::
              term | no_return

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

  #{Spark.OptionsHelpers.docs(@get_opts_schema)}
  """
  @callback get(
              resource :: Ash.Resource.t(),
              id_or_filter :: term(),
              params :: Keyword.t()
            ) ::
              {:ok, Ash.Resource.record()} | {:ok, nil} | {:error, term}

  @doc """
  Run an ash query, raising on more than one result. See `c:read_one/2` for more.
  """
  @callback read_one!(Ash.Query.t() | Ash.Resource.t(), params :: Keyword.t()) ::
              Ash.Resource.record() | {Ash.Resource.record(), Ash.Query.t()} | nil | no_return

  @doc """
  Run a query on a resource, but fail on more than one result.

  This is useful if you have a query that doesn't include a primary key
  but you know that it will only ever return a single result.
  """
  @callback read_one(Ash.Query.t() | Ash.Resource.t(), params :: Keyword.t()) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), Ash.Query.t()}
              | {:ok, nil}
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

  For more information on building a query, see `Ash.Query`.

  #{Spark.OptionsHelpers.docs(@read_opts_schema)}

  ## Pagination

  #### Limit/offset pagination
  #{Spark.OptionsHelpers.docs(@offset_page_opts)}

  #### Keyset pagination
  #{Spark.OptionsHelpers.docs(@keyset_page_opts)}
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

  @type load_statement ::
          Ash.Query.t()
          | [atom]
          | atom
          | Keyword.t()
          | list(atom | {atom, atom | Keyword.t()})

  @doc """
  Load fields or relationships on already fetched records. See `c:load/3` for more information.
  """

  @callback load!(
              record_or_records :: Ash.Resource.record() | [Ash.Resource.record()],
              query :: load_statement(),
              opts :: Keyword.t()
            ) ::
              Ash.Resource.record() | [Ash.Resource.record()] | no_return

  @doc """
  Load fields or relationships on already fetched records.

  Accepts a list of non-loaded fields and loads them on the provided records or a query, in
  which case the loaded fields of the query are used. Relationship loads can be nested, for
  example: `MyApi.load(record, [posts: [:comments]])`.

  #{Spark.OptionsHelpers.docs(@load_opts_schema)}
  """
  @callback load(
              record_or_records :: Ash.Resource.record() | [Ash.Resource.record()],
              query :: load_statement(),
              opts :: Keyword.t()
            ) ::
              {:ok, Ash.Resource.record() | [Ash.Resource.record()]} | {:error, term}

  @doc """
  Create a record. See `c:create/2` for more information.
  """
  @callback create!(Ash.Changeset.t(), params :: Keyword.t()) ::
              Ash.Resource.record()
              | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | no_return

  @doc """
  Create a record.

  #{Spark.OptionsHelpers.docs(@create_opts_schema)}
  """
  @callback create(Ash.Changeset.t(), params :: Keyword.t()) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

  @doc """
  Update a record. See `c:update/2` for more information.
  """
  @callback update!(Ash.Changeset.t(), params :: Keyword.t()) ::
              Ash.Resource.record()
              | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | no_return

  @doc """
  Update a record.

  #{Spark.OptionsHelpers.docs(@update_opts_schema)}
  """
  @callback update(Ash.Changeset.t(), params :: Keyword.t()) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

  @doc """
  Destroy a record. See `c:destroy/2` for more information.
  """
  @callback destroy!(Ash.Changeset.t() | Ash.Resource.record(), params :: Keyword.t()) ::
              :ok
              | Ash.Resource.record()
              | list(Ash.Notifier.Notification.t())
              | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | no_return

  @doc """
  Destroy a record.

  #{Spark.OptionsHelpers.docs(@destroy_opts_schema)}
  """
  @callback destroy(Ash.Changeset.t() | Ash.Resource.record(), params :: Keyword.t()) ::
              :ok
              | {:ok, Ash.Resource.record()}
              | {:ok, list(Ash.Notifier.Notification.t())}
              | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

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

  @doc false
  @impl Spark.Dsl
  def handle_opts(_) do
    quote do
      @behaviour Ash.Api
    end
  end

  @doc false
  # sobelow_skip ["DOS.StringToAtom"]
  @impl Spark.Dsl
  def handle_before_compile(_) do
    quote do
      use Ash.Api.Interface

      @default_short_name __MODULE__
                          |> Module.split()
                          |> List.last()
                          |> Macro.underscore()
                          |> String.to_atom()

      def default_short_name do
        @default_short_name
      end
    end
  end

  @deprecated "use Ash.Api.Info.resource/2 instead"
  defdelegate resource(api, resource), to: Ash.Api.Info

  @deprecated "use Ash.Api.Info.resources/1 instead"
  defdelegate resources(api), to: Ash.Api.Info

  @deprecated "use Ash.Api.Info.registry/1 instead"
  defdelegate registry(api), to: Ash.Api.Info

  @deprecated "use Ash.Api.Info.allow/1 instead"
  defdelegate allow(api), to: Ash.Api.Info

  @deprecated "use Ash.Api.Info.timeout/1 instead"
  defdelegate timeout(api), to: Ash.Api.Info

  @deprecated "use Ash.Api.Info.require_actor?/1 instead"
  defdelegate require_actor?(api), to: Ash.Api.Info

  @deprecated "use Ash.Api.Info.authorize/1 instead"
  defdelegate authorize(api), to: Ash.Api.Info

  @deprecated "use Ash.Api.Info.allow_unregistered?/1 instead"
  defdelegate allow_unregistered?(api), to: Ash.Api.Info

  @doc false
  @spec get!(Ash.Api.t(), Ash.Resource.t(), term(), Keyword.t()) ::
          Ash.Resource.record() | no_return
  def get!(api, resource, id, opts \\ []) do
    opts = Spark.OptionsHelpers.validate!(opts, @get_opts_schema)

    api
    |> get(resource, id, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec get(Ash.Api.t(), Ash.Resource.t(), term(), Keyword.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def get(api, resource, id, opts) do
    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @get_opts_schema),
         {:ok, resource} <- Ash.Api.Info.resource(api, resource),
         {:ok, filter} <- Ash.Filter.get_filter(resource, id) do
      query =
        resource
        |> Ash.Query.new(api)
        |> Ash.Query.set_tenant(opts[:tenant])
        |> Ash.Query.filter(^filter)
        |> Ash.Query.load(opts[:load] || [])
        |> Ash.Query.set_context(opts[:context] || %{})

      query =
        if Ash.DataLayer.data_layer_can?(query.resource, :limit) do
          Ash.Query.limit(query, 2)
        else
          query
        end

      query
      |> api.read(Keyword.take(opts, Keyword.keys(@read_opts_schema)))
      |> case do
        {:ok, %{results: [single_result]}} ->
          {:ok, single_result}

        {:ok, %{results: []}} ->
          if opts[:error?] do
            {:error,
             NotFound.exception(
               primary_key: filter,
               resource: resource
             )}
          else
            {:ok, nil}
          end

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
          if opts[:error?] do
            {:error,
             NotFound.exception(
               primary_key: filter,
               resource: resource
             )}
          else
            {:ok, nil}
          end

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

  @doc false
  def page!(api, keyset, request) do
    {_, opts} = keyset.rerun

    api
    |> page(keyset, request)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
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
  def load!(api, data, query, opts \\ []) do
    opts = Spark.OptionsHelpers.validate!(opts, @load_opts_schema)

    api
    |> load(data, query, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  def load(api, data, query, opts \\ [])
  def load(_, [], _, _), do: {:ok, []}
  def load(_, nil, _, _), do: {:ok, nil}
  def load(_, {:error, error}, _, _), do: {:error, error}

  def load(api, {:ok, values}, query, opts) do
    load(api, values, query, opts)
  end

  def load(api, %struct{results: results} = page, query, opts)
      when struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    api
    |> load(results, query, opts)
    |> case do
      {:ok, results} -> {:ok, %{page | results: results}}
      {:error, error} -> {:error, error}
    end
  end

  def load(api, data, query, opts) when not is_list(data) do
    api
    |> load(List.wrap(data), query, opts)
    |> case do
      {:ok, data} -> {:ok, Enum.at(data, 0)}
      {:error, error} -> {:error, error}
    end
  end

  def load(api, [%resource{} = record | _] = data, query, opts) do
    query =
      case query do
        %Ash.Query{} = query ->
          Ash.Query.set_tenant(query, query.tenant || Map.get(record.__metadata__, :tenant))

        keyword ->
          resource
          |> Ash.Query.new(api)
          |> Ash.Query.set_tenant(Map.get(record.__metadata__, :tenant))
          |> Ash.Query.load(keyword)
      end

    query = Map.put(query, :api, api)

    with %{valid?: true} <- query,
         {:ok, action} <- get_action(query.resource, opts, :read, query.action),
         {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @load_opts_schema) do
      Read.unpaginated_read(query, action, Keyword.merge(opts, initial_data: data))
    else
      {:error, error} ->
        {:error, error}

      %{errors: errors} ->
        {:error, errors}
    end
  end

  @doc false
  @spec read!(Ash.Api.t(), Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          list(Ash.Resource.record()) | Ash.Page.page() | no_return
  def read!(api, query, opts \\ []) do
    opts = Spark.OptionsHelpers.validate!(opts, @read_opts_schema)

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

    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @read_opts_schema),
         {:ok, action} <- get_action(query.resource, opts, :read, query.action),
         {:ok, action} <- pagination_check(action, query.resource, opts) do
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
          Ash.Resource.record()
          | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | no_return
  def create!(api, changeset, opts) do
    opts = Spark.OptionsHelpers.validate!(opts, @create_opts_schema)

    api
    |> create(changeset, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec create(Ash.Api.t(), Ash.Changeset.t(), Keyword.t()) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:error, term}
  def create(api, changeset, opts) do
    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @create_opts_schema),
         {:ok, resource} <- Ash.Api.Info.resource(api, changeset.resource),
         {:ok, action} <- get_action(resource, opts, :create, changeset.action) do
      Create.run(api, changeset, action, opts)
    end
  end

  @doc false
  def update!(api, changeset, opts) do
    opts = Spark.OptionsHelpers.validate!(opts, @update_opts_schema)

    api
    |> update(changeset, opts)
    |> unwrap_or_raise!(opts[:stacktraces?])
  end

  @doc false
  @spec update(Ash.Api.t(), Ash.Resource.record(), Keyword.t()) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:error, term}
  def update(api, changeset, opts) do
    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @update_opts_schema),
         {:ok, resource} <- Ash.Api.Info.resource(api, changeset.resource),
         {:ok, action} <- get_action(resource, opts, :update, changeset.action) do
      Update.run(api, changeset, action, opts)
    end
  end

  @doc false
  @spec destroy!(Ash.Api.t(), Ash.Changeset.t() | Ash.Resource.record(), Keyword.t()) ::
          :ok
          | Ash.Resource.record()
          | list(Ash.Notifier.Notification.t())
          | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | no_return()
  def destroy!(api, changeset, opts) do
    opts = Spark.OptionsHelpers.validate!(opts, @destroy_opts_schema)

    api
    |> destroy(changeset, opts)
    |> unwrap_or_raise!(
      opts[:stacktraces?],
      !(opts[:return_notifications?] || opts[:return_destroyed?])
    )
  end

  @doc false
  @spec destroy(Ash.Api.t(), Ash.Changeset.t() | Ash.Resource.record(), Keyword.t()) ::
          :ok
          | {:ok, Ash.Resource.record()}
          | {:ok, list(Ash.Notifier.Notification.t())}
          | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:error, term}
  def destroy(api, %Ash.Changeset{resource: resource} = changeset, opts) do
    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @destroy_opts_schema),
         {:ok, resource} <- Ash.Api.Info.resource(api, resource),
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
              if Ash.Resource.Info.resource?(resource) do
                {:error, NoPrimaryAction.exception(resource: resource, type: type)}
              else
                {:error, NoSuchResource.exception(resource: resource)}
              end

            action ->
              {:ok, action}
          end
        end
    end
  end

  defp pagination_check(action, resource, opts) do
    if Keyword.get(opts, :page) && Keyword.get(opts, :page) != [] && !Map.get(action, :pagination) do
      {:error,
       Ash.Error.to_error_class(
         PageRequiresPagination.exception(resource: resource, action: action)
       )}
    else
      {:ok, action}
    end
  end

  defp unwrap_or_raise!(first, second, destroy? \\ false)
  defp unwrap_or_raise!(:ok, _, _), do: :ok
  defp unwrap_or_raise!({:ok, result}, _, false), do: result
  defp unwrap_or_raise!({:ok, _result}, _, true), do: :ok
  defp unwrap_or_raise!({:ok, result, other}, _, _), do: {result, other}

  defp unwrap_or_raise!({:error, error}, stacktraces?, destroy?) when is_list(error) do
    unwrap_or_raise!({:error, Ash.Error.to_error_class(error)}, stacktraces?, destroy?)
  end

  defp unwrap_or_raise!({:error, error}, stacktraces?, _) do
    exception = Ash.Error.to_error_class(error)

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

  @impl Spark.Dsl
  def explain(dsl_state, _opts) do
    Ash.Api.Info.description(dsl_state)
  end
end
