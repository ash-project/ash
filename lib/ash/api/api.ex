defmodule Ash.Api do
  @moduledoc """
  An Api allows you to interact with your resources, and holds non-resource-specific configuration.

  For example, the json api extension adds an api extension that lets you toggle authorization on/off
  for all resources in that Api. You include them in an Api like so:

  ```elixir
  defmodule MyApp.Registry do
    use Ash.Registry

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

  use Spark.Dsl,
    default_extensions: [extensions: [Ash.Api.Dsl]],
    opt_schema: [
      validate_config_inclusion?: [
        type: :boolean,
        default: true
      ]
    ]

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

  @dialyzer {:nowarn_function, unwrap_or_raise!: 2}

  @type t() :: module

  @type page_request ::
          :next | :prev | :first | :last | :self | integer

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
      type: {:wrap_list, {:behaviour, Ash.Tracer}},
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
                        max_concurrency: [
                          type: :non_neg_integer,
                          doc:
                            "The maximum number of processes allowed to be started for parallel loading of relationships and calculations. Defaults to `System.schedulers_online() * 2`"
                        ],
                        lock: [
                          type: :any,
                          doc: "A lock statement to add onto the query"
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
                        ],
                        reselect_all?: [
                          type: :boolean,
                          default: false,
                          doc: """
                          Whether or not to reselect all attributes depended on by loads.
                          By default, we only reselect fields that weren't already selected.
                          """
                        ]
                      ],
                      @global_opts,
                      "Global Options"
                    )

  @doc false
  def read_opts_schema, do: @read_opts_schema

  @read_one_opts_schema merge_schemas(
                          [
                            not_found_error?: [
                              type: :boolean,
                              default: false,
                              doc:
                                "Whether or not to return an `Ash.Error.Query.NotFound` if no record is found."
                            ]
                          ],
                          @read_opts_schema,
                          "Read Options"
                        )

  @stream_opts [
                 batch_size: [
                   type: :integer,
                   doc:
                     "How many records to request in each query run. Defaults to the pagination limits on the resource, or 250."
                 ],
                 allow_stream_with: [
                   type: {:one_of, [:keyset, :offset, :full_read]},
                   doc:
                     "The 'worst' strategy allowed to be used to fetch records. See `Ash.Api.stream!/2` docs for more.",
                   default: :keyset
                 ],
                 stream_with: [
                   type: {:one_of, [:keyset, :offset, :full_read]},
                   doc:
                     "The specific strategy to use to fetch records. See `Ash.Api.stream!/2` docs for more."
                 ]
               ]
               |> merge_schemas(
                 @read_opts_schema,
                 "Read Options"
               )

  @doc """
  Streams the results of a query.

  ## Strategies

  There are three strategies supported, and the best one available is always chosen. They are,
  in order from best to worst:

  - `:keyset`
  - `:offset`
  - `:full_read`

  By default, only `:keyset` is supported. If you want to allow worse strategies to be used, pass
  the worst one you wish to allow as the `allow_stream_with` option, i.e `allow_stream_with: :full_read`.
  If you wish to specify a specific strategy to use, pass `stream_with: :strategy_name`.

  ### Keyset

  This utilizes keyset pagination to accomplish this stream. The action must support keyset pagination.
  This is the most efficient way to stream a query, because it works by using filters which can benefit
  from indexes in the data layer.

  ### Offset

  This utilizes offset/limit to accomplish this stream. If the action supports offset pagination, that will
  be used. Otherwise, if the data layer supports limit/offset, then explicit limits/offsets will be used.
  This is a much less efficient way of streaming a resource than `keyset`. To use limit/offset to reliably
  stream, a sort must always be applied, and limit/offset in the data layer will generally require sorting
  the entire table to figure out what is in each batch.

  ### Full Read

  This reads the entire table into memory with no limit. This is, generally speaking, the least efficient.

  ## Options

  #{Spark.OptionsHelpers.docs(@stream_opts)}
  """
  @callback stream!(Ash.Query.t(), opts :: Keyword.t()) :: Enumerable.t(Ash.Resource.record())

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
                        ],
                        reselect_all?: [
                          type: :boolean,
                          default: false,
                          doc: """
                          Whether or not to reselect all attributes depended on by loads.
                          By default, we only reselect fields that weren't already selected.
                          """
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
                     lock: [
                       type: :any,
                       doc: "A lock statement to add onto the query"
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
                     ],
                     reselect_all?: [
                       type: :boolean,
                       default: false,
                       doc: """
                       Whether or not to reselect all attributes depended on by loads.
                       By default, we only reselect fields that weren't already selected.
                       """
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
    rollback_on_error?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not to rollback the transaction on error, if the resource is in a transaction.

      If the action has `transaction? false` this option has no effect. If an error is returned from the
      data layer and the resource is in a transaction, the transaction is always rolled back, regardless.
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

  @create_update_opts_schema []

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
                        ],
                        upsert_fields: [
                          type:
                            {:or,
                             [
                               {:literal, :replace_all},
                               {:tuple, [{:literal, :replace}, {:wrap_list, :atom}]},
                               {:tuple, [{:literal, :replace_all_except}, {:wrap_list, :atom}]},
                               {:wrap_list, :atom}
                             ]},
                          doc:
                            "The fields to upsert. If not set, the action's upsert_fields is used, and if that is not set, then any fields not being set to defaults are written."
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

  @shared_bulk_opts_schema [
    assume_casted?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not to cast attributes and arguments as input. This is an optimization for cases where the input is already casted and/or not in need of casting"
    ],
    context: [
      type: :map,
      doc: "Context to set on each changeset"
    ],
    sorted?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not to sort results by their input position, in cases where `return_records?: true` was provided."
    ],
    return_records?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not to return all of the records that were inserted. Defaults to false to account for large inserts."
    ],
    return_errors?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not to return all of the errors that occur. Defaults to false to account for large inserts."
    ],
    batch_size: [
      type: :pos_integer,
      doc: """
      The number of records to include in each batch. Defaults to the `default_limit`
      or `max_page_size` of the action, or 100.
      """
    ],
    return_stream?: [
      type: :boolean,
      default: false,
      doc: """
      If set to `true`, instead of an `Ash.BulkResult`, a mixed stream is returned.

      Potential elements:

      `{:notification, notification}` - if `return_notifications?` is set to `true`
      `{:ok, record}` - if `return_records?` is set to `true`
      `{:error, error}` - an error that occurred. May be changeset or an invidual error.
      """
    ],
    stop_on_error?: [
      type: :boolean,
      default: false,
      doc: """
      If true, the first encountered error will stop the action and be returned. Otherwise, errors
      will be skipped.
      """
    ],
    notify?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not to send notifications out. If this is set to `true` then the data layer must return
      the results from each batch. This may be intensive for large bulk actions.
      """
    ],
    transaction: [
      type: {:one_of, [:all, :batch, false]},
      default: :batch,
      doc: """
      Whether or not to wrap the entire execution in a transaction, each batch, or not at all.

      Keep in mind:

      `before_transaction` and `after_transaction` hooks attached to changesets will have to be run
      *inside* the transaction if you choose `transaction: :all`.
      """
    ],
    max_concurrency: [
      type: :non_neg_integer,
      default: 0,
      doc:
        "If set to a value greater than 0, up to that many tasks will be started to run batches asynchronously"
    ]
  ]

  @bulk_update_opts_schema [
                             resource: [
                               type: {:spark, Ash.Resource},
                               doc:
                                 "The resource being updated. This must be provided if the input given is a stream, so we know ahead of time what the resource being updated is."
                             ],
                             atomic_update: [
                               type: :map,
                               doc:
                                 "A map of atomic updates to apply. See `Ash.Changeset.atomic_update/3` for more."
                             ],
                             stream_batch_size: [
                               type: :integer,
                               doc:
                                 "Batch size to use if provided a query and the query must be streamed"
                             ]
                           ]
                           |> merge_schemas(
                             Keyword.delete(@global_opts, :action),
                             "Global options"
                           )
                           |> merge_schemas(
                             Keyword.delete(@stream_opts, :batch_size),
                             "Stream Options"
                           )
                           |> merge_schemas(
                             @shared_created_update_and_destroy_opts_schema,
                             "Shared create/update/destroy options"
                           )
                           |> merge_schemas(
                             @shared_bulk_opts_schema,
                             "Shared bulk options"
                           )

  @bulk_destroy_opts_schema [
                              resource: [
                                type: {:spark, Ash.Resource},
                                doc:
                                  "The resource being updated. This must be provided if the input given is a stream, so we know ahead of time what the resource being updated is."
                              ],
                              stream_batch_size: [
                                type: :integer,
                                doc:
                                  "Batch size to use if provided a query and the query must be streamed"
                              ]
                            ]
                            |> merge_schemas(
                              Keyword.delete(@global_opts, :action),
                              "Global options"
                            )
                            |> merge_schemas(
                              Keyword.delete(@stream_opts, :batch_size),
                              "Stream Options"
                            )
                            |> merge_schemas(
                              @shared_created_update_and_destroy_opts_schema,
                              "Shared create/update/destroy options"
                            )
                            |> merge_schemas(
                              @shared_bulk_opts_schema,
                              "Shared bulk options"
                            )

  @bulk_create_opts_schema [
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
                             ],
                             upsert_fields: [
                               type:
                                 {:or,
                                  [
                                    {:literal, :replace_all},
                                    {:tuple, [{:literal, :replace}, {:wrap_list, :atom}]},
                                    {:tuple,
                                     [{:literal, :replace_all_except}, {:wrap_list, :atom}]},
                                    {:wrap_list, :atom}
                                  ]},
                               doc:
                                 "The fields to upsert. If not set, the action's `upsert_fields` is used. Unlike singular `create`, `bulk_create` with `upsert?` requires that `upsert_fields` be specified explicitly in one of these two locations."
                             ]
                           ]
                           |> merge_schemas(
                             Keyword.delete(@global_opts, :action),
                             "Global options"
                           )
                           |> merge_schemas(
                             @shared_created_update_and_destroy_opts_schema,
                             "Shared create/update/destroy options"
                           )
                           |> merge_schemas(
                             @shared_bulk_opts_schema,
                             "Shared bulk options"
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

  @type aggregate ::
          Ash.Query.Aggregate.t()
          | {name :: atom, kind :: atom}
          | {name :: atom, kind :: atom, opts :: Keyword.t()}
  @doc """
  Runs an aggregate or aggregates over a resource query

  #{Spark.OptionsHelpers.docs(@aggregate_opts)}
  """
  @spec aggregate(
          api :: Ash.Api.t(),
          Ash.Query.t() | Ash.Resource.t(),
          aggregates :: aggregate | list(aggregate),
          opts :: Keyword.t()
        ) ::
          {:ok, term} | {:error, Ash.Error.t()}
  def aggregate(api, query, aggregate_or_aggregates, opts \\ []) do
    query = Ash.Query.new(query)
    opts = Spark.OptionsHelpers.validate!(opts, @aggregate_opts)

    Ash.Actions.Aggregate.run(api, query, List.wrap(aggregate_or_aggregates), opts)
  end

  @spec can?(
          api :: Ash.Api.t(),
          query_or_changeset_or_action ::
            Ash.Query.t()
            | Ash.Changeset.t()
            | Ash.ActionInput.t()
            | {Ash.Resource.t(), atom | Ash.Resource.Actions.action()}
            | {Ash.Resource.t(), atom | Ash.Resource.Actions.action(), input :: map},
          actor :: term,
          opts :: Keyword.t()
        ) ::
          boolean | no_return
  def can?(api, action_or_query_or_changeset, actor, opts \\ []) do
    opts = Keyword.put_new(opts, :maybe_is, true)

    case can(api, action_or_query_or_changeset, actor, opts) do
      {:ok, :maybe} -> opts[:maybe_is]
      {:ok, result} -> result
      {:ok, true, _} -> {:ok, true}
      {:error, error} -> raise Ash.Error.to_ash_error(error)
    end
  end

  @spec can(
          api :: Ash.Api.t(),
          action_or_query_or_changeset ::
            Ash.Query.t()
            | Ash.Changeset.t()
            | Ash.ActionInput.t()
            | {Ash.Resource.t(), atom | Ash.Resource.Actions.action()}
            | {Ash.Resource.t(), atom | Ash.Resource.Actions.action(), input :: map},
          actor :: term,
          opts :: Keyword.t()
        ) ::
          {:ok, boolean | :maybe}
          | {:ok, true, Ash.Changeset.t() | Ash.Query.t()}
          | {:ok, true, Ash.Changeset.t(), Ash.Query.t()}
          | {:ok, false, Exception.t()}
          | {:error, term}
  def can(api, action_or_query_or_changeset, actor, opts \\ []) do
    opts = Keyword.put_new(opts, :maybe_is, :maybe)
    opts = Keyword.put_new(opts, :run_queries?, true)

    {resource, action_or_query_or_changeset, input} =
      case action_or_query_or_changeset do
        %Ash.Query{} = query ->
          {query.resource, query, nil}

        %Ash.Changeset{} = changeset ->
          {changeset.resource, changeset, nil}

        %Ash.ActionInput{} = input ->
          {input.resource, input, nil}

        {resource, %struct{}} = action
        when struct in [
               Ash.Resource.Actions.Create,
               Ash.Resource.Actions.Read,
               Ash.Resource.Actions.Update,
               Ash.Resource.Actions.Destroy,
               Ash.Resource.Actions.Action
             ] ->
          {resource, action, %{}}

        {resource, name} when is_atom(name) ->
          {resource, Ash.Resource.Info.action(resource, name), %{}}

        {resource, %struct{}, input} = action
        when struct in [
               Ash.Resource.Actions.Create,
               Ash.Resource.Actions.Read,
               Ash.Resource.Actions.Update,
               Ash.Resource.Actions.Destroy,
               Ash.Resource.Actions.Action
             ] ->
          {resource, action, input}

        {resource, name, input} when is_atom(name) ->
          {resource, Ash.Resource.Info.action(resource, name), input}
      end

    subject =
      case action_or_query_or_changeset do
        %{type: :update, name: name} ->
          if opts[:data] do
            Ash.Changeset.for_update(opts[:data], name, input,
              actor: actor,
              tenant: opts[:tenant]
            )
          else
            resource
            |> struct()
            |> Ash.Changeset.for_update(name, input, actor: actor, tenant: opts[:tenant])
          end

        %{type: :create, name: name} ->
          Ash.Changeset.for_create(resource, name, input, actor: actor, tenant: opts[:tenant])

        %{type: :read, name: name} ->
          Ash.Query.for_read(resource, name, input, actor: actor, tenant: opts[:tenant])

        %{type: :destroy, name: name} ->
          if opts[:data] do
            Ash.Changeset.for_destroy(opts[:data], name, input,
              actor: actor,
              tenant: opts[:tenant]
            )
          else
            resource
            |> struct()
            |> Ash.Changeset.for_destroy(name, input, actor: actor, tenant: opts[:tenant])
          end

        %{type: :action, name: name} ->
          Ash.ActionInput.for_action(resource, name, input, actor: actor)

        %Ash.ActionInput{} = action_input ->
          action_input

        %Ash.Query{} = query ->
          if opts[:tenant] do
            Ash.Query.set_tenant(query, opts[:tenant])
          else
            query
          end

        %Ash.Changeset{} = changeset ->
          if opts[:tenant] do
            Ash.Changeset.set_tenant(changeset, opts[:tenant])
          else
            changeset
          end

        _ ->
          raise ArgumentError,
            message: "Invalid action/query/changeset \"#{inspect(action_or_query_or_changeset)}\""
      end

    subject = %{subject | api: api}

    api
    |> run_check(actor, subject, opts)
    |> alter_source(api, actor, subject, opts)
  end

  defp alter_source({:ok, true, query}, api, actor, %Ash.Changeset{} = subject, opts) do
    case alter_source({:ok, true}, api, actor, subject, opts) do
      {:ok, true, new_subject} -> {:ok, true, new_subject, query}
      other -> other
    end
  end

  defp alter_source({:ok, true, query}, api, actor, _subject, opts) do
    alter_source({:ok, true}, api, actor, query, opts)
  end

  defp alter_source({:ok, true}, api, actor, subject, opts) do
    if opts[:alter_source?] do
      subject.resource
      |> Ash.Resource.Info.authorizers()
      |> case do
        [] ->
          {:ok, true, subject}

        authorizers ->
          authorizers
          |> Enum.reduce(
            {:ok, true, subject},
            fn authorizer, {:ok, true, subject} ->
              authorizer_state =
                authorizer.initial_state(
                  actor,
                  subject.resource,
                  subject.action,
                  false
                )

              context = %{api: api, query: nil, changeset: nil, action_input: nil}

              case subject do
                %Ash.Query{} = query ->
                  context = Map.put(context, :query, query)

                  with {:ok, query, _} <-
                         Ash.Authorizer.add_calculations(
                           authorizer,
                           query,
                           authorizer_state,
                           context
                         ),
                       {:ok, new_filter} <-
                         Ash.Authorizer.alter_filter(
                           authorizer,
                           authorizer_state,
                           query.filter,
                           context
                         ),
                       {:ok, hydrated} <-
                         Ash.Filter.hydrate_refs(new_filter, %{
                           resource: query.resource,
                           public?: false
                         }),
                       {:ok, new_sort} <-
                         Ash.Authorizer.alter_sort(
                           authorizer,
                           authorizer_state,
                           query.sort,
                           context
                         ) do
                    {:ok, true, %{query | filter: hydrated, sort: new_sort}}
                  end

                %Ash.Changeset{} = changeset ->
                  context = Map.put(context, :changeset, changeset)

                  with {:ok, changeset, _} <-
                         Ash.Authorizer.add_calculations(
                           authorizer,
                           changeset,
                           authorizer_state,
                           context
                         ) do
                    {:ok, true, changeset}
                  end

                %Ash.ActionInput{} = subject ->
                  {:ok, true, subject}
              end
            end
          )
      end
    else
      {:ok, true}
    end
  end

  defp alter_source(other, _, _, _, _), do: other

  defp run_check(api, actor, subject, opts) do
    authorizers =
      Ash.Resource.Info.authorizers(subject.resource)
      |> Enum.map(fn authorizer ->
        authorizer_state =
          authorizer.initial_state(
            actor,
            subject.resource,
            subject.action,
            false
          )

        context = %{api: api, query: nil, changeset: nil, action_input: nil}

        context =
          case subject do
            %Ash.Query{} -> Map.put(context, :query, subject)
            %Ash.Changeset{} -> Map.put(context, :changeset, subject)
            %Ash.ActionInput{} -> Map.put(context, :action_input, subject)
          end

        {authorizer, authorizer_state, context}
      end)

    base_query =
      case subject do
        %Ash.Query{} = query ->
          opts[:base_query] || query

        _ ->
          opts[:base_query]
      end

    case authorizers do
      [] ->
        {:ok, true}

      authorizers ->
        authorizers
        |> Enum.reduce_while(
          {false, base_query},
          fn {authorizer, authorizer_state, context}, {_authorized?, query} ->
            case authorizer.strict_check(authorizer_state, context) do
              {:error, %{class: :forbidden} = e} when is_exception(e) ->
                {:halt, {false, e}}

              {:error, error} ->
                {:halt, {:error, error}}

              {:authorized, _} ->
                {:cont, {true, query}}

              :forbidden ->
                {:halt,
                 {false, Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state)}}

              _ when not is_nil(context.action_input) ->
                raise """
                Cannot use filter or runtime checks with generic actions

                Failed when authorizing #{inspect(subject.resource)}.#{subject.action.name}
                """

              {:filter, _authorizer, filter} ->
                {:cont, {true, Ash.Query.filter(or_query(query, subject.resource, api), ^filter)}}

              {:filter, filter} ->
                {:cont, {true, Ash.Query.filter(or_query(query, subject.resource, api), ^filter)}}

              {:continue, authorizer_state} ->
                if opts[:alter_source?] do
                  query_with_hook =
                    Ash.Query.authorize_results(or_query(query, subject.resource, api), fn query,
                                                                                           results ->
                      context = Map.merge(context, %{data: results, query: query})

                      case authorizer.check(authorizer_state, context) do
                        :authorized -> {:ok, results}
                        {:error, error} -> {:error, error}
                        {:data, data} -> {:ok, data}
                      end
                    end)

                  {:cont, {true, query_with_hook}}
                else
                  if opts[:maybe_is] == false do
                    {:halt,
                     {false, Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state)}}
                  else
                    {:halt, {:maybe, nil}}
                  end
                end

              {:filter_and_continue, filter, authorizer_state} ->
                if opts[:alter_source?] do
                  query_with_hook =
                    query
                    |> or_query(subject.resource, api)
                    |> Ash.Query.filter(^filter)
                    |> Ash.Query.authorize_results(fn query, results ->
                      context = Map.merge(context, %{data: results, query: query})

                      case authorizer.check(authorizer_state, context) do
                        :authorized -> {:ok, results}
                        {:error, error} -> {:error, error}
                        {:data, data} -> {:ok, data}
                      end
                    end)

                  {:cont, {true, query_with_hook}}
                else
                  if opts[:maybe_is] == false do
                    {:halt,
                     {false, Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state)}}
                  else
                    {:halt, {:maybe, nil}}
                  end
                end
            end
          end
        )
        |> case do
          {:error, error} ->
            {:error, error}

          {true, query} when not is_nil(query) ->
            if opts[:run_queries?] do
              run_queries(subject, opts, authorizers, query)
            else
              if opts[:alter_source?] do
                {:ok, true, query}
              else
                {:ok, :maybe}
              end
            end

          {false, error} ->
            if opts[:return_forbidden_error?] do
              {:ok, false, error || authorizer_exception(authorizers)}
            else
              {:ok, false}
            end

          {other, _} ->
            {:ok, other}
        end
        |> case do
          {:ok, :maybe} ->
            if opts[:maybe_is] == false && opts[:return_forbidden_error?] do
              {:ok, false, authorizer_exception(authorizers)}
            else
              {:ok, opts[:maybe_is]}
            end

          other ->
            other
        end
    end
  end

  defp run_queries(subject, opts, authorizers, query) do
    case subject do
      %Ash.Query{} ->
        if opts[:data] do
          data = List.wrap(opts[:data])

          pkey = Ash.Resource.Info.primary_key(query.resource)
          pkey_values = Enum.map(data, &Map.take(&1, pkey))

          if Enum.any?(pkey_values, fn pkey_value ->
               pkey_value |> Map.values() |> Enum.any?(&is_nil/1)
             end) do
            {:ok, :maybe}
          else
            query
            |> Ash.Query.do_filter(or: pkey_values)
            |> Ash.Query.data_layer_query()
            |> case do
              {:ok, data_layer_query} ->
                data_layer_query
                |> Ash.DataLayer.run_query(query.resource)
                |> Ash.Actions.Helpers.rollback_if_in_transaction(query.resource, query)
                |> case do
                  {:ok, results} ->
                    case Ash.Actions.Read.run_authorize_results(query, results) do
                      {:ok, results} ->
                        if Enum.count(results) == Enum.count(data) do
                          {:ok, true}
                        else
                          if opts[:return_forbidden_error?] do
                            {:ok, false, authorizer_exception(authorizers)}
                          else
                            {:ok, false}
                          end
                        end

                      {:error, error} ->
                        {:error, error}
                    end

                  {:error, error} ->
                    {:error, error}
                end
            end
          end
        else
          {:ok, true}
        end

      %Ash.Changeset{data: data, action_type: type, resource: resource, tenant: tenant}
      when type in [:update, :destroy] ->
        pkey = Ash.Resource.Info.primary_key(resource)
        pkey_value = Map.take(data, pkey)

        if pkey_value |> Map.values() |> Enum.any?(&is_nil/1) do
          {:ok, :maybe}
        else
          query
          |> Ash.Query.do_filter(pkey_value)
          |> Ash.Query.set_tenant(tenant)
          |> Ash.Query.data_layer_query()
          |> case do
            {:ok, data_layer_query} ->
              data_layer_query
              |> Ash.DataLayer.run_query(resource)
              |> Ash.Actions.Helpers.rollback_if_in_transaction(query.resource, query)
              |> case do
                {:ok, results} ->
                  case Ash.Actions.Read.run_authorize_results(query, results) do
                    {:ok, []} ->
                      if opts[:return_forbidden_error?] do
                        {:ok, false, authorizer_exception(authorizers)}
                      else
                        {:ok, false}
                      end

                    {:ok, [_]} ->
                      {:ok, true}

                    {:error, error} ->
                      if opts[:return_forbidden_error?] do
                        {:ok, false, error}
                      else
                        {:ok, false}
                      end
                  end

                {:error, error} ->
                  {:error, error}

                _ ->
                  if opts[:return_forbidden_error?] do
                    {:ok, false, authorizer_exception(authorizers)}
                  else
                    {:ok, false}
                  end
              end
          end
        end

      %Ash.Changeset{} ->
        if opts[:return_forbidden_error?] do
          {:ok, false, authorizer_exception(authorizers)}
        else
          {:ok, false}
        end
    end
  end

  defp or_query(query, resource, api) do
    query || Ash.Query.new(resource, api)
  end

  defp authorizer_exception([{authorizer, authorizer_state, _context}]) do
    Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state)
  end

  defp authorizer_exception(authorizers) do
    authorizers
    |> Enum.map(&authorizer_exception([&1]))
    |> Ash.Error.to_error_class()
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
      The actor for handling `^actor/1` templates, supplied to calculation context.
      """
    ],
    tenant: [
      type: :any,
      doc: """
      The tenant, supplied to calculation context.
      """
    ],
    authorize?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the request is being authorized, provided to calculation context.
      """
    ],
    tracer: [
      type: {:wrap_list, {:behaviour, Ash.Tracer}},
      doc: """
      A tracer, provided to the calculation context.
      """
    ],
    record: [
      type: :any,
      doc: """
      A record to use as the base of the calculation
      """
    ]
  ]

  @run_action_opts [
    actor: [
      type: :any,
      doc: """
      The actor for handling `^actor/1` templates, supplied to calculation context.
      """
    ],
    tenant: [
      type: :any,
      doc: """
      The tenant, supplied to calculation context.
      """
    ],
    authorize?: [
      type: :boolean,
      doc: """
      Whether or not the request should be authorized.
      """
    ],
    tracer: [
      type: {:wrap_list, {:behaviour, Ash.Tracer}},
      doc: """
      A tracer, provided to the calculation context.
      """
    ]
  ]

  @spec run_action!(api :: Ash.Api.t(), input :: Ash.ActionInput.t(), opts :: Keyword.t()) ::
          term | no_return
  def run_action!(api, input, opts \\ []) do
    api
    |> run_action(input, opts)
    |> unwrap_or_raise!()
  end

  @doc """
  Runs a generic action.

  Options:

  #{Spark.OptionsHelpers.docs(@run_action_opts)}
  """
  @spec run_action(api :: Ash.Api.t(), input :: Ash.ActionInput.t(), opts :: Keyword.t()) ::
          {:ok, term} | {:error, Ash.Error.t()}
  def run_action(api, input, opts \\ []) do
    case Spark.OptionsHelpers.validate(opts, @run_action_opts) do
      {:ok, opts} ->
        input = %{input | api: api}

        Ash.Actions.Action.run(api, input, opts)

      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  def calculate_opts, do: @calculate_opts

  @doc """
  Evaluates the calculation on the resource.

  If a record is provided, its field values will be used to evaluate the calculation.

  #{Spark.OptionsHelpers.docs(@calculate_opts)}
  """
  def calculate(resource_or_record, calculation, opts \\ []) do
    {resource, record} =
      case resource_or_record do
        %resource{} = record -> {resource, record}
        resource -> {resource, opts[:record]}
      end

    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, Ash.Api.calculate_opts()),
         {:calc,
          %{
            arguments: arguments,
            calculation: {module, calc_opts},
            type: type,
            constraints: constraints
          }} <-
           {:calc, Ash.Resource.Info.calculation(resource, calculation)},
         record <- struct(record || resource, opts[:refs] || %{}) do
      calc_context =
        opts[:context]
        |> Kernel.||(%{})
        |> Map.merge(%{
          actor: opts[:actor],
          tenant: opts[:tenant],
          authorize?: opts[:authorize?],
          tracer: opts[:tracer],
          resource: resource
        })
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
        |> Map.put(:api, opts[:api])

      Code.ensure_compiled!(module)

      if function_exported?(module, :expression, 2) do
        expr =
          case module.expression(calc_opts, calc_context) do
            {:ok, result} -> {:ok, result}
            {:error, error} -> {:error, error}
            result -> {:ok, result}
          end

        with {:ok, expr} <- expr do
          case Ash.Expr.eval(expr, record: record, resource: resource) do
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

  @callback aggregate(
              Ash.Query.t(),
              Ash.Api.aggregate() | list(Ash.Api.aggregate()),
              opts :: Keyword.t()
            ) ::
              {:ok, any} | {:error, Ash.Error.t()}

  @callback aggregate!(
              Ash.Query.t(),
              Ash.Api.aggregate() | list(Ash.Api.aggregate()),
              opts :: Keyword.t()
            ) ::
              any | no_return
  @callback count(Ash.Query.t(), opts :: Keyword.t()) :: {:ok, integer} | {:error, Ash.Error.t()}
  @callback count!(Ash.Query.t(), opts :: Keyword.t()) :: integer | no_return
  @doc "Get the first of a given field from the given query"
  @callback first(Ash.Query.t(), field :: atom, opts :: Keyword.t()) ::
              {:ok, term} | {:error, Ash.Error.t()}
  @doc "Get the first of a given field from the given query, raising any errors"
  @callback first!(Ash.Query.t(), field :: atom, opts :: Keyword.t()) :: term | no_return
  @doc "Get the sum of a given field from the given query"
  @callback sum(Ash.Query.t(), field :: atom, opts :: Keyword.t()) ::
              {:ok, term} | {:error, Ash.Error.t()}
  @doc "Get the sum of a given field from the given query, raising any errors"
  @callback sum!(Ash.Query.t(), field :: atom, opts :: Keyword.t()) :: term | no_return
  @doc "Get the min of a given field from the given query"
  @callback min(Ash.Query.t(), field :: atom, opts :: Keyword.t()) ::
              {:ok, term} | {:error, Ash.Error.t()}
  @doc "Get the min of a given field from the given query, raising any errors"
  @callback min!(Ash.Query.t(), field :: atom, opts :: Keyword.t()) :: term | no_return
  @doc "Get the max of a given field from the given query"
  @callback max(Ash.Query.t(), field :: atom, opts :: Keyword.t()) ::
              {:ok, term} | {:error, Ash.Error.t()}
  @doc "Get the max of a given field from the given query, raising any errors"
  @callback max!(Ash.Query.t(), field :: atom, opts :: Keyword.t()) :: term | no_return
  @doc "Get the avg of a given field from the given query"
  @callback avg(Ash.Query.t(), field :: atom, opts :: Keyword.t()) ::
              {:ok, term} | {:error, Ash.Error.t()}
  @doc "Get the avg of a given field from the given query, raising any errors"
  @callback avg!(Ash.Query.t(), field :: atom, opts :: Keyword.t()) :: term | no_return
  @doc "Whether or not the given query would return any results"
  @callback exists(Ash.Query.t(), opts :: Keyword.t()) ::
              {:ok, boolean} | {:error, Ash.Error.t()}
  @doc "Whether or not the given query would return any results, raising any errors"
  @callback exists?(Ash.Query.t(), opts :: Keyword.t()) ::
              boolean | no_return
  @doc "Get list of a given field from the given query"
  @callback list(Ash.Query.t(), field :: atom, opts :: Keyword.t()) ::
              {:ok, list(term)} | {:error, Ash.Error.t()}

  @doc "Get the list of a given field from the given query, raising any errors"
  @callback list!(Ash.Query.t(), field :: atom, opts :: Keyword.t()) :: list(term) | no_return

  @doc """
  Returns whether or not the user can perform the action, or raises on errors.

  See `can/3` for more info.
  """

  @callback can?(
              query_or_changeset_or_action ::
                Ash.Query.t()
                | Ash.Changeset.t()
                | Ash.ActionInput.t()
                | {Ash.Resource.t(), atom | Ash.Resource.Actions.action()}
                | {Ash.Resource.t(), atom | Ash.Resource.Actions.action(), input :: map},
              actor :: term,
              opts :: Keyword.t()
            ) ::
              boolean | no_return

  @doc """
  Returns whether or not the user can perform the action, or `:maybe`, returning any errors.

  In cases with "runtime" checks (checks after the action), we may not be able to determine
  an answer, and so the value `:maybe` will be returned from `can/2`. The `can?` function assumes that
  `:maybe` means `true`. Keep in mind, this is just for doing things like "can they do this" in a UI,
  so assuming `:maybe` is `true` is fine. The actual action invocation will be properly checked regardless.
  If you have runtime checks, you may need to use `can` instead of `can?`, or configure what `:maybe` means.

  ## Options

    - `maybe_is` - What to treat `:maybe` results as, defaults to `true` if using `can?`, or `:maybe` if using `can`.
    - `run_queries?` - In order to determine authorization status for changesets that use filter checks, we may need to
      run queries (almost always only one query). Set this to `false` to disable (returning `:maybe` instead).
      The default value is `true`.
    - `data` - A record or list of records. For authorizing reads with filter checks, this can be provided and a filter
      check will only be `true` if all records match the filter. This is detected by running a query.
    - `alter_source?` - If true, the query or changeset will be returned with authorization modifications made. For a query,
      this mans adding field visibility calculations and altering the filter or the sort. For a changeset, this means only adding
      field visibility calculations. The default value is `false`.
    - `base_query` - If authorizing an update, some cases can return both a new changeset and a query filtered for only things
      that will be authorized to update. Providing the `base_query` will cause that query to be altered instead of a new one to be
      generated.
  """

  @callback can(
              action_or_query_or_changeset ::
                Ash.Query.t()
                | Ash.Changeset.t()
                | Ash.ActionInput.t()
                | {Ash.Resource.t(), atom | Ash.Resource.Actions.action()}
                | {Ash.Resource.t(), atom | Ash.Resource.Actions.action(), input :: map},
              actor :: term,
              opts :: Keyword.t()
            ) ::
              {:ok, boolean | :maybe}
              | {:ok, true, Ash.Changeset.t() | Ash.Query.t()}
              | {:ok, true, Ash.Changeset.t(), Ash.Query.t()}
              | {:ok, false, Exception.t()}
              | {:error, term}

  @callback calculate(resource :: Ash.Resource.t(), calculation :: atom, opts :: Keyword.t()) ::
              {:ok, term} | {:error, term}

  @callback calculate!(resource :: Ash.Resource.t(), calculation :: atom, opts :: Keyword.t()) ::
              term | no_return

  @doc "Runs a generic action, raising on errors"
  @callback run_action!(input :: Ash.ActionInput.t(), opts :: Keyword.t()) ::
              term | no_return

  @doc "Runs a generic action"
  @callback run_action(input :: Ash.ActionInput.t(), opts :: Keyword.t()) ::
              {:ok, term} | {:error, term}

  @doc """
  Get a record by a primary key. See `c:get/3` for more.
  """
  @callback get!(
              resource :: Ash.Resource.t(),
              id_or_filter :: term(),
              opts :: Keyword.t()
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
  @callback read_one!(Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
              Ash.Resource.record() | {Ash.Resource.record(), Ash.Query.t()} | nil | no_return

  @doc """
  Run a query on a resource, but fail on more than one result.

  This is useful if you have a query that doesn't include a primary key
  but you know that it will only ever return a single result.

  ## Options

  #{Spark.OptionsHelpers.docs(@read_one_opts_schema)}
  """
  @callback read_one(Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), Ash.Query.t()}
              | {:ok, nil}
              | {:error, term}
  @doc """
  Run an ash query. See `c:read/2` for more.
  """
  @callback read!(Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
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
  @callback read(Ash.Query.t(), opts :: Keyword.t()) ::
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
  @callback create!(Ash.Changeset.t(), opts :: Keyword.t()) ::
              Ash.Resource.record()
              | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | no_return

  @doc """
  Create a record.

  #{Spark.OptionsHelpers.docs(@create_opts_schema)}
  """
  @callback create(Ash.Changeset.t(), opts :: Keyword.t()) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

  @doc """
  Creates many records.

  ## Assumptions

  We assume that the input is a list of changesets all for the same action, or a list of input maps for the
  same action with the `:resource` and `:action` option provided to illustrate which action it is for.

  ## Performance/Feasibility

  The performance of this operation depends on the data layer in question.
  Data layers like AshPostgres will choose reasonable batch sizes in an attempt
  to handle large bulk actions, but that does not mean that you can pass a list of
  500k inputs and expect things to go off without a hitch (although it might).
  If you need to do large data processing, you should look into projects like
  GenStage and Broadway. With that said, if you want to do things like support CSV upload
  and you place some reasonable limits on the size this is a great tool. You'll need to
  test it yourself, YMMV.

  Passing `return_records?: true` can significantly increase the time it takes to perform the operation,
  and can also make the operation completely unreasonable due to the memory requirement. If you want to
  do very large bulk creates and display all of the results, the suggestion is to annotate them with a
  "bulk_create_id" in the data layer, and then read the records with that `bulk_create_id` so that they can
  be retrieved later if necessary.

  ## Changes/Validations

  Changes will be applied in the order they are given on the actions as normal. Any change that exposes
  the `bulk_change` or `bulk_validate` callback will be applied on the entire list.

  ## After Action Hooks

  The following requirements must be met for `after_action` hooks to function properly. If they are not met,
  and an after_action hook being applied to a changeset in a `change`.

  1. `return_records?` must be set to `true`.
  2. The changeset must be setting the primary key as part of its changes, so that we know which result applies to which
     changeset.

  It is possible to use `after_action` hooks with `bulk_change/3`, but you need to return the hooks along with the changesets.
  This allows for setting up `after_action` hooks that don't need access to the returned record,
  or `after_action` hooks that can operate on the entire list at once.  See the documentation for that callback for more on
  how to do accomplish that.

  ## Options

  #{Spark.OptionsHelpers.docs(@bulk_create_opts_schema)}
  """
  @callback bulk_create(
              [map],
              resource :: Ash.Resource.t(),
              action :: atom,
              opts :: Keyword.t()
            ) ::
              Ash.BulkResult.t()
              | Enumerable.t(
                  {:ok, Ash.Resource.record()}
                  | {:error, Ash.Changeset.t() | Ash.Error.t()}
                  | {:notification, Ash.Notifier.Notification.t()}
                )

  @doc """
  Updates all items in the provided enumerable or query with the provided input. See `c:bulk_update/4` for more.
  """
  @callback bulk_update!(
              Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
              atom,
              input :: map,
              Keyword.t()
            ) ::
              Ash.BulkResult.t() | no_return

  @doc """
  Updates all items in the provided enumerable or query with the provided input.

  If the data layer supports updating from a query, and the update action can be done fully atomically,
  it will be updated in a single pass using the data layer.

  Otherwise, this will stream each record and update it.

  ## Options

  #{Spark.OptionsHelpers.docs(@bulk_update_opts_schema)}
  """
  @callback bulk_update(
              Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
              atom,
              input :: map,
              Keyword.t()
            ) ::
              Ash.BulkResult.t()

  @doc """
  Destroys all items in the provided enumerable or query with the provided input. See `c:bulk_destroy/4` for more.
  """
  @callback bulk_destroy!(
              Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
              atom,
              input :: map,
              Keyword.t()
            ) ::
              Ash.BulkResult.t() | no_return

  @doc """
  Destroys all items in the provided enumerable or query with the provided input.

  If the data layer supports destroying from a query, and the destroy action can be done fully atomically,
  it will be updated in a single pass using the data layer.

  Otherwise, this will stream each record and update it.

  ## Options

  #{Spark.OptionsHelpers.docs(@bulk_destroy_opts_schema)}
  """
  @callback bulk_destroy(
              Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
              atom,
              input :: map,
              Keyword.t()
            ) ::
              Ash.BulkResult.t()

  @doc """
  Creates many records, raising on any errors. See `bulk_create/2` for more.

  #{Spark.OptionsHelpers.docs(@bulk_create_opts_schema)}
  """
  @callback bulk_create!(
              [map],
              resource :: Ash.Resource.t(),
              action :: atom,
              opts :: Keyword.t()
            ) ::
              Ash.BulkResult.t() | no_return()

  @doc """
  Update a record. See `c:update/2` for more information.
  """
  @callback update!(Ash.Changeset.t(), opts :: Keyword.t()) ::
              Ash.Resource.record()
              | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | no_return

  @doc """
  Update a record.

  #{Spark.OptionsHelpers.docs(@update_opts_schema)}
  """
  @callback update(Ash.Changeset.t(), opts :: Keyword.t()) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

  @doc """
  Destroy a record. See `c:destroy/2` for more information.
  """
  @callback destroy!(Ash.Changeset.t() | Ash.Resource.record(), opts :: Keyword.t()) ::
              :ok
              | Ash.Resource.record()
              | list(Ash.Notifier.Notification.t())
              | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | no_return

  @doc """
  Destroy a record.

  #{Spark.OptionsHelpers.docs(@destroy_opts_schema)}
  """
  @callback destroy(Ash.Changeset.t() | Ash.Resource.record(), opts :: Keyword.t()) ::
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

  @impl true
  def verify(module, opts) do
    if Application.get_env(:ash, :validate_api_config_inclusion?, true) &&
         Keyword.get(opts, :validate_config_inclusion?, true) do
      otp_app = Mix.Project.config()[:app]

      apis =
        Application.get_env(otp_app, :ash_apis, [])

      if module not in apis do
        IO.warn("""
        Api #{inspect(module)} is not present in

            config :#{otp_app}, ash_apis: #{inspect(apis)}.


        To resolve this warning, do one of the following.

        1. Add the api to your configured api modules. The following snippet can be used.

            config :#{otp_app}, ash_apis: #{inspect(apis ++ [module])}

        2. Add the option `validate_config_inclusion?: false` to `use Ash.Api`

        3. Configure all apis not to warn, with `config :ash, :validate_api_config_inclusion?, false`
        """)
      end
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
    |> unwrap_or_raise!()
  end

  @doc false
  @spec get(Ash.Api.t(), Ash.Resource.t(), term(), Keyword.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def get(api, resource, id, opts) do
    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @get_opts_schema),
         {:ok, resource} <- Ash.Api.Info.resource(api, resource),
         {:ok, filter} <- Ash.Filter.get_filter(resource, id),
         {:ok, read_opts} <-
           Spark.OptionsHelpers.validate(
             Keyword.take(opts, Keyword.keys(@read_opts_schema)),
             @read_opts_schema
           ) do
      query =
        resource
        |> Ash.Query.new(api)
        |> Ash.Query.set_tenant(opts[:tenant])
        |> Ash.Query.filter(^filter)
        |> Ash.Query.load(opts[:load] || [])
        |> Ash.Query.set_context(opts[:context] || %{})
        |> Ash.Query.lock(opts[:lock])

      query =
        if Ash.DataLayer.data_layer_can?(query.resource, :limit) do
          Ash.Query.limit(query, 2)
        else
          query
        end

      query
      |> Ash.Actions.Read.unpaginated_read(opts[:action] || query.action, read_opts)
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
    api
    |> page(keyset, request)
    |> unwrap_or_raise!()
  end

  @doc false
  def page(_, %Ash.Page.Keyset{results: []} = page, :next) do
    {:ok, page}
  end

  def page(_, %Ash.Page.Keyset{before: nil, after: nil} = page, :prev) do
    {:ok, page}
  end

  def page(api, %Ash.Page.Keyset{results: [], before: before, rerun: {query, opts}}, :prev)
      when not is_nil(before) do
    new_page_opts =
      opts[:page]
      |> Keyword.delete(:before)
      |> Keyword.put(:after, before)

    read(api, query, Keyword.put(opts, :page, new_page_opts))
  end

  def page(_, %Ash.Page.Keyset{}, n) when is_integer(n) do
    {:error, "Cannot seek to a specific page with keyset based pagination"}
  end

  def page(
        api,
        %Ash.Page.Keyset{results: results, rerun: {query, opts}} = page,
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

    case read(api, query, Keyword.put(opts, :page, new_page_opts)) do
      {:ok, %{results: []}} ->
        {:ok, page}

      other ->
        other
    end
  end

  def page(api, %Ash.Page.Keyset{results: results, rerun: {query, opts}} = page, :prev) do
    first_keyset =
      results
      |> List.first()
      |> Map.get(:__metadata__)
      |> Map.get(:keyset)

    new_page_opts =
      opts[:page]
      |> Keyword.put(:before, first_keyset)
      |> Keyword.delete(:after)

    case read(api, query, Keyword.put(opts, :page, new_page_opts)) do
      {:ok, %{results: []}} ->
        {:ok, page}

      other ->
        other
    end
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

  def page(api, %Ash.Page.Keyset{rerun: {query, opts}}, :self) do
    read(api, query, opts)
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

        :self ->
          opts[:page]

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
    |> unwrap_or_raise!()
  end

  @doc false
  def load(api, data, query, opts \\ [])
  def load(_, [], _, _), do: {:ok, []}
  def load(_, nil, _, _), do: {:ok, nil}
  def load(_, {:error, error}, _, _), do: {:error, error}

  def load(api, {:ok, values}, query, opts) do
    resource = resource_from_data!(values, query, opts)
    load(api, values, query, Keyword.put(opts, :resource, resource))
  end

  def load(api, %struct{results: results} = page, query, opts)
      when struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    resource = resource_from_data!(page, query, opts)

    api
    |> load(results, query, Keyword.put(opts, :resource, resource))
    |> case do
      {:ok, results} -> {:ok, %{page | results: results}}
      {:error, error} -> {:error, error}
    end
  end

  def load(api, data, query, opts) when not is_list(data) do
    resource = resource_from_data!(data, query, opts)

    api
    |> load(List.wrap(data), query, Keyword.put(opts, :resource, resource))
    |> case do
      {:ok, data} -> {:ok, Enum.at(data, 0)}
      {:error, error} -> {:error, error}
    end
  end

  def load(api, [record | _] = data, query, opts) do
    resource = resource_from_data!(data, query, opts)
    opts = Keyword.delete(opts, :resource)

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

  defp resource_from_data!(data, query, opts) do
    if opts[:resource] do
      opts[:resource]
    else
      case query do
        %Ash.Query{resource: resource} -> resource
        _ -> do_resource_from_data!(data)
      end
    end
  end

  defp do_resource_from_data!(%struct{rerun: {%Ash.Query{resource: resource}, _}})
       when struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    resource
  end

  defp do_resource_from_data!(%struct{results: [%resource{} | _]} = data)
       when struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    if Ash.Resource.Info.resource?(resource) do
      resource
    else
      raise_no_resource_error!(data)
    end
  end

  defp do_resource_from_data!(%resource{} = data) do
    if Ash.Resource.Info.resource?(resource) do
      resource
    else
      raise_no_resource_error!(data)
    end
  end

  defp do_resource_from_data!([%resource{} | _] = data) do
    if Ash.Resource.Info.resource?(resource) do
      resource
    else
      raise_no_resource_error!(data)
    end
  end

  defp do_resource_from_data!(data) do
    raise_no_resource_error!(data)
  end

  defp raise_no_resource_error!(data) do
    raise ArgumentError,
      message: """
      Could not determine a resource from the provided input:

      #{inspect(data)}
      """
  end

  @spec stream!(api :: module(), query :: Ash.Query.t(), opts :: Keyword.t()) ::
          Enumerable.t(Ash.Resource.record())
  def stream!(api, query, opts \\ []) do
    opts = Spark.OptionsHelpers.validate!(opts, @stream_opts)

    Ash.Actions.Read.Stream.run!(api, query, opts)
  end

  @doc false
  @spec read!(Ash.Api.t(), Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          list(Ash.Resource.record()) | Ash.Page.page() | no_return
  def read!(api, query, opts \\ []) do
    opts = Spark.OptionsHelpers.validate!(opts, @read_opts_schema)

    api
    |> read(query, opts)
    |> unwrap_or_raise!()
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

    query =
      if opts[:lock] do
        Ash.Query.lock(query, opts[:lock])
      else
        query
      end

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
    |> unwrap_or_raise!()
  end

  @doc false
  def read_one(api, query, opts) do
    query = Ash.Query.to_query(query)
    query = Ash.Query.set_api(query, api)

    with {:ok, opts} <- Spark.OptionsHelpers.validate(opts, @read_one_opts_schema),
         {:ok, action} <- get_action(query.resource, opts, :read, query.action),
         {:ok, action} <- pagination_check(action, query.resource, opts) do
      query
      |> Ash.Actions.Read.unpaginated_read(action, opts)
      |> unwrap_one()
    else
      {:error, error} ->
        {:error, error}
    end
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
    api
    |> create(changeset, opts)
    |> unwrap_or_raise!()
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
  @spec bulk_create!(Ash.Api.t(), Enumerable.t(map), Ash.Resource.t(), atom, Keyword.t()) ::
          Ash.BulkResult.t() | no_return
  def bulk_create!(api, inputs, resource, action, opts) do
    api
    |> bulk_create(inputs, resource, action, opts)
    |> case do
      %Ash.BulkResult{status: :error, errors: errors} when errors in [nil, []] ->
        if opts[:return_errors?] do
          raise Ash.Error.to_error_class(
                  Ash.Error.Unknown.UnknownError.exception(
                    error: "Something went wrong with bulk create, but no errors were produced."
                  )
                )
        else
          raise Ash.Error.to_error_class(
                  Ash.Error.Unknown.UnknownError.exception(
                    error:
                      "Something went wrong with bulk create, but no errors were produced due to `return_errors?` being set to `false`."
                  )
                )
        end

      %Ash.BulkResult{status: :error, errors: errors} ->
        raise Ash.Error.to_error_class(errors)

      bulk_result ->
        bulk_result
    end
  end

  @doc false
  @spec bulk_create(Ash.Api.t(), Enumerable.t(map), Ash.Resource.t(), atom, Keyword.t()) ::
          Ash.BulkResult.t()
  def bulk_create(api, inputs, resource, action, opts) do
    case inputs do
      [] ->
        result = %Ash.BulkResult{status: :success, errors: []}

        result =
          if opts[:return_records?] do
            %{result | records: []}
          else
            result
          end

        if opts[:return_notifications?] do
          %{result | notifications: []}
        else
          result
        end

      inputs ->
        case Spark.OptionsHelpers.validate(opts, @bulk_create_opts_schema) do
          {:ok, opts} ->
            Create.Bulk.run(api, resource, action, inputs, opts)

          {:error, error} ->
            %Ash.BulkResult{status: :error, errors: [Ash.Error.to_ash_error(error)]}
        end
    end
  end

  @doc false
  @spec bulk_update!(
          Ash.Api.t(),
          Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
          atom,
          input :: map,
          Keyword.t()
        ) ::
          Ash.BulkResult.t() | no_return
  def bulk_update!(api, stream_or_query, action, input, opts) do
    api
    |> bulk_update(stream_or_query, action, input, opts)
    |> case do
      %Ash.BulkResult{status: :error, errors: errors} when errors in [nil, []] ->
        if opts[:return_errors?] do
          raise Ash.Error.to_error_class(
                  Ash.Error.Unknown.UnknownError.exception(
                    error: "Something went wrong with bulk update, but no errors were produced."
                  )
                )
        else
          raise Ash.Error.to_error_class(
                  Ash.Error.Unknown.UnknownError.exception(
                    error:
                      "Something went wrong with bulk update, but no errors were produced due to `return_errors?` being set to `false`."
                  )
                )
        end

      %Ash.BulkResult{status: :error, errors: errors} ->
        raise Ash.Error.to_error_class(errors)

      bulk_result ->
        bulk_result
    end
  end

  @doc false
  @spec bulk_update(
          Ash.Api.t(),
          Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
          atom,
          input :: map,
          Keyword.t()
        ) ::
          Ash.BulkResult.t()
  def bulk_update(api, query_or_stream, action, input, opts) do
    case query_or_stream do
      [] ->
        result = %Ash.BulkResult{status: :success, errors: []}

        result =
          if opts[:return_records?] do
            %{result | records: []}
          else
            result
          end

        if opts[:return_notifications?] do
          %{result | notifications: []}
        else
          result
        end

      query_or_stream ->
        case Spark.OptionsHelpers.validate(opts, @bulk_update_opts_schema) do
          {:ok, opts} ->
            Update.Bulk.run(api, query_or_stream, action, input, opts)

          {:error, error} ->
            %Ash.BulkResult{status: :error, errors: [Ash.Error.to_ash_error(error)]}
        end
    end
  end

  @doc false
  @spec bulk_destroy!(
          Ash.Api.t(),
          Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
          atom,
          input :: map,
          Keyword.t()
        ) ::
          Ash.BulkResult.t() | no_return
  def bulk_destroy!(api, stream_or_query, action, input, opts) do
    api
    |> bulk_destroy(stream_or_query, action, input, opts)
    |> case do
      %Ash.BulkResult{status: :error, errors: errors} when errors in [nil, []] ->
        if opts[:return_errors?] do
          raise Ash.Error.to_error_class(
                  Ash.Error.Unknown.UnknownError.exception(
                    error: "Something went wrong with bulk update, but no errors were produced."
                  )
                )
        else
          raise Ash.Error.to_error_class(
                  Ash.Error.Unknown.UnknownError.exception(
                    error:
                      "Something went wrong with bulk update, but no errors were produced due to `return_errors?` being set to `false`."
                  )
                )
        end

      %Ash.BulkResult{status: :error, errors: errors} ->
        raise Ash.Error.to_error_class(errors)

      %Ash.BulkResult{} = bulk_result ->
        bulk_result
    end
  end

  @doc false
  @spec bulk_destroy(
          Ash.Api.t(),
          Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
          atom,
          input :: map,
          Keyword.t()
        ) ::
          Ash.BulkResult.t()
  def bulk_destroy(api, query_or_stream, action, input, opts) do
    case query_or_stream do
      [] ->
        result = %Ash.BulkResult{status: :success, errors: []}

        result =
          if opts[:return_records?] do
            %{result | records: []}
          else
            result
          end

        if opts[:return_notifications?] do
          %{result | notifications: []}
        else
          result
        end

      query_or_stream ->
        case Spark.OptionsHelpers.validate(opts, @bulk_destroy_opts_schema) do
          {:ok, opts} ->
            %Ash.BulkResult{} =
              result = Destroy.Bulk.run(api, query_or_stream, action, input, opts)

            result

          {:error, error} ->
            %Ash.BulkResult{status: :error, errors: [Ash.Error.to_ash_error(error)]}
        end
    end
  end

  @doc false
  def update!(api, changeset, opts) do
    opts = Spark.OptionsHelpers.validate!(opts, @update_opts_schema)

    api
    |> update(changeset, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec update(Ash.Api.t(), Ash.Changeset.t(), Keyword.t()) ::
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
    |> unwrap_or_raise!(!(opts[:return_notifications?] || opts[:return_destroyed?]))
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

  defp unwrap_or_raise!(first, destroy? \\ false)
  defp unwrap_or_raise!(%Ash.BulkResult{} = bulk_result, _), do: bulk_result
  defp unwrap_or_raise!(:ok, _), do: :ok
  defp unwrap_or_raise!({:ok, result}, false), do: result
  defp unwrap_or_raise!({:ok, _result}, true), do: :ok
  defp unwrap_or_raise!({:ok, result, other}, _), do: {result, other}

  defp unwrap_or_raise!({:error, error}, destroy?) when is_list(error) do
    unwrap_or_raise!({:error, Ash.Error.to_error_class(error)}, destroy?)
  end

  defp unwrap_or_raise!({:error, error}, _) do
    exception = Ash.Error.to_error_class(error)

    case exception do
      %{stacktrace: %{stacktrace: stacktrace}} = exception ->
        reraise exception, stacktrace

      _ ->
        raise exception
    end
  end

  @impl Spark.Dsl
  def explain(dsl_state, _opts) do
    Ash.Api.Info.description(dsl_state)
  end
end
