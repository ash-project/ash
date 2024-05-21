defmodule Ash do
  @moduledoc """
  The primary interface to call actions and interact with resources.
  """

  require Ash.Query
  require Ash.Helpers

  @type page_request ::
          :next | :prev | :first | :last | :self | integer

  @type aggregate ::
          Ash.Query.Aggregate.t()
          | {name :: atom, kind :: atom}
          | {name :: atom, kind :: atom, opts :: Keyword.t()}

  @type load_statement ::
          Ash.Query.t()
          | [atom]
          | atom
          | Keyword.t()
          | list(atom | {atom, atom | Keyword.t()})

  @type resource_with_args :: {Ash.Resource.t(), map() | Keyword.t()}

  @type record_with_args :: {Ash.Resource.record(), map() | Keyword.t()}

  @global_opts [
    domain: [
      type: {:spark, Ash.Domain},
      doc: "The domain to use."
    ],
    timeout: [
      type: :timeout,
      doc: """
      A positive integer, or `:infinity`. If none is provided, the timeout configured on the domain is used (which defaults to `30_000`).
      """
    ],
    tracer: [
      type: {:wrap_list, {:behaviour, Ash.Tracer}},
      doc: """
      A tracer that implements the `Ash.Tracer` behaviour. See that module for more.
      """
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
    context: [
      type: :map,
      doc: "Context to set on the query, changeset, or input"
    ],
    tenant: [
      type: {:protocol, Ash.ToTenant},
      doc: "A tenant to set on the query or changeset"
    ],
    actor: [
      type: :any,
      doc:
        "If an actor is provided, it will be used in conjunction with the authorizers of a resource to authorize access"
    ]
  ]

  @read_opts_schema Spark.Options.merge(
                      [
                        page: [
                          doc:
                            "Pagination options, see [the pagination docs for more](read-actions.md#pagination).",
                          type: {:custom, Ash.Page, :page_opts, []}
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
                        reuse_values?: [
                          type: :boolean,
                          default: false,
                          doc:
                            "Whether calculations are allowed to reuse values that have already been loaded, or must refetch them from the data layer."
                        ]
                      ],
                      @global_opts,
                      "Global Options"
                    )

  @doc false
  def read_opts, do: @read_opts_schema

  @read_one_opts_schema Spark.Options.merge(
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
                     "The 'worst' strategy allowed to be used to fetch records. See `Ash.stream!/2` docs for more.",
                   default: :keyset
                 ],
                 stream_with: [
                   type: {:one_of, [:keyset, :offset, :full_read]},
                   doc:
                     "The specific strategy to use to fetch records. See `Ash.stream!/2` docs for more."
                 ]
               ]
               |> Spark.Options.merge(
                 @read_opts_schema,
                 "Read Options"
               )

  @doc false
  def stream_opts, do: @stream_opts

  @load_opts_schema Spark.Options.merge(
                      [
                        lazy?: [
                          type: :boolean,
                          doc:
                            "If set to true, values will only be loaded if the related value isn't currently loaded.",
                          default: false
                        ],
                        reuse_values?: [
                          type: :boolean,
                          default: false,
                          doc:
                            "Whether calculations are allowed to reuse values that have already been loaded, or must refetch them from the data layer."
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
                       type: {:protocol, Ash.ToTenant},
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
                     reuse_values?: [
                       type: :boolean,
                       default: false,
                       doc:
                         "Whether calculations are allowed to reuse values that have already been loaded, or must refetch them from the data layer."
                     ]
                   ]
                   |> Spark.Options.merge(@global_opts, "Global Options")

  @shared_created_update_and_destroy_opts_schema [
    return_notifications?: [
      type: :boolean,
      default: false,
      doc: """
      Use this if you're running ash actions in your own transaction and you want to manually handle sending notifications.

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
    ],
    load: [
      type: :any,
      doc: "A load statement to add onto the changeset"
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
                      |> Spark.Options.merge(@global_opts, "Global Options")
                      |> Spark.Options.merge(
                        @shared_created_update_and_destroy_opts_schema,
                        "Shared create/update/destroy Options"
                      )

  @doc false
  def create_opts, do: @create_opts_schema

  @shared_bulk_opts_schema [
    read_action: [
      type: :atom,
      doc: "The action to use when building the read query."
    ],
    assume_casted?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not to cast attributes and arguments as input. This is an optimization for cases where the input is already casted and/or not in need of casting"
    ],
    load: [
      type: :any,
      doc: "A load statement to apply to records. Ignored if `return_records?` is not true."
    ],
    select: [
      type: {:list, :atom},
      doc: "A select statement to apply to records. Ignored if `return_records?` is not true."
    ],
    authorize_query_with: [
      type: {:one_of, [:filter, :error]},
      default: :filter,
      doc:
        "If set to `:error`, instead of filtering unauthorized query results, unauthorized query results will raise an appropriate forbidden error"
    ],
    authorize_changeset_with: [
      type: {:one_of, [:filter, :error]},
      default: :filter,
      doc:
        "If set to `:error`, instead of filtering unauthorized changes, unauthorized changes will raise an appropriate forbidden error"
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
      Whether or not to generate any notifications. If this is set to `true` then the data layer must return
      the results from each batch. This may be intensive for large bulk actions.

      Notifications will be automatically sent unless `return_notifications?` is set to `true`.
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
    ],
    skip_unknown_inputs: [
      type: {:list, {:or, [:atom, :string]}},
      doc:
        "A list of inputs that, if provided, will be ignored if they are not recognized by the action."
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
                             ],
                             allow_stream_with: [
                               type: {:one_of, [:keyset, :offset, :full_read]},
                               doc:
                                 "The 'worst' strategy allowed to be used to fetch records if the `:stream` strategy is chosen. See the `Ash.stream!/2` docs for more.",
                               default: :keyset
                             ],
                             authorize_query?: [
                               type: :boolean,
                               default: true,
                               doc:
                                 "If a query is given, determines whether or not authorization is run on that query."
                             ],
                             select: [
                               type: {:list, :atom},
                               doc:
                                 "A select statement to apply to records. Ignored if `return_records?` is not true."
                             ],
                             filter: [
                               type: :any,
                               doc:
                                 "A filter to apply to records. This is also applied to a stream of inputs."
                             ],
                             strategy: [
                               type: {:wrap_list, {:one_of, [:atomic, :atomic_batches, :stream]}},
                               default: [:atomic],
                               doc:
                                 "The strategy or strategies to enable. :stream is used in all cases if the data layer does not support atomics."
                             ],
                             skip_unknown_inputs: [
                               type: {:list, {:or, [:atom, :string]}},
                               doc:
                                 "A list of inputs that, if provided, will be ignored if they are not recognized by the action."
                             ],
                             load: [
                               type: :any,
                               doc: "A load statement to apply on the resulting records."
                             ]
                           ]
                           |> Spark.Options.merge(
                             Keyword.delete(@global_opts, :action),
                             "Global options"
                           )
                           |> Spark.Options.merge(
                             Keyword.delete(@stream_opts, :batch_size),
                             "Stream Options"
                           )
                           |> Spark.Options.merge(
                             @shared_created_update_and_destroy_opts_schema,
                             "Shared create/update/destroy options"
                           )
                           |> Spark.Options.merge(
                             @shared_bulk_opts_schema,
                             "Shared bulk options"
                           )

  @doc false
  def bulk_update_opts, do: @bulk_update_opts_schema

  @bulk_destroy_opts_schema [
                              resource: [
                                type: {:spark, Ash.Resource},
                                doc:
                                  "The resource being destroyed. This must be provided if the input given is a stream, so we know ahead of time what the resource being updated is."
                              ],
                              stream_batch_size: [
                                type: :integer,
                                doc:
                                  "Batch size to use if provided a query and the query must be streamed"
                              ],
                              allow_stream_with: [
                                type: {:one_of, [:keyset, :offset, :full_read]},
                                doc:
                                  "The 'worst' strategy allowed to be used to fetch records if the `:stream` strategy is chosen. See the `Ash.stream!/2` docs for more.",
                                default: :keyset
                              ],
                              authorize_query?: [
                                type: :boolean,
                                default: true,
                                doc:
                                  "If a query is given, determines whether or not authorization is run on that query."
                              ],
                              strategy: [
                                type:
                                  {:wrap_list, {:one_of, [:atomic, :atomic_batches, :stream]}},
                                default: :atomic,
                                doc:
                                  "The strategy or strategies to enable. :stream is used in all cases if the data layer does not support atomics."
                              ],
                              filter: [
                                type: :any,
                                doc:
                                  "A filter to apply to records. This is also applied to a stream of inputs."
                              ],
                              skip_unknown_inputs: [
                                type: {:list, {:or, [:atom, :string]}},
                                doc:
                                  "A list of inputs that, if provided, will be ignored if they are not recognized by the action."
                              ]
                            ]
                            |> Spark.Options.merge(
                              Keyword.delete(@global_opts, :action),
                              "Global options"
                            )
                            |> Spark.Options.merge(
                              Keyword.delete(@stream_opts, :batch_size),
                              "Stream Options"
                            )
                            |> Spark.Options.merge(
                              @shared_created_update_and_destroy_opts_schema,
                              "Shared create/update/destroy options"
                            )
                            |> Spark.Options.merge(
                              @shared_bulk_opts_schema,
                              "Shared bulk options"
                            )

  @doc false
  def bulk_destroy_opts, do: @bulk_destroy_opts_schema

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
                             select: [
                               type: {:list, :atom},
                               doc:
                                 "A select statement to apply to records. Ignored if `return_records?` is not true."
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
                             ],
                             skip_unknown_inputs: [
                               type: {:list, {:or, [:atom, :string]}},
                               doc:
                                 "A list of inputs that, if provided, will be ignored if they are not recognized by the action."
                             ]
                           ]
                           |> Spark.Options.merge(
                             Keyword.delete(@global_opts, :action),
                             "Global options"
                           )
                           |> Spark.Options.merge(
                             @shared_created_update_and_destroy_opts_schema,
                             "Shared create/update/destroy options"
                           )
                           |> Spark.Options.merge(
                             @shared_bulk_opts_schema,
                             "Shared bulk options"
                           )

  @doc false
  def bulk_create_opts, do: @bulk_create_opts_schema

  @update_opts_schema [
                        params: [
                          type: :map,
                          doc:
                            "Parameters to supply, ignored if the input is a changeset, only used when an identifier is given."
                        ],
                        atomic_upgrade?: [
                          type: :boolean,
                          default: true,
                          doc:
                            "If true the action will be done atomically if it can, ignoring the in memory transformations and validations. You should not generally need to disable this."
                        ]
                      ]
                      |> Spark.Options.merge(@global_opts, "Global Options")
                      |> Spark.Options.merge(
                        @shared_created_update_and_destroy_opts_schema,
                        "Shared create/update/destroy Options"
                      )

  @doc false
  def update_opts, do: @update_opts_schema

  @destroy_opts_schema [
                         return_destroyed?: [
                           type: :boolean,
                           default: false,
                           doc:
                             "If true, the destroyed record is included in the return result, e.g `{:ok, destroyed}` or `{:ok, destroyed, notifications}`"
                         ]
                       ]
                       |> Spark.Options.merge(@global_opts, "Global Opts")
                       |> Spark.Options.merge(
                         @shared_created_update_and_destroy_opts_schema,
                         "Shared create/update/destroy Options"
                       )

  @doc false
  def destroy_opts, do: @destroy_opts_schema

  @aggregate_opts [] |> Spark.Options.merge(@global_opts, "Global Options")

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
      type: {:protocol, Ash.ToTenant},
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
    ],
    domain: [
      type: {:spark, Ash.Domain},
      doc: "The domain to use for the action"
    ]
  ]

  def calculate_opts, do: @calculate_opts

  @run_action_opts [
    actor: [
      type: :any,
      doc: """
      The actor for handling `^actor/1` templates, supplied to calculation context.
      """
    ],
    tenant: [
      type: {:protocol, Ash.ToTenant},
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
    ],
    domain: [
      type: {:spark, Ash.Domain},
      doc: "The domain to use for the action"
    ]
  ]

  @doc false
  def run_action_opts, do: @run_action_opts

  @can_opts [
    maybe_is: [
      type: :any,
      doc: "If the actor *may* be able to perform the action, what value should be returned.",
      default: :maybe
    ],
    filter_with: [
      type: {:one_of, [:filter, :error]},
      default: :filter,
      doc:
        "If set to `:error`, the query will raise an error on a match. If set to `:filter` the query will filter out unauthorized access."
    ],
    pre_flight?: [
      type: :boolean,
      default: true,
      doc:
        "Whether or not this is a pre_flight check (which may perform optimized in-memory checks) or the final proper check."
    ],
    run_queries?: [
      type: :boolean,
      doc: "Whether or not to run queries. If set to `true`, `:maybe` will not be returned.",
      default: true
    ],
    data: [
      type: {:or, [:struct, {:list, :struct}]},
      doc: "The record or records specifically attempting to be acted upon."
    ],
    tenant: [
      type: {:protocol, Ash.ToTenant},
      doc: "The tenant to use for authorization"
    ],
    alter_source?: [
      type: :boolean,
      default: false,
      doc: "If set to `true`, the source being authorized is returned so it can be run."
    ],
    base_query: [
      type: :any,
      doc: "A base query on which to apply an generated filters"
    ],
    no_check?: [
      type: :boolean,
      doc:
        "Whether or not authorization must pass at the strict/filter step, or if post-checks are allowed to be run",
      default: false
    ],
    on_must_pass_strict_check: [
      type: :any,
      doc: "Override the value returned when `no_check?` is `true` but a check must be run."
    ],
    atomic_changeset: [
      type: :any,
      doc: "A base query on which to apply an generated filters"
    ],
    return_forbidden_error?: [
      type: :boolean,
      default: false,
      doc: "Whether or not to return a forbidden error in cases of not being authorized."
    ]
  ]

  @doc false
  def can_opts, do: @can_opts

  @can_question_mark_opts Spark.Options.Helpers.set_default!(@can_opts, :maybe_is, true)

  @doc false
  def can_question_mark_opts, do: @can_question_mark_opts

  @doc """
  Runs an aggregate or aggregates over a resource query. See `aggregate/3` for more.
  """
  @spec aggregate!(
          Ash.Query.t() | Ash.Resource.t(),
          aggregates :: aggregate | list(aggregate),
          opts :: Keyword.t()
        ) ::
          term | no_return
  @doc spark_opts: [{2, @aggregate_opts}]
  def aggregate!(query, aggregate_or_aggregates, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> aggregate(aggregate_or_aggregates, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Runs an aggregate or aggregates over a resource query

  If you pass an `%Ash.Query.Aggregate{}`, gotten from `Ash.Query.Aggregate.new()`,
  the query provided as the first argument to this function will not apply. For this
  reason, it is preferred that you pass in the tuple format, i.e

  Prefer this:
  `Api.aggregate(query, {:count_of_things, :count})`

  Over this:
  `Api.aggregate(query, Ash.Query.Aggregate.new(...))`

  #{Spark.Options.docs(@aggregate_opts)}
  """
  @spec aggregate(
          Ash.Query.t() | Ash.Resource.t(),
          aggregates :: aggregate | list(aggregate),
          opts :: Keyword.t()
        ) ::
          {:ok, term} | {:error, Ash.Error.t()}
  @doc spark_opts: [{2, @aggregate_opts}]
  def aggregate(query, aggregate_or_aggregates, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    domain = Ash.Helpers.domain!(query, opts)
    opts = Spark.Options.validate!(opts, @aggregate_opts)

    query = Ash.Query.new(query)

    with {:ok, _resource} <- Ash.Domain.Info.resource(domain, query.resource),
         {:ok, result} <-
           Ash.Actions.Aggregate.run(domain, query, List.wrap(aggregate_or_aggregates), opts) do
      {:ok, result}
    else
      {:error, error} -> {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the count of results that would be returned from a given query, or raises an error.
  """
  @doc spark_opts: [{1, @aggregate_opts}]
  def count!(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> count(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Fetches the count of results that would be returned from a given query.
  """
  @doc spark_opts: [{1, @aggregate_opts}]
  def count(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    query = Ash.Query.new(query)

    {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

    case aggregate(query, {:count, :count, aggregate_opts}, opts) do
      {:ok, %{count: count}} ->
        {:ok, count}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Returns whether or not the query would return any results, or raises an error.
  """
  @doc spark_opts: [{1, @aggregate_opts}]
  def exists?(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> exists(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Returns whether or not the query would return any results.
  """
  @doc spark_opts: [{1, @aggregate_opts}]
  def exists(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    query = Ash.Query.new(query)

    opts =
      if query.action do
        Keyword.put(opts, :read_action, query.action.name)
      else
        opts
      end

    {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

    case aggregate(query, {:exists, :exists, aggregate_opts}, opts) do
      {:ok, %{exists: exists}} ->
        {:ok, exists}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the first value for a given field, or raises an error.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def first(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> Ash.Query.new()
    |> Ash.Query.select([])
    |> Ash.Query.load(field)
    |> Ash.Query.limit(1)
    |> read_one(opts)
    |> case do
      {:ok, record} when not is_nil(record) ->
        case field do
          field when is_atom(field) ->
            {:ok, Map.get(record, field)}

          %Ash.Query.Aggregate{load: nil, name: name} ->
            {:ok, Map.get(record.aggregates, name)}

          %Ash.Query.Aggregate{load: load} ->
            {:ok, Map.get(record, load)}

          %Ash.Query.Calculation{load: nil, name: name} ->
            {:ok, Map.get(record.calculations, name)}

          %Ash.Query.Calculation{load: load} ->
            {:ok, Map.get(record, load)}

          field ->
            {:ok, Map.get(record, field)}
        end

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the first value for a given field.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def first!(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> first(field, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Fetches the sum of a given field.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def sum(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

    case aggregate(
           query,
           {:sum, :sum, Keyword.put(aggregate_opts, :field, field)},
           opts
         ) do
      {:ok, %{sum: value}} ->
        {:ok, value}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the sum of a given field or raises an error.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def sum!(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> sum(field, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Fetches a list of all values of a given field.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def list(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> Ash.Query.new()
    |> Ash.Query.select([])
    |> Ash.Query.load(field)
    |> Ash.Actions.Read.unpaginated_read(nil, opts)
    |> case do
      {:ok, records} ->
        case field do
          field when is_atom(field) ->
            {:ok, Enum.map(records, fn record -> Map.get(record, field) end)}

          %Ash.Query.Aggregate{load: nil, name: name} ->
            {:ok, Enum.map(records, fn record -> Map.get(record.aggregates, name) end)}

          %Ash.Query.Aggregate{load: load} ->
            {:ok, Enum.map(records, fn record -> Map.get(record, load) end)}

          %Ash.Query.Calculation{load: nil, name: name} ->
            {:ok, Enum.map(records, fn record -> Map.get(record.calculations, name) end)}

          %Ash.Query.Calculation{load: load} ->
            {:ok, Enum.map(records, fn record -> Map.get(record, load) end)}

          field ->
            {:ok, Enum.map(records, fn record -> Map.get(record, field) end)}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  @doc """
  Fetches a list of all values of a given field or raises an error.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def list!(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> list(field, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Fetches the greatest of all values of a given field.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def max(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

    case aggregate(
           query,
           {:max, :max, Keyword.put(aggregate_opts, :field, field)},
           opts
         ) do
      {:ok, %{max: value}} ->
        {:ok, value}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the greatest of all values of a given field or raises an error.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def max!(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> __MODULE__.max(field, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Fetches the least of all values of a given field.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def min(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

    case aggregate(
           query,
           {:min, :min, Keyword.put(aggregate_opts, :field, field)},
           opts
         ) do
      {:ok, %{min: value}} ->
        {:ok, value}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the least of all values of a given field or raises an error.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def min!(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> min(field, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Fetches the average of all values of a given field.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def avg(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

    case aggregate(
           query,
           {:avg, :avg, Keyword.put(aggregate_opts, :field, field)},
           opts
         ) do
      {:ok, %{avg: value}} ->
        {:ok, value}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the average of all values of a given field or raises an error.
  """
  @doc spark_opts: [{2, @aggregate_opts}]
  def avg!(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> avg(field, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Returns whether or not the user can perform the action, or raises on errors.

  Calls `can/3` with a `maybe_is: true`. See `can/3` for more info.

  ### Options

  #{Spark.Options.docs(@can_question_mark_opts)}
  """
  @spec can?(
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

  @doc spark_opts: [{2, @can_question_mark_opts}]
  def can?(action_or_query_or_changeset, actor, opts \\ []) do
    case Spark.Options.validate(opts, @can_opts) do
      {:ok, opts} ->
        domain = Ash.Helpers.domain!(action_or_query_or_changeset, opts)
        Ash.Can.can?(action_or_query_or_changeset, domain, actor, opts)

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Returns whether or not the user can perform the action, or `:maybe`, returning any errors.

  In cases with "runtime" checks (checks after the action), we may not be able to determine
  an answer, and so the value `:maybe` will be returned from `can/2`. The `can?` function assumes that
  `:maybe` means `true`. Keep in mind, this is just for doing things like "can they do this" in a UI,
  so assuming `:maybe` is `true` is fine. The actual action invocation will be properly checked regardless.
  If you have runtime checks, you may need to use `can` instead of `can?`, or configure what `:maybe` means.

  ### Accepted inputs

  You can pass many different inputs as the subject to `can/3`.

  ```elixir
  # Can this user run this query.
  Ash.Query.t()

  # Can this user run this changeset.
  Ash.Changeset.t()

  # Can this user run this action.
  Ash.ActionInput.t()

  # Can this user run this action.
  {Ash.Resource.t(), :action}

  # Can this user run this action.
  {Ash.Resource.t(), %Action{}}

  # Can this user run this action with this input.
  {Ash.Resource.t(), :atom, %{...input}}

  # Can this user run this action with this input.
  {Ash.Resource.t(), %Action{}, %{...input}}
  ```

  ### Options

  #{Spark.Options.docs(@can_opts)}
  """
  @spec can(
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
  @doc spark_opts: [{2, @can_opts}]
  def can(action_or_query_or_changeset, actor, opts \\ []) do
    domain = Ash.Helpers.domain!(action_or_query_or_changeset, opts)

    case Spark.Options.validate(opts, @can_opts) do
      {:ok, opts} ->
        case Ash.Can.can(action_or_query_or_changeset, domain, actor, opts) do
          {:error, %Ash.Error.Forbidden.InitialDataRequired{} = error} ->
            if opts[:on_must_pass_strict_check] do
              {:error, error}
            else
              {:error, Ash.Error.to_error_class(error)}
            end

          {:error, error} ->
            {:error, Ash.Error.to_error_class(error)}

          other ->
            other
        end

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Runs a generic action or raises an error. See `run_action/2` for more
  """
  @spec run_action!(input :: Ash.ActionInput.t(), opts :: Keyword.t()) ::
          term | no_return
  @doc spark_opts: [{1, @run_action_opts}]
  def run_action!(input, opts \\ []) do
    Ash.Helpers.expect_options!(opts)

    input
    |> run_action(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Runs a generic action.

  Options:

  #{Spark.Options.docs(@run_action_opts)}
  """
  @doc spark_opts: [{1, @run_action_opts}]
  @spec run_action(input :: Ash.ActionInput.t(), opts :: Keyword.t()) ::
          :ok | {:ok, term} | {:error, Ash.Error.t()}
  def run_action(input, opts \\ []) do
    Ash.Helpers.expect_options!(opts)
    domain = Ash.Helpers.domain!(input, opts)

    with {:ok, opts} <- Spark.Options.validate(opts, @run_action_opts),
         input = %{input | domain: domain},
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, input.resource),
         {:ok, result} <- Ash.Actions.Action.run(domain, input, opts) do
      {:ok, result}
    else
      :ok ->
        :ok

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Evaluates the calculation on the resource or raises an error. See `calculate/3` for more.
  """
  @spec calculate!(resource :: Ash.Resource.t(), calculation :: atom, opts :: Keyword.t()) ::
          term | no_return
  @doc spark_opts: [{2, @calculate_opts}]
  def calculate!(resource_or_record, calculation, opts \\ []) do
    Ash.Helpers.expect_resource_or_record!(resource_or_record)
    Ash.Helpers.expect_options!(opts)

    resource_or_record
    |> calculate(calculation, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Evaluates the calculation on the resource.

  If a record is provided, its field values will be used to evaluate the calculation.

  #{Spark.Options.docs(@calculate_opts)}
  """
  @doc spark_opts: [{2, @calculate_opts}]
  @spec calculate(resource :: Ash.Resource.t(), calculation :: atom, opts :: Keyword.t()) ::
          {:ok, term} | {:error, term}
  def calculate(resource_or_record, calculation, opts \\ []) do
    Ash.Helpers.expect_resource_or_record!(resource_or_record)
    Ash.Helpers.expect_options!(opts)

    with {:ok, opts} <- Spark.Options.validate(opts, @calculate_opts) do
      Ash.Actions.Read.Calculations.calculate(resource_or_record, calculation, opts)
    end
  end

  @doc """
  Get a record by an identifier, or raises an error. See `get/3` for more.
  """
  @spec get!(Ash.Resource.t(), term(), Keyword.t()) ::
          Ash.Resource.record() | no_return
  @doc spark_opts: [{2, @get_opts_schema}]
  def get!(resource, id, opts \\ []) do
    Ash.Helpers.expect_resource!(resource)
    Ash.Helpers.expect_options!(opts)

    resource
    |> get(id, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Get a record by an identifier.

  For a resource with a composite primary key, pass a keyword list or map, e.g
  `Ash.get(MyResource, %{first_key: 1, second_key: 2})`

  Additionally, a keyword list or map of keys matching an identity can be provided.

  #{Spark.Options.docs(@get_opts_schema)}
  """
  @doc spark_opts: [{2, @get_opts_schema}]
  @spec get(Ash.Resource.t(), term(), Keyword.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def get(resource, id, opts \\ []) do
    Ash.Helpers.expect_resource!(resource)
    Ash.Helpers.expect_options!(opts)

    with {:ok, opts} <- Spark.Options.validate(opts, @get_opts_schema),
         domain = Ash.Helpers.domain!(resource, opts),
         {:ok, resource} <- Ash.Domain.Info.resource(domain, resource),
         {:ok, filter} <- Ash.Filter.get_filter(resource, id),
         {:ok, read_opts} <-
           Spark.Options.validate(
             Keyword.take(opts, Keyword.keys(@read_opts_schema)),
             @read_opts_schema
           ),
         {:ok, result} <- do_get(resource, filter, domain, opts, read_opts) do
      {:ok, result}
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  defp do_get(resource, filter, domain, opts, read_opts) do
    query =
      resource
      |> Ash.Query.new(domain: domain)
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
           Ash.Error.Query.NotFound.exception(
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
           Ash.Error.Query.NotFound.exception(
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
  end

  @doc """
  Fetch a page relative to the provided page or raises an error
  """
  @spec page!(Ash.Page.page(), page_request) ::
          Ash.Page.page() | no_return
  def page!(page, request) do
    page
    |> page(request)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Fetch a page relative to the provided page.
  """
  @spec page(Ash.Page.page(), page_request) ::
          {:ok, Ash.Page.page()} | {:error, Ash.Error.t()}
  def page(%Ash.Page.Keyset{results: []} = page, :next) do
    {:ok, page}
  end

  def page(%Ash.Page.Keyset{before: nil, after: nil} = page, :prev) do
    {:ok, page}
  end

  def page(%Ash.Page.Keyset{results: [], before: before, rerun: {query, opts}}, :prev)
      when not is_nil(before) do
    new_page_opts =
      query.page
      |> Keyword.delete(:before)
      |> Keyword.put(:after, before)

    query
    |> Ash.Query.page(new_page_opts)
    |> read(opts)
  end

  def page(%Ash.Page.Keyset{}, n) when is_integer(n) do
    {:error, "Cannot seek to a specific page with keyset based pagination"}
  end

  def page(
        %Ash.Page.Keyset{results: results, rerun: {query, opts}} = page,
        :next
      ) do
    last_keyset =
      results
      |> :lists.last()
      |> Map.get(:__metadata__)
      |> Map.get(:keyset)

    new_page_opts =
      query.page
      |> Keyword.delete(:before)
      |> Keyword.put(:after, last_keyset)

    query = Ash.Query.page(query, new_page_opts)

    case read(query, opts) do
      {:ok, %{results: []}} ->
        {:ok, page}

      other ->
        other
    end
  end

  def page(%Ash.Page.Keyset{results: results, rerun: {query, opts}} = page, :prev) do
    first_keyset =
      results
      |> List.first()
      |> Map.get(:__metadata__)
      |> Map.get(:keyset)

    new_page_opts =
      query.page
      |> Keyword.put(:before, first_keyset)
      |> Keyword.delete(:after)

    query = Ash.Query.page(query, new_page_opts)

    case read(query, opts) do
      {:ok, %{results: []}} ->
        {:ok, page}

      other ->
        other
    end
  end

  def page(%Ash.Page.Keyset{rerun: {query, opts}}, :first) do
    page_opts =
      if query.page[:count] do
        [count: true]
      else
        []
      end

    query
    |> Ash.Query.page(page_opts)
    |> read(opts)
  end

  def page(%Ash.Page.Keyset{rerun: {query, opts}}, :self) do
    read(query, opts)
  end

  def page(
        %Ash.Page.Offset{count: count, limit: limit, offset: offset, rerun: {query, opts}},
        request
      ) do
    page_opts =
      case request do
        :next ->
          [offset: offset + limit, limit: limit]

        :prev ->
          [offset: Kernel.max(offset - limit, 0), limit: limit]

        :first ->
          [offset: 0, limit: limit]

        :last ->
          if count do
            [offset: count - limit, limit: limit]
          else
            [offset: 0, limit: limit]
          end

        :self ->
          query.page

        page_num when is_integer(page_num) ->
          [offset: (page_num - 1) * limit, limit: limit]
      end

    page_opts =
      if query.page[:count] do
        Keyword.put(page_opts, :count, true)
      else
        page_opts
      end

    if request == :last && !count do
      {:error, "Cannot fetch last page without counting"}
    else
      query
      |> Ash.Query.page(page_opts)
      |> read(opts)
    end
  end

  @type record_or_records :: Ash.Resource.record() | [Ash.Resource.record()]

  @doc """
  Load fields or relationships on already fetched records. See `load/3` for more information.
  """
  @spec load!(
          record_or_records ::
            record_or_records
            | {:ok, record_or_records}
            | :error
            | {:error, term}
            | :ok
            | Ash.Page.page(),
          query :: load_statement(),
          opts :: Keyword.t()
        ) ::
          Ash.Resource.record() | [Ash.Resource.record()] | no_return
  @doc spark_opts: [{2, @load_opts_schema}]
  def load!(data, query, opts \\ []) do
    opts = Spark.Options.validate!(opts, @load_opts_schema)

    data
    |> load(query, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Load fields or relationships on already fetched records.

  Accepts a list of non-loaded fields and loads them on the provided records or a query, in
  which case the loaded fields of the query are used. Relationship loads can be nested, for
  example: `Ash.load(record, [posts: [:comments]])`.

  #{Spark.Options.docs(@load_opts_schema)}
  """
  @spec load(
          record_or_records :: Ash.Resource.record() | [Ash.Resource.record()],
          query :: load_statement(),
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.record() | [Ash.Resource.record()]} | {:error, term}

  @doc spark_opts: [{2, @load_opts_schema}]
  def load(data, query, opts \\ [])
  def load([], _, _), do: {:ok, []}
  def load(nil, _, _), do: {:ok, nil}
  def load(:ok, _, _), do: {:ok, :ok}
  def load({:error, error}, _, _), do: {:error, error}

  def load({:ok, values}, query, opts) do
    Ash.Helpers.expect_options!(opts)
    resource = Ash.Helpers.resource_from_data!(values, query, opts)
    load(values, query, Keyword.put(opts, :resource, resource))
  end

  def load(%struct{results: results} = page, query, opts)
      when struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    Ash.Helpers.expect_options!(opts)
    resource = Ash.Helpers.resource_from_data!(page, query, opts)

    results
    |> load(query, Keyword.put(opts, :resource, resource))
    |> case do
      {:ok, results} -> {:ok, %{page | results: results}}
      {:error, error} -> {:error, error}
    end
  end

  def load(data, query, opts) when not is_list(data) do
    Ash.Helpers.expect_options!(opts)
    resource = Ash.Helpers.resource_from_data!(data, query, opts)

    data
    |> List.wrap()
    |> load(query, Keyword.put(opts, :resource, resource))
    |> case do
      {:ok, data} -> {:ok, Enum.at(data, 0)}
      {:error, error} -> {:error, error}
    end
  end

  def load([record | _] = data, query, opts) do
    Ash.Helpers.expect_options!(opts)
    resource = Ash.Helpers.resource_from_data!(data, query, opts)
    opts = Keyword.delete(opts, :resource)

    query =
      case query do
        %Ash.Query{} = query ->
          Ash.Query.set_tenant(query, query.tenant || Map.get(record.__metadata__, :tenant))

        keyword ->
          resource
          |> Ash.Query.new()
          |> Ash.Query.set_tenant(Map.get(record.__metadata__, :tenant))
          |> Ash.Query.load(keyword)
      end

    with %{valid?: true} <- query,
         {:ok, action} <- Ash.Helpers.get_action(query.resource, opts, :read, query.action),
         {:ok, opts} <- Spark.Options.validate(opts, @load_opts_schema),
         domain = Ash.Helpers.domain!(query, opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, resource),
         {:ok, results} <-
           Ash.Actions.Read.unpaginated_read(
             query,
             action,
             Keyword.merge(opts, initial_data: data)
           ) do
      {:ok, results}
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}

      %{errors: errors} ->
        {:error, Ash.Error.to_error_class(errors)}
    end
  end

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

  #{Spark.Options.docs(@stream_opts)}
  """
  @spec stream!(query :: Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
          Enumerable.t(Ash.Resource.record())
  @doc spark_opts: [{1, @stream_opts}]
  def stream!(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    opts = Spark.Options.validate!(opts, @stream_opts)

    domain = Ash.Helpers.domain!(query, opts)

    query = Ash.Query.new(query)

    case Ash.Domain.Info.resource(domain, query.resource) do
      {:ok, _resource} ->
        Ash.Actions.Read.Stream.run!(domain, query, opts)

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  @doc """
  Run an `Ash.Query`. See `read/2` for more.
  """
  @spec read!(Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          list(Ash.Resource.record()) | Ash.Page.page() | no_return
  @doc spark_opts: [{1, @read_opts_schema}]
  def read!(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    opts = Spark.Options.validate!(opts, @read_opts_schema)

    query
    |> read(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Runs an `Ash.Query`.

  For more information on building a query, see `Ash.Query`.

  #{Spark.Options.docs(@read_opts_schema)}

  ## Pagination

  #### Limit/offset pagination
  #{Spark.Options.docs(Ash.Page.Offset.page_opts())}

  #### Keyset pagination
  #{Spark.Options.docs(Ash.Page.Keyset.page_opts())}
  """
  @spec read(Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          {:ok, list(Ash.Resource.record()) | Ash.Page.page()} | {:error, term}
  @doc read: [{1, @read_opts_schema}]
  def read(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query = Ash.Query.new(query)

    domain = Ash.Helpers.domain!(query, opts)

    query =
      if opts[:lock] do
        Ash.Query.lock(query, opts[:lock])
      else
        query
      end

    with {:ok, opts} <- Spark.Options.validate(opts, @read_opts_schema),
         {:ok, action} <- Ash.Helpers.get_action(query.resource, opts, :read, query.action),
         {:ok, action} <- Ash.Helpers.pagination_check(action, query, opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, query.resource),
         {:ok, results} <- Ash.Actions.Read.run(query, action, opts) do
      {:ok, results}
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Refetches a record by primary key or raises an error. See `reload/2` for more.
  """
  @spec reload!(record :: Ash.Resource.record(), opts :: Keyword.t()) ::
          Ash.Resource.record() | no_return
  @doc spark_opts: [{1, @get_opts_schema}]
  def reload!(record, opts \\ []) do
    Ash.Helpers.expect_record!(record)
    Ash.Helpers.expect_options!(opts)

    record
    |> reload(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Refetches a record by primary key. See `reload/2` for more.
  """
  @spec reload(record :: Ash.Resource.record(), opts :: Keyword.t()) ::
          {:ok, Ash.Resource.record()} | {:error, Ash.Error.t()}
  @doc spark_opts: [{1, @get_opts_schema}]
  def reload(record, opts \\ []) do
    Ash.Helpers.expect_record!(record)
    Ash.Helpers.expect_options!(opts)
    %resource{} = record
    id = record |> Map.take(Ash.Resource.Info.primary_key(resource)) |> Enum.to_list()
    opts = Keyword.put_new(opts, :tenant, Map.get(record.__metadata__, :tenant))
    get(resource, id, opts)
  end

  @doc """
  Runs an ash query, returning a single result or raise an error. See `read_one/2` for more.
  """
  @doc spark_opts: [{1, @read_one_opts_schema}]
  def read_one!(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> read_one(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Runs a query on a resource, returning a single result, nil, or an error.

  If more than one result would be returned, an error is returned instead.

  ## Options

  #{Spark.Options.docs(@read_one_opts_schema)}
  """
  @doc spark_opts: [{1, @read_one_opts_schema}]
  def read_one(query, opts \\ []) do
    Ash.Helpers.expect_options!(opts)
    Ash.Helpers.expect_resource_or_query!(query)
    domain = Ash.Helpers.domain!(query, opts)
    query = Ash.Query.new(query)

    with {:ok, opts} <- Spark.Options.validate(opts, @read_one_opts_schema),
         {:ok, action} <- Ash.Helpers.get_action(query.resource, opts, :read, query.action),
         {:ok, action} <- Ash.Helpers.pagination_check(action, query, opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, query.resource),
         {:ok, result} <- do_read_one(query, action, opts) do
      {:ok, result}
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Runs an ash query, returning the first result or raise an error. See `read_first/2` for more.
  """
  @doc spark_opts: [{1, @read_one_opts_schema}]
  def read_first!(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> read_first(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Runs a query on a resource, returning a first result, nil, or an error.

  Query is automatically limited to only return one result, unlike `read_one/3`

  ## Options

  #{Spark.Options.docs(@read_one_opts_schema)}
  """
  @doc spark_opts: [{1, @read_one_opts_schema}]
  def read_first(query, opts \\ []) do
    Ash.Helpers.expect_options!(opts)
    Ash.Helpers.expect_resource_or_query!(query)
    domain = Ash.Helpers.domain!(query, opts)
    query = query |> Ash.Query.new() |> Ash.Query.limit(1)

    with {:ok, opts} <- Spark.Options.validate(opts, @read_one_opts_schema),
         {:ok, action} <- Ash.Helpers.get_action(query.resource, opts, :read, query.action),
         {:ok, action} <- Ash.Helpers.pagination_check(action, query, opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, query.resource),
         {:ok, result} <- do_read_one(query, action, opts) do
      {:ok, result}
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  defp do_read_one(query, action, opts) do
    query
    |> Ash.Actions.Read.unpaginated_read(action, opts)
    |> Ash.Helpers.unwrap_one()
    |> case do
      {:ok, nil} ->
        if opts[:not_found_error?] do
          {:error,
           Ash.Error.to_error_class(Ash.Error.Query.NotFound.exception(resource: query.resource))}
        else
          {:ok, nil}
        end

      other ->
        other
    end
  end

  @doc """
  Create a record or raises an error. See `create/2` for more information.
  """
  @doc spark_opts: [{1, @create_opts_schema}]
  @spec create!(
          changset_or_resource :: Ash.Changeset.t() | Ash.Resource.t(),
          params_or_opts :: map() | Keyword.t(),
          opts :: Keyword.t()
        ) ::
          Ash.Resource.record()
          | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | no_return
  def create!(changeset_or_resource, params \\ %{}, opts \\ []) do
    create(changeset_or_resource, params, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Create a record.

  #{Spark.Options.docs(@create_opts_schema)}
  """
  @doc spark_opts: [{1, @create_opts_schema}]
  @spec create(
          changset_or_resource :: Ash.Changeset.t() | Ash.Resource.t(),
          params_or_opts :: map() | Keyword.t(),
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:error, term}
  def create(changeset_or_resource, params_or_opts \\ %{}, opts \\ [])

  def create(%Ash.Changeset{} = changeset, params_or_opts, opts) do
    {params, opts} = Ash.Helpers.get_params_and_opts(params_or_opts, opts)

    Ash.Helpers.expect_options!(opts)
    domain = Ash.Helpers.domain!(changeset, opts)

    changeset =
      cond do
        changeset.__validated_for_action__ && params != %{} ->
          raise ArgumentError,
            message: """
            params should not be provided for a changeset
            that has already been validated for an action
            """

        is_nil(changeset.__validated_for_action__) ->
          action =
            opts[:action] ||
              Ash.Resource.Info.primary_action!(changeset.resource, :create).name

          Ash.Changeset.for_create(changeset, action, params, opts)

        true ->
          changeset
      end

    with {:ok, opts} <- Spark.Options.validate(opts, @create_opts_schema),
         {:ok, resource} <- Ash.Domain.Info.resource(domain, changeset.resource),
         {:ok, action} <- Ash.Helpers.get_action(resource, opts, :create, changeset.action) do
      Ash.Actions.Create.run(domain, changeset, action, opts)
    else
      {:error, error} -> {:error, Ash.Error.to_error_class(error)}
    end
  end

  def create(resource, params_or_opts, opts) when is_atom(resource) do
    {params, opts} = Ash.Helpers.get_params_and_opts(params_or_opts, opts)

    Ash.Helpers.expect_resource!(resource)
    Ash.Helpers.expect_options!(opts)

    changeset_opts = Keyword.take(opts, Keyword.keys(Ash.Changeset.for_create_opts()))
    create_opts = Keyword.take(opts, Keyword.keys(@create_opts_schema))

    action = opts[:action] || Ash.Resource.Info.primary_action!(resource, :create).name

    resource
    |> Ash.Changeset.for_create(action, params, changeset_opts)
    |> create(create_opts)
  end

  @doc """
  Creates many records, raising any errors that are returned. See `bulk_create/4` for more.
  """
  @spec bulk_create!(Enumerable.t(map), Ash.Resource.t(), atom, Keyword.t()) ::
          Ash.BulkResult.t() | no_return
  @doc spark_opts: [{3, @bulk_create_opts_schema}]
  def bulk_create!(inputs, resource, action, opts \\ []) do
    inputs
    |> bulk_create(resource, action, opts)
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

  #{Spark.Options.docs(@bulk_create_opts_schema)}
  """
  @spec bulk_create(
          Enumerable.t(map),
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
  @doc spark_opts: [{3, @bulk_create_opts_schema}]
  def bulk_create(inputs, resource, action, opts \\ []) do
    Ash.Helpers.expect_options!(opts)
    domain = Ash.Helpers.domain!(resource, opts)

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
        with {:ok, opts} <- Spark.Options.validate(opts, @bulk_create_opts_schema),
             {:ok, resource} <- Ash.Domain.Info.resource(domain, resource) do
          Ash.Actions.Create.Bulk.run(domain, resource, action, inputs, opts)
        else
          {:error, error} ->
            %Ash.BulkResult{status: :error, errors: [Ash.Error.to_ash_error(error)]}
        end
    end
  end

  @doc """
  Updates all items in the provided enumerable or query with the provided input.

  See `bulk_update/4` for more.
  """
  @spec bulk_update!(
          Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
          action :: atom,
          input :: map,
          opts :: Keyword.t()
        ) ::
          Ash.BulkResult.t() | no_return
  @doc spark_opts: [{3, @bulk_update_opts_schema}]
  def bulk_update!(stream_or_query, action, input, opts \\ []) do
    stream_or_query
    |> bulk_update(action, input, opts)
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

  @doc """
  Updates all items in the provided enumerable or query with the provided input.

  The input is a map of valid inputs for the action. The input will be applied to all records in the enumerable/query.

  If the data layer supports updating from a query, and the update action can be done fully atomically,
  it will be updated in a single pass using the data layer.

  Otherwise, this will stream each record and update it.

  ## Options

  #{Spark.Options.docs(@bulk_update_opts_schema)}
  """
  @spec bulk_update(
          Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
          atom,
          input :: map,
          Keyword.t()
        ) ::
          Ash.BulkResult.t()
  @doc spark_opts: [{3, @bulk_update_opts_schema}]
  def bulk_update(query_or_stream, action, input, opts \\ []) do
    Ash.Helpers.expect_options!(opts)

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
        domain = Ash.Helpers.domain!(query_or_stream, opts)

        with {:ok, opts} <- Spark.Options.validate(opts, @bulk_update_opts_schema),
             {:ok, resource} <-
               Ash.Helpers.resource_from_query_or_stream(domain, query_or_stream, opts) do
          opts = Keyword.put(opts, :resource, resource)

          Ash.Actions.Update.Bulk.run(domain, query_or_stream, action, input, opts)
        else
          {:error, error} ->
            %Ash.BulkResult{status: :error, errors: [Ash.Error.to_ash_error(error)]}
        end
    end
  end

  @doc """
  Destroys all items in the provided enumerable or query with the provided input.

  See `bulk_destroy/4` for more.
  """
  @spec bulk_destroy!(
          Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
          action :: atom,
          input :: map,
          opts :: Keyword.t()
        ) ::
          Ash.BulkResult.t() | no_return
  @doc spark_opts: [{3, @bulk_destroy_opts_schema}]
  def bulk_destroy!(stream_or_query, action, input, opts \\ []) do
    stream_or_query
    |> bulk_destroy(action, input, opts)
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

  @doc """
  Destroys all items in the provided enumerable or query with the provided input.

  The input is a map of valid inputs for the action. The input will be applied to all records in the enumerable/query.

  If the data layer supports destroying from a query, and the destroy action can be done fully atomically,
  it will be updated in a single pass using the data layer.

  Otherwise, this will stream each record and update it.

  ## Options

  #{Spark.Options.docs(@bulk_destroy_opts_schema)}
  """
  @spec bulk_destroy(
          Enumerable.t(Ash.Resource.record()) | Ash.Query.t(),
          atom,
          input :: map,
          Keyword.t()
        ) ::
          Ash.BulkResult.t()
  @doc spark_opts: [{3, @bulk_destroy_opts_schema}]
  def bulk_destroy(query_or_stream, action, input, opts \\ []) do
    Ash.Helpers.expect_options!(opts)

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
        domain = Ash.Helpers.domain!(query_or_stream, opts)

        with {:ok, opts} <- Spark.Options.validate(opts, @bulk_destroy_opts_schema),
             {:ok, resource} <-
               Ash.Helpers.resource_from_query_or_stream(domain, query_or_stream, opts) do
          opts = Keyword.put(opts, :resource, resource)
          Ash.Actions.Destroy.Bulk.run(domain, query_or_stream, action, input, opts)
        else
          {:error, error} ->
            %Ash.BulkResult{status: :error, errors: [Ash.Error.to_ash_error(error)]}
        end
    end
  end

  @doc """
  Update a record. See `update/2` for more information.
  """
  @doc spark_opts: [{1, @update_opts_schema}]
  @spec update!(
          changeset_or_record :: Ash.Changeset.t() | Ash.Resource.record(),
          params_or_opts :: map() | Keyword.t(),
          opts :: Keyword.t()
        ) ::
          Ash.Resource.record()
          | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | no_return
  def update!(changeset_or_record, params_or_opts \\ %{}, opts \\ []) do
    update(changeset_or_record, params_or_opts, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Update a record.

  #{Spark.Options.docs(@update_opts_schema)}
  """
  @spec update(
          changeset_or_record :: Ash.Changeset.t() | Ash.Resource.record(),
          params_or_opts :: map() | Keyword.t(),
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:error, term}
  def update(changeset_or_record, params_or_opts \\ %{}, opts \\ [])

  @doc spark_opts: [{1, @update_opts_schema}]
  def update(%Ash.Changeset{} = changeset, params_or_opts, opts) do
    {params, opts} = Ash.Helpers.get_params_and_opts(params_or_opts, opts)

    changeset =
      cond do
        changeset.__validated_for_action__ && params != %{} ->
          raise ArgumentError,
            message: """
            params should not be provided for a changeset
            that has already been validated for an action
            """

        is_nil(changeset.__validated_for_action__) ->
          action =
            opts[:action] ||
              Ash.Resource.Info.primary_action!(changeset.resource, :update).name

          Ash.Changeset.for_update(changeset, action, params, opts)

        true ->
          changeset
      end

    Ash.Helpers.expect_options!(opts)
    domain = Ash.Helpers.domain!(changeset, opts)

    with {:ok, opts} <- Spark.Options.validate(opts, @update_opts_schema),
         {:ok, resource} <- Ash.Domain.Info.resource(domain, changeset.resource),
         {:ok, action} <- Ash.Helpers.get_action(resource, opts, :update, changeset.action),
         {:ok, result} <- Ash.Actions.Update.run(domain, changeset, action, opts) do
      {:ok, result}
    else
      {:ok, result, notifications} ->
        {:ok, result, notifications}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  def update(record, params_or_opts, opts) do
    {params, opts} = Ash.Helpers.get_params_and_opts(params_or_opts, opts)

    Ash.Helpers.expect_record!(record)
    Ash.Helpers.expect_options!(opts)
    Ash.Helpers.expect_map_or_nil!(opts[:input])

    changeset_opts = Keyword.take(opts, Keyword.keys(Ash.Changeset.for_update_opts()))
    update_opts = Keyword.take(opts, Keyword.keys(@update_opts_schema))

    action = opts[:action] || Ash.Resource.Info.primary_action!(record, :update).name

    record
    |> Ash.Changeset.for_update(action, params, changeset_opts)
    |> update(update_opts)
  end

  @doc """
  Destroy a record. See `destroy/2` for more information.
  """
  @spec destroy!(Ash.Changeset.t() | Ash.Resource.record(), opts :: Keyword.t()) ::
          :ok
          | Ash.Resource.record()
          | list(Ash.Notifier.Notification.t())
          | {Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | no_return
  @doc spark_opts: [{1, @destroy_opts_schema}]
  def destroy!(changeset_or_record, opts \\ []) do
    Ash.Helpers.expect_changeset_or_record!(changeset_or_record)
    Ash.Helpers.expect_options!(opts)
    opts = Spark.Options.validate!(opts, @destroy_opts_schema)

    changeset_or_record
    |> destroy(opts)
    |> Ash.Helpers.unwrap_or_raise!(!(opts[:return_notifications?] || opts[:return_destroyed?]))
  end

  @doc """
  Destroy a record.

  #{Spark.Options.docs(@destroy_opts_schema)}
  """
  @spec destroy(Ash.Changeset.t() | Ash.Resource.record(), opts :: Keyword.t()) ::
          :ok
          | {:ok, Ash.Resource.record()}
          | {:ok, list(Ash.Notifier.Notification.t())}
          | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:error, term}
  @doc spark_opts: [{1, @destroy_opts_schema}]
  def destroy(changeset_or_record, opts \\ []) do
    Ash.Helpers.expect_changeset_or_record!(changeset_or_record)
    Ash.Helpers.expect_options!(opts)

    changeset =
      case changeset_or_record do
        %Ash.Changeset{} = changeset -> changeset
        record -> Ash.Changeset.new(record)
      end

    with {:ok, opts} <- Spark.Options.validate(opts, @destroy_opts_schema),
         domain = Ash.Helpers.domain!(changeset, opts),
         {:ok, resource} <- Ash.Domain.Info.resource(domain, changeset.resource),
         {:ok, action} <- Ash.Helpers.get_action(resource, opts, :destroy, changeset.action),
         {:ok, result} <- Ash.Actions.Destroy.run(domain, changeset, action, opts) do
      {:ok, result}
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}

      {:ok, result, notifications} ->
        {:ok, result, notifications}

      :ok ->
        :ok
    end
  end

  @doc deprecated: """
       Converts a context map to opts to be passed into an action.
       """

  @deprecated "Use `Ash.Context.to_opts/2 instead"
  defdelegate context_to_opts(map, add_to \\ []), to: Ash.Context, as: :to_opts

  @doc false
  def stream_opt_keys, do: Keyword.keys(@stream_opts)
end
