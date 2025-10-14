# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash do
  @moduledoc """
  The primary interface to call actions and interact with resources.
  """

  require Ash.Query
  require Ash.Helpers

  @typedoc """
  Page request options for paginated queries.

  Can be atoms for navigation (`:next`, `:prev`, `:first`, `:last`, `:self`) or an integer for specific page numbers.
  """
  @type page_request ::
          :next | :prev | :first | :last | :self | integer

  @typedoc """
  Aggregate specification for queries.

  Can be an `Ash.Query.Aggregate` struct, a `{name, kind}` tuple, or a `{name, kind, opts}` tuple with options.
  """
  @type aggregate ::
          Ash.Query.Aggregate.t()
          | {name :: atom, kind :: atom}
          | {name :: atom, kind :: atom, opts :: Keyword.t()}

  @typedoc """
  Load statement for relationships and calculations.

  Can be a query, a list of atoms, a single atom, keywords, or a list of atoms and tuples with options.
  """
  @type load_statement ::
          Ash.Query.t()
          | [atom]
          | atom
          | Keyword.t()
          | list(atom | {atom, atom | Keyword.t()})

  @global_opts [
    domain: [
      type: {:spark, Ash.Domain},
      doc: "The domain to use."
    ],
    timeout: [
      type: :timeout,
      doc: """
      A positive integer, or `:infinity`. If none is provided, the timeout configured on the domain is used.
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
      type: {:or, [:boolean, {:literal, nil}]},
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
    ],
    scope: [
      type: :any,
      doc:
        "A value that implements the `Ash.Scope.ToOpts` protocol, for passing around actor/tenant/context in a single value. See `Ash.Scope.ToOpts` for more."
    ]
  ]

  @read_opts_schema Spark.Options.merge(
                      [
                        page: [
                          doc: "Pagination options, see `Ash.read/2` for more.",
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
                        skip_unknown_inputs: [
                          type: {:wrap_list, {:or, [:atom, :string]}},
                          doc:
                            "A list of inputs that, if provided, will be ignored if they are not recognized by the action. Use `:*` to indicate all unknown keys."
                        ],
                        reuse_values?: [
                          type: :boolean,
                          default: false,
                          doc:
                            "Whether calculations are allowed to reuse values that have already been loaded, or must refetch them from the data layer."
                        ],
                        strict?: [
                          type: :boolean,
                          default: false,
                          doc: """
                            If set to true, only specified attributes will be loaded when passing
                            a list of fields to fetch on a relationship, which allows for more
                            optimized data-fetching.

                            See `Ash.Query.load/2`.
                          """
                        ],
                        authorize_with: [
                          type: {:one_of, [:filter, :error]},
                          default: :filter,
                          doc:
                            "If set to `:error`, instead of applying authorization filters as a filter, any records not matching the authorization filter will cause an error to be returned."
                        ]
                      ],
                      @global_opts,
                      "Global Options"
                    )

  @doc false
  def read_opts, do: @read_opts_schema

  read_opts_schema = @read_opts_schema

  defmodule ReadOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: read_opts_schema
  end

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
                 Keyword.drop(@read_opts_schema, [:page]),
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
                        ],
                        strict?: [
                          type: :boolean,
                          default: false,
                          doc: """
                            If set to true, only specified attributes will be loaded when passing
                            a list of fields to fetch on a relationship, which allows for more
                            optimized data-fetching.

                            See `Ash.Query.load/2`.
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
                     ],
                     strict?: [
                       type: :boolean,
                       default: false,
                       doc: """
                         If set to true, only specified attributes will be loaded when passing
                         a list of fields to fetch on a relationship, which allows for more
                         optimized data-fetching.

                         See `Ash.Query.load/2`.
                       """
                     ],
                     authorize_with: [
                       type: {:one_of, [:filter, :error]},
                       default: :filter,
                       doc:
                         "If set to `:error`, instead of applying authorization filters as a filter, any records not matching the authorization filter will cause an error to be returned."
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
    skip_unknown_inputs: [
      type: {:wrap_list, {:or, [:atom, :string]}},
      doc:
        "A list of inputs that, if provided, will be ignored if they are not recognized by the action. Use `:*` to indicate all unknown keys."
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
                        return_skipped_upsert?: [
                          type: :boolean,
                          default: false,
                          doc:
                            "If `true`, and a record was *not* upserted because its filter prevented the upsert, the original record (which was *not* upserted) will be returned."
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
                        ],
                        upsert_condition: [
                          type: :any,
                          doc:
                            "An expression to check if the record should be updated when there's a conflict."
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
      doc:
        "If set to `:error`, instead of filtering unauthorized query results, unauthorized query results will raise an appropriate forbidden error. Uses `authorize_with` if not set."
    ],
    authorize_changeset_with: [
      type: {:one_of, [:filter, :error]},
      doc:
        "If set to `:error`, instead of filtering unauthorized changes, unauthorized changes will raise an appropriate forbidden error. Uses `authorize_with` if not set."
    ],
    authorize_with: [
      type: {:one_of, [:filter, :error]},
      default: :filter,
      doc:
        "If set to `:error`, instead of filtering unauthorized query results, unauthorized query results will raise an appropriate forbidden error."
    ],
    context: [
      type: :map,
      doc: "Context to set on each changeset"
    ],
    private_arguments: [
      type: :map,
      default: %{},
      doc:
        "Private argument values to set on each changeset before validations and changes are run."
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
      default: Application.compile_env(:ash, :bulk_actions_default_to_errors?, false),
      doc:
        "Whether to return all errors that occur during the operation. Defaults to the value of `:bulk_actions_default_to_errors?` in your config, or `false` if not set. Returning all errors may be expensive for large inserts."
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
      `{:error, error}` - an error that occurred. May be changeset or an individual error.
      """
    ],
    return_nothing?: [
      type: :boolean,
      default: false,
      doc: """
      Mutes warnings about returning nothing.

      Only relevant if `return_stream?` is set to `true` and all other
      `return_*?` options are set to `false`.
      """
    ],
    stop_on_error?: [
      type: :boolean,
      default: Application.compile_env(:ash, :bulk_actions_default_to_errors?, false),
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
      type: {:wrap_list, {:or, [:atom, :string]}},
      doc:
        "A list of inputs that, if provided, will be ignored if they are not recognized by the action. Use `:*` to indicate all unknown keys."
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
                             return_skipped_upsert?: [
                               type: :boolean,
                               hide: true
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
                             after_action: [
                               type: {:fun, 2},
                               doc: "An after_action hook to be added to each processed changeset"
                             ],
                             upsert_condition: [
                               type: :any,
                               doc:
                                 "An expression to check if the record should be updated when there's a conflict."
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
                            "If true the action will be done atomically if it can (and is configured to do so), ignoring the in memory transformations and validations. You should not generally need to disable this."
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
    scope: [
      type: :any,
      doc:
        "A value that implements the `Ash.Scope.ToOpts` protocol. Will overwrite any actor, tenant or context provided. See `Ash.Context` for more."
    ],
    tenant: [
      type: {:protocol, Ash.ToTenant},
      doc: """
      The tenant, supplied to calculation context.
      """
    ],
    context: [
      type: :map,
      doc: """
      Context to set on the calculation input.
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
    data_layer?: [
      type: :boolean,
      doc: """
      Set to `true` to require that the value be computed within the data layer. Only works for calculations that define an expression.
      """
    ],
    reuse_values?: [
      type: :boolean,
      default: false,
      doc: """
      Set to `true` to reuse existing values on any provided record. Only necessary if providing a record as the basis for calculation.
      """
    ],
    domain: [
      type: {:spark, Ash.Domain},
      doc: "The domain to use for the action"
    ]
  ]

  @doc false
  def calculate_opts, do: @calculate_opts

  @run_action_opts [
    actor: [
      type: :any,
      doc: """
      The actor for handling `^actor/1` templates, supplied to calculation context.
      """
    ],
    scope: [
      type: :any,
      doc:
        "A value that implements the `Ash.Scope.ToOpts` protocol. Will overwrite any actor, tenant or context provided. See `Ash.Context` for more."
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
    ],
    context: [
      type: :map,
      doc: "Context to set on the action input"
    ],
    skip_unknown_inputs: [
      type: {:wrap_list, {:or, [:atom, :string]}},
      doc:
        "A list of inputs that, if provided, will be ignored if they are not recognized by the action. Use `:*` to indicate all unknown keys."
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
    validate?: [
      type: :boolean,
      default: false,
      doc: "Whether or not to treat an invalid action as a non-allowed action."
    ],
    reuse_values?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not loaded data like aggregates, calculations and relationships should be checked in memory if possible, instead of querying. No effect if `pre_flight?` is `false`."
    ],
    pre_flight?: [
      type: :boolean,
      default: true,
      doc:
        "Whether or not this is a pre_flight check (which may perform optimized in-memory checks) or the final proper check."
    ],
    scope: [
      type: :any,
      doc:
        "A value that implements the `Ash.Scope.ToOpts` protocol. Will overwrite any actor, tenant or context provided. See `Ash.Context` for more."
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
    ],
    log?: [
      type: :boolean,
      default: false,
      doc: "Whether or not to log the authorization result."
    ]
  ]

  @doc false
  def can_opts, do: @can_opts

  @can_question_mark_opts Spark.Options.Helpers.set_default!(@can_opts, :maybe_is, true)

  @doc false
  def can_question_mark_opts, do: @can_question_mark_opts

  @doc """
  Runs an aggregate or aggregates over a resource query, returning the result or raising an error.

  This is the bang version of `aggregate/3` that raises an error if the operation fails.

  ## Examples

      iex> MyApp.Post |> Ash.aggregate!({:count, :count})
      42

      iex> query |> Ash.aggregate!([{:avg_likes, :avg, field: :likes}, {:count, :count}])
      %{avg_likes: 10.5, count: 42}

  ## See also

  - `aggregate/3` for the non-raising version
  - `count!/2` for counting records specifically
  - `sum!/3` for summing field values
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Aggregates Guide](/documentation/topics/resources/aggregates.md) for resource-level aggregates
  """
  @spec aggregate!(
          Ash.Query.t() | Ash.Resource.t(),
          aggregate | list(aggregate),
          opts :: Keyword.t()
        ) :: term | no_return
  @doc spark_opts: [{2, @aggregate_opts}]
  def aggregate!(query, aggregate_or_aggregates, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> aggregate(aggregate_or_aggregates, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  aggregate_opts = @aggregate_opts

  defmodule AggregateOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: aggregate_opts
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

  ## Examples

      iex> MyApp.Post |> Ash.aggregate({:count, :count})
      {:ok, %{count: 42}}

      iex> query |> Ash.aggregate([{:avg_likes, :avg, field: :likes}, {:count, :count}])
      {:ok, %{avg_likes: 10.5, count: 42}}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.aggregate({:sum_views, :sum, field: :view_count})
      {:ok, %{sum_views: 1542}}

  ## See also

  - `aggregate!/3` for the raising version
  - `count/2` for counting records specifically
  - `sum/3` for summing field values
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Aggregates Guide](/documentation/topics/resources/aggregates.md) for resource-level aggregates

  ## Options

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

    query = Ash.Query.new(query)

    with {:ok, opts} <- AggregateOpts.validate(opts),
         opts <- AggregateOpts.to_options(opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, query.resource),
         {:ok, result} <-
           Ash.Actions.Aggregate.run(domain, query, List.wrap(aggregate_or_aggregates), opts) do
      {:ok, result}
    else
      {:error, error} -> {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the count of results that would be returned from a given query, or raises an error.

  ## Examples

      iex> MyApp.Post |> Ash.count!()
      42

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.count!()
      15

  ## See also

  - `count/2` for the non-raising version
  - `aggregate!/3` for running multiple aggregates
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Aggregates Guide](/documentation/topics/resources/aggregates.md) for resource-level aggregates
  """
  @spec count!(Ash.Query.t() | Ash.Resource.t(), Keyword.t()) :: non_neg_integer() | no_return
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

  ## Examples

      iex> MyApp.Post |> Ash.count()
      {:ok, 42}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.count()
      {:ok, 15}

  ## See also

  - `count!/2` for the raising version
  - `aggregate/3` for running multiple aggregates
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Aggregates Guide](/documentation/topics/resources/aggregates.md) for resource-level aggregates
  """
  @spec count(Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          {:ok, non_neg_integer()} | {:error, Ash.Error.t()}
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

  ## Examples

      iex> MyApp.Post |> Ash.exists?()
      true

      iex> MyApp.Post |> Ash.Query.filter(published: false) |> Ash.exists?()
      false

  ## See also

  - `exists/2` for the non-raising version
  - `count!/2` for getting the actual count
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  """
  @spec exists?(Ash.Query.t() | Ash.Resource.t(), Keyword.t()) :: boolean() | no_return
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

  ## Examples

      iex> MyApp.Post |> Ash.exists()
      {:ok, true}

      iex> MyApp.Post |> Ash.Query.filter(published: false) |> Ash.exists()
      {:ok, false}

  ## See also

  - `exists?/2` for the raising version
  - `count/2` for getting the actual count
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  """
  @spec exists(Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          {:ok, boolean()} | {:error, Ash.Error.t()}
  @doc spark_opts: [{1, @aggregate_opts}]
  def exists(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    query = Ash.Query.new(query)

    {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

    case aggregate(query, {:exists, :exists, aggregate_opts}, opts) do
      {:ok, %{exists: exists}} ->
        {:ok, exists}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Fetches the first value for a given field.

  ## Examples

      iex> MyApp.Post |> Ash.first(:title)
      {:ok, "Hello World"}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.first(:view_count)
      {:ok, 42}

  ## See also

  - `first!/3` for the raising version
  - `list/3` for getting all values of a field
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Aggregates Guide](/documentation/topics/resources/aggregates.md) for resource-level aggregates
  """
  @spec first(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) ::
          {:ok, term()} | {:error, Ash.Error.t()}
  @doc spark_opts: [{2, @aggregate_opts}]
  def first(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    {default, opts} = Keyword.pop(opts, :default)

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

      {:ok, nil} ->
        {:ok, nil}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
    |> case do
      {:ok, nil} when is_function(default) -> {:ok, default.()}
      {:ok, nil} -> {:ok, default}
      other -> other
    end
  end

  @doc """
  Fetches the first value for a given field, or raises an error.

  ## Examples

      iex> MyApp.Post |> Ash.first!(:title)
      "Hello World"

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.first!(:view_count)
      42

  ## See also

  - `first/3` for the non-raising version
  - `list!/3` for getting all values of a field
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Aggregates Guide](/documentation/topics/resources/aggregates.md) for resource-level aggregates
  """
  @spec first!(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) :: term() | no_return
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

  ## Examples

      iex> MyApp.Post |> Ash.sum(:view_count)
      {:ok, 1542}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.sum(:likes)
      {:ok, 238}

  ## See also

  - `sum!/3` for the raising version
  - `avg/3` for getting the average value
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Aggregates Guide](/documentation/topics/resources/aggregates.md) for resource-level aggregates
  """
  @spec sum(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) ::
          {:ok, number()} | {:error, Ash.Error.t()}
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

  ## Examples

      iex> MyApp.Post |> Ash.sum!(:view_count)
      1542

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.sum!(:likes)
      238

  ## See also

  - `sum/3` for the non-raising version
  - `avg!/3` for getting the average value
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec sum!(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) :: number() | no_return
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

  ## Examples

      iex> MyApp.Post |> Ash.list(:title)
      {:ok, ["Hello World", "Another Post", "Final Post"]}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.list(:view_count)
      {:ok, [42, 15, 89]}

  ## See also

  - `list!/3` for the raising version
  - `first/3` for getting just the first value
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec list(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) ::
          {:ok, list(term())} | {:error, Ash.Error.t()}
  @doc spark_opts: [{2, @aggregate_opts}]
  def list(query, field, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    if opts[:uniq?] do
    end

    query
    |> Ash.Query.new()
    |> Ash.Query.select([])
    |> Ash.Query.load(field)
    |> then(fn query ->
      if opts[:uniq?] do
        Ash.Query.distinct(query, [field])
      else
        query
      end
    end)
    |> then(fn query ->
      if Keyword.get(opts, :include_nil?, false) do
        query
      else
        Ash.Query.filter(query, not is_nil(^Ash.Expr.ref(field)))
      end
    end)
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

  ## Examples

      iex> MyApp.Post |> Ash.list!(:title)
      ["Hello World", "Another Post", "Final Post"]

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.list!(:view_count)
      [42, 15, 89]

  ## See also

  - `list/3` for the non-raising version
  - `first!/3` for getting just the first value
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec list!(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) :: list(term()) | no_return
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

  ## Examples

      iex> MyApp.Post |> Ash.max(:view_count)
      {:ok, 1542}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.max(:created_at)
      {:ok, ~U[2023-12-25 10:30:00Z]}

  ## See also

  - `max!/3` for the raising version
  - `min/3` for getting the minimum value
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec max(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) ::
          {:ok, term()} | {:error, Ash.Error.t()}
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

  ## Examples

      iex> MyApp.Post |> Ash.max!(:view_count)
      1542

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.max!(:created_at)
      ~U[2023-12-25 10:30:00Z]

  ## See also

  - `max/3` for the non-raising version
  - `min!/3` for getting the minimum value
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec max!(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) :: term() | no_return
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

  ## Examples

      iex> MyApp.Post |> Ash.min(:view_count)
      {:ok, 5}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.min(:created_at)
      {:ok, ~U[2023-01-01 08:00:00Z]}

  ## See also

  - `min!/3` for the raising version
  - `max/3` for getting the maximum value
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec min(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) ::
          {:ok, term()} | {:error, Ash.Error.t()}
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

  ## Examples

      iex> MyApp.Post |> Ash.min!(:view_count)
      5

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.min!(:created_at)
      ~U[2023-01-01 08:00:00Z]

  ## See also

  - `min/3` for the non-raising version
  - `max!/3` for getting the maximum value
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec min!(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) :: term() | no_return
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

  ## Examples

      iex> MyApp.Post |> Ash.avg(:view_count)
      {:ok, 42.5}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.avg(:likes)
      {:ok, 15.8}

  ## See also

  - `avg!/3` for the raising version
  - `sum/3` for getting the total sum
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec avg(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) ::
          {:ok, number()} | {:error, Ash.Error.t()}
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

  ## Examples

      iex> MyApp.Post |> Ash.avg!(:view_count)
      42.5

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.avg!(:likes)
      15.8

  ## See also

  - `avg/3` for the non-raising version
  - `sum!/3` for getting the total sum
  - `d:Ash.Resource.Dsl.aggregates` for defining aggregates on resources
  """
  @spec avg!(Ash.Query.t() | Ash.Resource.t(), atom(), Keyword.t()) :: number() | no_return
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

  ## Examples

      iex> Ash.can?({MyApp.Post, :create}, actor)
      true

      iex> Ash.can?({MyApp.Post, :read}, nil)
      true

      iex> Ash.can?({post, :update}, actor)
      false

  ## See also

  - `can/3` for the non-raising version that returns detailed results
  - `d:Ash.Policy.Authorizer.policies` for defining authorization policies
  - [Actors and Authorization Guide](/documentation/topics/security/actors-and-authorization.md) for understanding authorization
  - [Policies Guide](/documentation/topics/security/policies.md) for defining authorization policies

  ### Options

  #{Spark.Options.docs(@can_question_mark_opts)}
  """
  @spec can?(Ash.Can.subject(), actor() | Ash.Scope.t(), Keyword.t()) :: boolean() | no_return()
  @doc spark_opts: [{2, @can_question_mark_opts}]
  def can?(action_or_query_or_changeset, actor_or_scope, opts \\ []) do
    domain = Ash.Helpers.domain!(action_or_query_or_changeset, opts)
    Ash.Can.can?(action_or_query_or_changeset, domain, actor_or_scope, opts)
  end

  can_opts = @can_opts

  defmodule CanOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: can_opts
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

  ### Examples

  ```elixir
  # no actor
  Ash.can?({MyApp.Accounts.Organization, :create}, nil)
  # => false

  # admin user actor
  Ash.can?({MyApp.Accounts.Organization, :create}, %MyApp.Accounts.User{role: :admin})
  # => true

  # check for permission to update a specific thing
  user = MyApp.Accounts.get_post_by_id!(«uuid»)
  Ash.can?({user, :update}, %{role: :user})
  # => false

  # read actions
  # no logged in user. Will say `true` because the action
  # is allowed, but will just be filtered
  Ash.can?({MyApp.Accounts.Organization, :read}, nil)
  # => true

  # check for permission to read a specific thing
  Ash.can?({organization, :read}, nil)
  # => false
  ```

  ### Code Interfaces

  When you define code interfaces, they provide `can_*` functions, which can be used like so:

  ```elixir
  # no actor
  MyApp.Accounts.can_create_organization?(nil)
  # => false

  # admin user actor
  MyApp.Accounts.can_create_organization?(%MyApp.Accounts.User{role: :admin})
  # => true

  # check for permission to update a specific thing
  user = MyApp.Accounts.get_post_by_id!(«uuid»)
  MyApp.Accounts.can_update_user(user, %{role: :user})
  # => false

  # read actions
  # no logged in user. Will say `true` because the action
  # is allowed, but will just be filtered
  MyApp.Accounts.can_read_organizations?(nil)
  # => true

  # check for permission to read a specific thing
  MyApp.Accounts.can_read_organizations?(nil, data: organization)
  # => false
  ```

  ## See also

  - `can?/3` for the raising version that returns true/false
  - `d:Ash.Policy.Authorizer.policies` for defining authorization policies
  - [Actors and Authorization Guide](/documentation/topics/security/actors-and-authorization.md) for understanding authorization
  - [Policies Guide](/documentation/topics/security/policies.md) for defining authorization policies

  ## Options

  #{Spark.Options.docs(@can_opts)}
  """
  @spec can(Ash.Can.subject(), actor() | Ash.Scope.t(), Keyword.t()) ::
          {:ok, boolean | :maybe}
          | {:ok, true, Ash.Changeset.t() | Ash.Query.t()}
          | {:ok, true, Ash.Changeset.t(), Ash.Query.t()}
          | {:ok, false, Exception.t()}
          | {:error, term}
  @doc spark_opts: [{2, @can_opts}]
  def can(action_or_query_or_changeset, actor_or_scope, opts \\ []) do
    domain = Ash.Helpers.domain!(action_or_query_or_changeset, opts)

    case CanOpts.validate(opts) do
      {:ok, opts} ->
        opts = CanOpts.to_options(opts)

        case Ash.Can.can(action_or_query_or_changeset, domain, actor_or_scope, opts) do
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

  ## Examples

      iex> input = Ash.ActionInput.for_action(MyApp.Post, :send_email, %{email: "test@example.com"})
      iex> Ash.run_action!(input)
      :ok

      iex> input = Ash.ActionInput.for_action(MyApp.Calculator, :calculate_tax, %{amount: 100})
      iex> Ash.run_action!(input)
      8.25

  ## See also

  - `run_action/2` for the non-raising version
  - `d:Ash.Resource.Dsl.actions.action` for defining generic actions
  - [Generic Actions Guide](/documentation/topics/actions/generic-actions.md) for understanding generic actions
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts
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

  run_action_opts = @run_action_opts

  defmodule RunActionOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: run_action_opts
  end

  @doc """
  Runs a generic action.

  ## Examples

      iex> input = Ash.ActionInput.for_action(MyApp.Post, :send_email, %{email: "test@example.com"})
      iex> Ash.run_action(input)
      {:ok, :ok}

      iex> input = Ash.ActionInput.for_action(MyApp.Calculator, :calculate_tax, %{amount: 100})
      iex> Ash.run_action(input)
      {:ok, 8.25}

  ## See also

  - `run_action!/2` for the raising version
  - `d:Ash.Resource.Dsl.actions.action` for defining generic actions
  - [Generic Actions Guide](/documentation/topics/actions/generic-actions.md) for understanding generic actions
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts

  ## Options

  #{Spark.Options.docs(@run_action_opts)}
  """
  @doc spark_opts: [{1, @run_action_opts}]
  @spec run_action(input :: Ash.ActionInput.t(), opts :: Keyword.t()) ::
          :ok | {:ok, term} | {:error, Ash.Error.t()}
  def run_action(input, opts \\ []) do
    Ash.Helpers.expect_options!(opts)
    domain = Ash.Helpers.domain!(input, opts)

    with {:ok, opts} <- RunActionOpts.validate(opts),
         opts <- RunActionOpts.to_options(opts),
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

  ## Examples

      iex> Ash.calculate!(post, :word_count)
      142

      iex> Ash.calculate!(MyApp.User, :age, args: %{birth_date: ~D[1990-01-01]})
      34

  ## See also

  - `calculate/3` for the non-raising version
  - `d:Ash.Resource.Dsl.calculations` for defining calculations on resources
  - [Calculations Guide](/documentation/topics/resources/calculations.md) for understanding calculations
  """
  @spec calculate!(
          resource_or_record :: Ash.Resource.t() | Ash.Resource.record(),
          calculation :: atom,
          opts :: Keyword.t()
        ) ::
          term | no_return
  @doc spark_opts: [{2, @calculate_opts}]
  def calculate!(resource_or_record, calculation, opts \\ []) do
    Ash.Helpers.expect_resource_or_record!(resource_or_record)
    Ash.Helpers.expect_options!(opts)

    resource_or_record
    |> calculate(calculation, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  calculate_opts = @calculate_opts

  defmodule CalculateOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: calculate_opts
  end

  @doc """
  Evaluates the calculation on the resource.

  If a record is provided, its field values will be used to evaluate the calculation.

  ## Examples

      iex> Ash.calculate(post, :word_count)
      {:ok, 142}

      iex> Ash.calculate(MyApp.User, :age, args: %{birth_date: ~D[1990-01-01]})
      {:ok, 34}

  ## See also

  - `calculate!/3` for the raising version
  - `d:Ash.Resource.Dsl.calculations` for defining calculations on resources
  - [Calculations Guide](/documentation/topics/resources/calculations.md) for understanding calculations

  ## Options

  #{Spark.Options.docs(@calculate_opts)}
  """
  @doc spark_opts: [{2, @calculate_opts}]
  @spec calculate(
          resource_or_record :: Ash.Resource.t() | Ash.Resource.record(),
          calculation :: atom,
          opts :: Keyword.t()
        ) ::
          {:ok, term} | {:error, term}
  def calculate(resource_or_record, calculation, opts \\ []) do
    Ash.Helpers.expect_resource_or_record!(resource_or_record)
    Ash.Helpers.expect_options!(opts)

    with {:ok, opts} <- CalculateOpts.validate(opts) do
      opts = CalculateOpts.to_options(opts)
      Ash.Actions.Read.Calculations.calculate(resource_or_record, calculation, opts)
    end
  end

  @doc """
  Get a record by an identifier, or raises an error. See `get/3` for more.

  ## Examples

      iex> Ash.get!(MyApp.Post, 1)
      %MyApp.Post{id: 1, title: "Hello World"}

      iex> Ash.get!(MyApp.User, %{email: "user@example.com"})
      %MyApp.User{id: 5, email: "user@example.com"}

      iex> Ash.get!(MyApp.Post, %{first_key: 1, second_key: 2})
      %MyApp.Post{first_key: 1, second_key: 2}

  ## See also

  - `get/3` for the non-raising version
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  """
  @spec get!(Ash.Resource.t(), term(), Keyword.t()) ::
          Ash.Resource.record() | nil | no_return
  @doc spark_opts: [{2, @get_opts_schema}]
  def get!(resource, id, opts \\ []) do
    Ash.Helpers.expect_resource!(resource)
    Ash.Helpers.expect_options!(opts)

    resource
    |> get(id, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  get_opts_schema = @get_opts_schema

  defmodule GetOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: get_opts_schema
  end

  @doc """
  Get a record by an identifier.

  For a resource with a composite primary key, pass a keyword list or map, e.g
  `Ash.get(MyResource, %{first_key: 1, second_key: 2})`

  Additionally, a keyword list or map of keys matching an identity can be provided.

  ## Examples

      iex> Ash.get(MyApp.Post, 1)
      {:ok, %MyApp.Post{id: 1, title: "Hello World"}}

      iex> Ash.get(MyApp.User, %{email: "user@example.com"})
      {:ok, %MyApp.User{id: 5, email: "user@example.com"}}

      iex> Ash.get(MyApp.Post, %{first_key: 1, second_key: 2})
      {:ok, %MyApp.Post{first_key: 1, second_key: 2}}

  ## See also

  - `get!/3` for the raising version
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations

  ## Options

  #{Spark.Options.docs(@get_opts_schema)}
  """
  @doc spark_opts: [{2, @get_opts_schema}]
  @spec get(Ash.Resource.t(), term(), Keyword.t()) ::
          {:ok, Ash.Resource.record() | nil} | {:error, term}
  def get(resource, id, opts \\ []) do
    Ash.Helpers.expect_resource!(resource)
    Ash.Helpers.expect_options!(opts)

    with {:ok, opts} <- GetOpts.validate(opts),
         opts <- GetOpts.to_options(opts),
         domain = Ash.Helpers.domain!(resource, opts),
         {:ok, resource} <- Ash.Domain.Info.resource(domain, resource),
         {:ok, filter} <- Ash.Filter.get_filter(resource, id),
         {:ok, read_opts} <-
           ReadOpts.validate(Keyword.take(opts, Keyword.keys(@read_opts_schema))),
         read_opts <- ReadOpts.to_options(read_opts),
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

  ## Examples

      iex> Ash.page!(page, :next)
      %Ash.Page.Offset{results: [...], more?: true}

      iex> Ash.page!(page, :prev)
      %Ash.Page.Offset{results: [...], more?: false}

      iex> Ash.page!(page, 3)
      %Ash.Page.Offset{results: [...], offset: 40}

  ## See also

  - `page/2` for the non-raising version
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

  ## Examples

      iex> Ash.page(page, :next)
      {:ok, %Ash.Page.Offset{results: [...], more?: true}}

      iex> Ash.page(page, :prev)
      {:ok, %Ash.Page.Offset{results: [...], more?: false}}

      iex> Ash.page(page, 3)
      {:ok, %Ash.Page.Offset{results: [...], offset: 40}}

  ## See also

  - `page!/2` for the raising version
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

    query =
      Ash.Query.page(query, new_page_opts)

    case read(query, opts) do
      {:ok, %{results: []}} ->
        {:ok, %{page | more?: false}}

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

    page_opts =
      if query.page[:limit] do
        Keyword.put(page_opts, :limit, query.page[:limit])
      else
        page_opts
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

  @typedoc """
  A single record or a list of records.
  """
  @type record_or_records :: Ash.Resource.record() | [Ash.Resource.record()]

  @typedoc """
  The actor performing the action - can be any term.
  """
  @type actor :: any()

  @doc """
  Load fields or relationships on already fetched records. See `load/3` for more information.

  ## Examples

      iex> Ash.load!(post, :comments)
      %MyApp.Post{comments: [%MyApp.Comment{}, ...]}

      iex> Ash.load!(posts, [:author, :comments])
      [%MyApp.Post{author: %MyApp.User{}, comments: [...]}, ...]

      iex> Ash.load!(user, [posts: [:comments]])
      %MyApp.User{posts: [%MyApp.Post{comments: [...]}]}

  ## See also

  - `load/3` for the non-raising version
  - `d:Ash.Resource.Dsl.relationships` for defining relationships to load
  - `d:Ash.Resource.Dsl.calculations` for defining calculations to load
  - [Relationships Guide](/documentation/topics/resources/relationships.md) for understanding relationships
  - [Calculations Guide](/documentation/topics/resources/calculations.md) for understanding calculations
  """
  @spec load!(
          record_or_records ::
            record_or_records
            | Ash.Page.page()
            | {:ok, record_or_records}
            | {:ok, Ash.Page.page()}
            | {:error, term}
            | :ok
            | nil,
          query :: load_statement(),
          opts :: Keyword.t()
        ) ::
          Ash.Resource.record() | [Ash.Resource.record()] | nil | no_return
  @doc spark_opts: [{2, @load_opts_schema}]
  def load!(data, query, opts \\ []) do
    data
    |> load(query, opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  load_opts_schema = @load_opts_schema

  defmodule LoadOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: load_opts_schema
  end

  @doc """
  Load fields or relationships on already fetched records.

  Accepts a list of non-loaded fields and loads them on the provided records or a query, in
  which case the loaded fields of the query are used. Relationship loads can be nested, for
  example: `Ash.load(record, [posts: [:comments]])`.

  ## Examples

      iex> Ash.load(post, :comments)
      {:ok, %MyApp.Post{comments: [%MyApp.Comment{}, ...]}}

      iex> Ash.load(posts, [:author, :comments])
      {:ok, [%MyApp.Post{author: %MyApp.User{}, comments: [...]}, ...]}

      iex> Ash.load(user, [posts: [:comments]])
      {:ok, %MyApp.User{posts: [%MyApp.Post{comments: [...]}]}}

  ## See also

  - `load!/3` for the raising version
  - `d:Ash.Resource.Dsl.relationships` for defining relationships to load
  - `d:Ash.Resource.Dsl.calculations` for defining calculations to load

  ## Options

  #{Spark.Options.docs(@load_opts_schema)}
  """
  @spec load(
          record_or_records ::
            record_or_records
            | Ash.Page.page()
            | {:ok, record_or_records}
            | {:ok, Ash.Page.page()}
            | {:error, term}
            | :ok
            | nil,
          query :: load_statement(),
          opts :: Keyword.t()
        ) ::
          {:ok, Ash.Resource.record() | [Ash.Resource.record()] | nil} | {:error, term}

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

  def load(data, load, _) when load in [[], %{}, nil], do: {:ok, data}

  def load(
        data,
        %Ash.Query{select: select, load: rel_load, calculations: calcs, aggregates: aggregates},
        _
      )
      when rel_load in [[], %{}, nil] and calcs in [[], %{}, nil] and aggregates in [[], %{}, nil] and
             select in [[], nil] do
    {:ok, data}
  end

  def load([record | _] = data, query, opts) do
    Ash.Helpers.expect_options!(opts)
    resource = Ash.Helpers.resource_from_data!(data, query, opts)
    opts = Keyword.delete(opts, :resource)

    query =
      case query do
        %Ash.Query{} = query ->
          Ash.Query.set_tenant(
            query,
            opts[:tenant] || query.tenant || Map.get(record.__metadata__, :tenant)
          )

        keyword ->
          resource
          |> Ash.Query.new()
          |> Ash.Query.set_tenant(opts[:tenant] || Map.get(record.__metadata__, :tenant))
          |> Ash.Query.load(keyword, opts)
      end

    with %{valid?: true} <- query,
         {:ok, action} <- Ash.Helpers.get_action(query.resource, opts, :read, query.action),
         {:ok, opts} <- LoadOpts.validate(opts),
         opts <- LoadOpts.to_options(opts),
         domain = Ash.Helpers.domain!(query, opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, resource),
         {:ok, results} <-
           Ash.Actions.Read.unpaginated_read(
             query,
             action,
             Keyword.put(opts, :initial_data, data)
           ) do
      {:ok, results}
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}

      %{errors: errors} ->
        {:error, Ash.Error.to_error_class(errors)}
    end
  end

  stream_opts = @stream_opts

  defmodule StreamOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: stream_opts
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

  ## Examples

      iex> MyApp.Post |> Ash.stream!() |> Enum.take(10)
      [%MyApp.Post{}, %MyApp.Post{}, ...]

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.stream!(strategy: :keyset) |> Enum.map(& &1.title)
      ["Hello World", "Another Post", ...]

      iex> MyApp.Post |> Ash.stream!(strategy: :offset, batch_size: 50) |> Stream.filter(& &1.likes > 10) |> Enum.to_list()
      [%MyApp.Post{likes: 15}, ...]

  ## See also

  - `read/2` for non-streaming reads
  - `d:Ash.Resource.Dsl.actions.read` for defining read actions
  - `d:Ash.Resource.Dsl.actions.read.pagination` for pagination configuration

  ## Options

  #{Spark.Options.docs(@stream_opts)}
  """
  @spec stream!(query :: Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
          Enumerable.t(Ash.Resource.record())

  @doc spark_opts: [{1, @stream_opts}]
  def stream!(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)
    opts = opts |> StreamOpts.validate!() |> StreamOpts.to_options()

    domain = Ash.Helpers.domain!(query, opts)

    query = Ash.Query.new(query)

    case Ash.Domain.Info.resource(domain, query.resource) do
      {:ok, _resource} ->
        Ash.Actions.Read.Stream.run!(domain, query, opts)

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  @typedoc """
  A data layer query structure with execution and counting functions.

  Contains the query that would be executed along with functions for counting,
  running the query, and loading any runtime data needed for the operation.
  """
  @type data_layer_query :: %{
          query: Ash.DataLayer.data_layer_query(),
          ash_query: Ash.Query.t(),
          count: (-> {:ok, integer() | nil} | {:error, Ash.Error.t()}),
          run: (Ash.DataLayer.data_layer_query() ->
                  {:ok, list(Ash.Resource.record()) | Ash.Page.page() | no_return}
                  | {:error, Ash.Error.t()}),
          load: (list(Ash.Resource.record()) | Ash.Page.page() ->
                   {:ok, list(Ash.Resource.record()) | Ash.Page.page()} | {:error, Ash.Error.t()})
        }

  @doc """
  Gets the full query and any runtime calculations that would be loaded

  ## Examples

      iex> query = MyApp.Post |> Ash.Query.filter(published: true)
      iex> Ash.data_layer_query(query)
      {:ok, %{query: #Ecto.Query<...>, ash_query: %Ash.Query{}, count: #Function<...>, run: #Function<...>, load: #Function<...>}}

      iex> MyApp.Post |> Ash.Query.limit(10) |> Ash.data_layer_query()
      {:ok, %{query: #Ecto.Query<...>, ...}}

  ## See also

  - `data_layer_query!/2` for the raising version
  """
  @spec data_layer_query(Ash.Query.t(), opts :: Keyword.t()) ::
          {:ok, data_layer_query} | {:error, Ash.Error.t()}
  def data_layer_query(query, opts \\ []) do
    read(query, Keyword.put(opts, :data_layer_query?, true))
  end

  @doc """
  Gets the full query and any runtime calculations that would be loaded, raising any errors.

  ## Examples

      iex> query = MyApp.Post |> Ash.Query.filter(published: true)
      iex> Ash.data_layer_query!(query)
      %{query: #Ecto.Query<...>, ash_query: %Ash.Query{}, count: #Function<...>, run: #Function<...>, load: #Function<...>}

      iex> MyApp.Post |> Ash.Query.limit(10) |> Ash.data_layer_query!()
      %{query: #Ecto.Query<...>, ...}

  ## See also

  - `data_layer_query/2` for the non-raising version

  See `data_layer_query/2` for more.
  """
  def data_layer_query!(query, opts \\ []) do
    case read(query, Keyword.put(opts, :data_layer_query?, true)) do
      {:ok, result} -> result
      {:error, error} -> raise Ash.Error.to_error_class(error)
    end
  end

  @doc """
  Run an `Ash.Query`. See `read/2` for more.

  ## Examples

      iex> Ash.read!(MyApp.Post)
      [%MyApp.Post{id: 1, title: "Hello"}, %MyApp.Post{id: 2, title: "World"}]

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.read!()
      [%MyApp.Post{id: 1, title: "Hello", published: true}]

      iex> MyApp.Post |> Ash.Query.limit(5) |> Ash.read!()
      [%MyApp.Post{}, %MyApp.Post{}, ...]

  ## See also

  - `read/2` for the non-raising version
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  """
  @spec read!(Ash.Query.t() | Ash.Resource.t(), Keyword.t()) ::
          list(Ash.Resource.record()) | Ash.Page.page() | no_return
  @doc spark_opts: [{1, @read_opts_schema}]
  def read!(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> read(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  @doc """
  Runs an `Ash.Query`.

  For more information on building a query, see `Ash.Query`.

  ## Examples

      iex> Ash.read(MyApp.Post)
      {:ok, [%MyApp.Post{id: 1, title: "Hello"}, %MyApp.Post{id: 2, title: "World"}]}

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.read()
      {:ok, [%MyApp.Post{id: 1, title: "Hello", published: true}]}

      iex> MyApp.Post |> Ash.Query.limit(5) |> Ash.read()
      {:ok, [%MyApp.Post{}, %MyApp.Post{}, ...]}

  ## See also

  - `read!/2` for the raising version
  - `d:Ash.Resource.Dsl.actions.read` for defining read actions
  - `d:Ash.Resource.Dsl.actions.read.pagination` for pagination configuration
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts

  ## Options

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

    {data_layer_query?, opts} = Keyword.pop(opts, :data_layer_query?, false)

    with {:ok, opts} <- ReadOpts.validate(opts),
         opts <- ReadOpts.to_options(opts),
         {:ok, action} <- Ash.Helpers.get_action(query.resource, opts, :read, query.action),
         {:ok, action} <- Ash.Helpers.pagination_check(action, query, opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, query.resource) do
      case Ash.Actions.Read.run(
             query,
             action,
             Keyword.put(opts, :data_layer_query?, data_layer_query?)
           ) do
        {:ok, results} -> {:ok, results}
        {:ok, results, query} -> {:ok, results, query}
        {:error, error} -> {:error, Ash.Error.to_error_class(error)}
      end
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Refetches a record by primary key or raises an error. See `reload/2` for more.

  ## Examples

      iex> Ash.reload!(post)
      %MyApp.Post{id: 1, title: "Updated Title", updated_at: ~U[2023-12-25 10:30:00Z]}

      iex> Ash.reload!(user, load: [:posts])
      %MyApp.User{id: 1, posts: [%MyApp.Post{}, ...]}

  ## See also

  - `reload/2` for the non-raising version
  - `d:Ash.Resource.Dsl.relationships` for defining relationships to load
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
  Refetches a record by primary key. See `get/2` for more.

  ## Examples

      iex> Ash.reload(post)
      {:ok, %MyApp.Post{id: 1, title: "Updated Title", updated_at: ~U[2023-12-25 10:30:00Z]}}

      iex> Ash.reload(user, load: [:posts])
      {:ok, %MyApp.User{id: 1, posts: [%MyApp.Post{}, ...]}}

  ## See also

  - `reload!/2` for the raising version
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

  ## Examples

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.read_one!()
      %MyApp.Post{id: 1, published: true}

      iex> MyApp.User |> Ash.Query.filter(email: "nonexistent@example.com") |> Ash.read_one!()
      nil

  ## See also

  - `read_one/2` for the non-raising version
  """
  @doc spark_opts: [{1, @read_one_opts_schema}]
  @spec read_one!(resource_or_query :: Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
          Ash.Resource.record() | nil
  def read_one!(query, opts \\ []) do
    Ash.Helpers.expect_resource_or_query!(query)
    Ash.Helpers.expect_options!(opts)

    query
    |> read_one(opts)
    |> Ash.Helpers.unwrap_or_raise!()
  end

  read_one_opts_schema = @read_one_opts_schema

  defmodule ReadOneOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: read_one_opts_schema
  end

  @doc """
  Runs a query on a resource, returning a single result, nil, or an error.

  If more than one result would be returned, an error is returned instead.

  ## Examples

      iex> MyApp.Post |> Ash.Query.filter(published: true) |> Ash.read_one()
      {:ok, %MyApp.Post{id: 1, published: true}}

      iex> MyApp.User |> Ash.Query.filter(email: "nonexistent@example.com") |> Ash.read_one()
      {:ok, nil}

  ## See also

  - `read_one!/2` for the raising version

  ## Options

  #{Spark.Options.docs(@read_one_opts_schema)}
  """
  @doc spark_opts: [{1, @read_one_opts_schema}]
  @spec read_one(resource_or_query :: Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
          {:ok, Ash.Resource.record() | nil} | {:error, Ash.Error.t()}
  def read_one(query, opts \\ []) do
    Ash.Helpers.expect_options!(opts)
    Ash.Helpers.expect_resource_or_query!(query)
    domain = Ash.Helpers.domain!(query, opts)
    query = Ash.Query.new(query)

    with {:ok, opts} <- ReadOneOpts.validate(opts),
         opts <- ReadOneOpts.to_options(opts),
         {:ok, action} <- Ash.Helpers.get_action(query.resource, opts, :read, query.action),
         {:ok, action} <- Ash.Helpers.pagination_check(action, query, opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, query.resource) do
      case do_read_one(query, action, opts) do
        {:ok, result} -> {:ok, result}
        {:ok, result, query} -> {:ok, result, query}
        {:error, error} -> {:error, Ash.Error.to_error_class(error)}
      end
    else
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}
    end
  end

  @doc """
  Runs an Ash query, returning the first result or nil, or raising an error. See `read_first/2` for more.

  ## Examples

      iex> Ash.read_first!(MyApp.Post)
      %MyApp.Post{id: 1, title: "First Post"}

      iex> MyApp.Post |> Ash.Query.sort(:created_at) |> Ash.read_first!()
      %MyApp.Post{id: 1, created_at: ~U[2023-01-01 00:00:00Z]}

      iex> MyApp.Post |> Ash.Query.filter(published: false) |> Ash.read_first!()
      nil

  ## See also

  - `read_first/2` for the non-raising version
  """
  @spec read_first!(resource_or_query :: Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
          Ash.Resource.record() | nil
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

  ## Examples

      iex> Ash.read_first(MyApp.Post)
      {:ok, %MyApp.Post{id: 1, title: "First Post"}}

      iex> MyApp.Post |> Ash.Query.sort(:created_at) |> Ash.read_first()
      {:ok, %MyApp.Post{id: 1, created_at: ~U[2023-01-01 00:00:00Z]}}

      iex> MyApp.Post |> Ash.Query.filter(published: false) |> Ash.read_first()
      {:ok, nil}

  ## See also

  - `read_first!/2` for the raising version

  ## Options

  #{Spark.Options.docs(@read_one_opts_schema)}
  """
  @spec read_first(resource_or_query :: Ash.Query.t() | Ash.Resource.t(), opts :: Keyword.t()) ::
          {:ok, Ash.Resource.record() | nil} | {:error, Ash.Error.t()}
  @doc spark_opts: [{1, @read_one_opts_schema}]
  def read_first(query, opts \\ []) do
    Ash.Helpers.expect_options!(opts)
    Ash.Helpers.expect_resource_or_query!(query)
    domain = Ash.Helpers.domain!(query, opts)
    query = query |> Ash.Query.new() |> Ash.Query.limit(1)

    with {:ok, opts} <- ReadOneOpts.validate(opts),
         opts <- ReadOneOpts.to_options(opts),
         {:ok, action} <- Ash.Helpers.get_action(query.resource, opts, :read, query.action),
         {:ok, action} <- Ash.Helpers.pagination_check(action, query, opts),
         {:ok, _resource} <- Ash.Domain.Info.resource(domain, query.resource) do
      case do_read_one(query, action, opts) do
        {:ok, result} -> {:ok, result}
        {:ok, result, query} -> {:ok, result, query}
        {:error, error} -> {:error, Ash.Error.to_error_class(error)}
      end
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
  Create a record. See `create/2` for more information.

  ## Examples

      iex> Ash.create!(MyApp.Post, %{title: "Hello World", content: "..."})
      %MyApp.Post{id: 1, title: "Hello World", content: "..."}

      iex> changeset = Ash.Changeset.for_create(MyApp.User, :create, %{name: "John"})
      iex> Ash.create!(changeset)
      %MyApp.User{id: 1, name: "John"}

      iex> Ash.create!(MyApp.Post, %{title: "New Post"}, return_notifications?: true)
      {%MyApp.Post{id: 2, title: "New Post"}, [%Ash.Notifier.Notification{}]}

  ## See also

  - `create/3` for the non-raising version
  - [Create Actions Guide](/documentation/topics/actions/create-actions.md) for understanding create operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts
  """
  @doc spark_opts: [{1, @create_opts_schema}]
  @spec create!(
          changeset_or_resource :: Ash.Changeset.t() | Ash.Resource.t(),
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

  create_opts_schema = @create_opts_schema

  defmodule CreateOptions do
    @moduledoc false

    use Spark.Options.Validator, schema: create_opts_schema
  end

  @doc """
  Create a record.

  ## Examples

      iex> Ash.create(MyApp.Post, %{title: "Hello World", content: "..."})
      {:ok, %MyApp.Post{id: 1, title: "Hello World", content: "..."}}

      iex> changeset = Ash.Changeset.for_create(MyApp.User, :create, %{name: "John"})
      iex> Ash.create(changeset)
      {:ok, %MyApp.User{id: 1, name: "John"}}

      iex> Ash.create(MyApp.Post, %{title: "New Post"}, return_notifications?: true)
      {:ok, %MyApp.Post{id: 2, title: "New Post"}, [%Ash.Notifier.Notification{}]}

  ## See also

  - `create!/3` for the raising version
  - `d:Ash.Resource.Dsl.actions.create` for defining create actions
  - `d:Ash.Resource.Dsl.attributes` for defining attributes
  - [Create Actions Guide](/documentation/topics/actions/create-actions.md) for understanding create operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts

  ## Options

  #{Spark.Options.docs(@create_opts_schema)}
  """
  @doc spark_opts: [{1, @create_opts_schema}]
  @spec create(
          changeset_or_resource :: Ash.Changeset.t() | Ash.Resource.t(),
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
            opts[:action] || changeset.action ||
              Ash.Resource.Info.primary_action!(changeset.resource, :create).name

          Ash.Changeset.for_create(changeset, action, params, opts)

        true ->
          changeset
      end

    with {:ok, opts} <- CreateOptions.validate(opts),
         opts <- CreateOptions.to_options(opts),
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
      %Ash.BulkResult{status: status, errors: errors}
      when status in [:partial_success, :error] and errors in [nil, []] ->
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

      %Ash.BulkResult{status: status, errors: errors} when status in [:partial_success, :error] ->
        raise Ash.Error.to_error_class(errors)

      bulk_result ->
        bulk_result
    end
  end

  bulk_create_opts_schema = @bulk_create_opts_schema

  defmodule BulkCreateOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: bulk_create_opts_schema
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
  the `bulk_change` callbacks will be applied on the entire list.

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

  ## See also

  - `bulk_create!/4` for the raising version
  - `create/3` for creating single records
  - [Create Actions Guide](/documentation/topics/actions/create-actions.md) for understanding create operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts

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
    Ash.Helpers.verify_stream_options(opts)
    domain = Ash.Helpers.domain!(resource, opts)

    case inputs do
      [] ->
        if opts[:return_stream?] do
          []
        else
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
        end

      inputs ->
        with {:ok, opts} <- BulkCreateOpts.validate(opts),
             opts <- BulkCreateOpts.to_options(opts),
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
      %Ash.BulkResult{status: status, errors: errors}
      when status in [:partial_success, :error] and errors in [nil, []] ->
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

      %Ash.BulkResult{status: status, errors: errors} when status in [:partial_success, :error] ->
        raise Ash.Error.to_error_class(errors)

      bulk_result ->
        bulk_result
    end
  end

  bulk_update_opts = @bulk_update_opts_schema

  defmodule BulkUpdateOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: bulk_update_opts
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
    Ash.Helpers.verify_stream_options(opts)

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

        with {:ok, opts} <- BulkUpdateOpts.validate(opts),
             opts <- BulkUpdateOpts.to_options(opts),
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
      %Ash.BulkResult{status: status, errors: errors}
      when status in [:partial_success, :error] and errors in [nil, []] ->
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

      %Ash.BulkResult{status: status, errors: errors} when status in [:partial_success, :error] ->
        raise Ash.Error.to_error_class(errors)

      %Ash.BulkResult{} = bulk_result ->
        bulk_result
    end
  end

  bulk_destroy_opts = @bulk_destroy_opts_schema

  defmodule BulkDestroyOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: bulk_destroy_opts
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
    Ash.Helpers.verify_stream_options(opts)

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

        with {:ok, opts} <- BulkDestroyOpts.validate(opts),
             opts <- BulkDestroyOpts.to_options(opts),
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

  ## Examples

      iex> Ash.update!(post, %{title: "Updated Title"})
      %MyApp.Post{id: 1, title: "Updated Title"}

      iex> changeset = Ash.Changeset.for_update(user, :update, %{name: "Jane"})
      iex> Ash.update!(changeset)
      %MyApp.User{id: 1, name: "Jane"}

      iex> Ash.update!(post, %{content: "New content"}, return_notifications?: true)
      {%MyApp.Post{id: 1, content: "New content"}, [%Ash.Notifier.Notification{}]}

  ## See also

  - `update/3` for the non-raising version
  - `d:Ash.Resource.Dsl.actions.update` for defining update actions
  - [Update Actions Guide](/documentation/topics/actions/update-actions.md) for understanding update operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts
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

  update_opts = @update_opts_schema

  defmodule UpdateOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: update_opts
  end

  @doc """
  Update a record.

  ## Examples

      iex> Ash.update(post, %{title: "Updated Title"})
      {:ok, %MyApp.Post{id: 1, title: "Updated Title"}}

      iex> changeset = Ash.Changeset.for_update(user, :update, %{name: "Jane"})
      iex> Ash.update(changeset)
      {:ok, %MyApp.User{id: 1, name: "Jane"}}

      iex> Ash.update(post, %{content: "New content"}, return_notifications?: true)
      {:ok, %MyApp.Post{id: 1, content: "New content"}, [%Ash.Notifier.Notification{}]}

  ## See also

  - `update!/3` for the raising version
  - `d:Ash.Resource.Dsl.actions.update` for defining update actions
  - `d:Ash.Resource.Dsl.changes` for defining changes
  - [Update Actions Guide](/documentation/topics/actions/update-actions.md) for understanding update operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts

  ## Options

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

    opts = Keyword.put_new(opts, :tenant, Map.get(changeset.data.__metadata__, :tenant))

    with {:ok, opts} <- UpdateOpts.validate(opts),
         opts <- UpdateOpts.to_options(opts),
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

    opts = Keyword.put_new(opts, :tenant, Map.get(record.__metadata__, :tenant))

    changeset_opts = Keyword.take(opts, Keyword.keys(Ash.Changeset.for_update_opts()))
    update_opts = Keyword.take(opts, Keyword.keys(@update_opts_schema))

    action = opts[:action] || Ash.Resource.Info.primary_action!(record, :update).name

    record
    |> Ash.Changeset.for_update(action, params, changeset_opts)
    |> update(update_opts)
  end

  @doc """
  Destroy a record. See `destroy/2` for more information.

  ## Examples

      iex> Ash.destroy!(post)
      :ok

      iex> changeset = Ash.Changeset.for_destroy(user, :archive)
      iex> Ash.destroy!(changeset)
      :ok

      iex> Ash.destroy!(post, return_destroyed?: true)
      %MyApp.Post{id: 1, title: "Deleted Post"}

      iex> Ash.destroy!(user, return_notifications?: true)
      [%Ash.Notifier.Notification{}]

  ## See also

  - `destroy/2` for the non-raising version
  - `d:Ash.Resource.Dsl.actions.destroy` for defining destroy actions
  - [Destroy Actions Guide](/documentation/topics/actions/destroy-actions.md) for understanding destroy operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts
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

    changeset_or_record
    |> destroy(opts)
    |> Ash.Helpers.unwrap_or_raise!(!(opts[:return_notifications?] || opts[:return_destroyed?]))
  end

  destroy_opts = @destroy_opts_schema

  defmodule DestroyOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: destroy_opts
  end

  @doc """
  Destroy a record.

  ## Examples

      iex> Ash.destroy(post)
      :ok

      iex> changeset = Ash.Changeset.for_destroy(user, :archive)
      iex> Ash.destroy(changeset)
      {:ok, :ok}

      iex> Ash.destroy(post, return_destroyed?: true)
      {:ok, %MyApp.Post{id: 1, title: "Deleted Post"}}

      iex> Ash.destroy(user, return_notifications?: true)
      {:ok, [%Ash.Notifier.Notification{}]}

  ## See also

  - `destroy!/2` for the raising version
  - `d:Ash.Resource.Dsl.actions.destroy` for defining destroy actions
  - [Destroy Actions Guide](/documentation/topics/actions/destroy-actions.md) for understanding destroy operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts

  ## Options

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

    data =
      case changeset_or_record do
        %Ash.Changeset{data: data} -> data
        record -> record
      end

    opts = Keyword.put_new(opts, :tenant, Map.get(data.__metadata__, :tenant))
    Ash.Helpers.expect_options!(opts)

    changeset =
      case changeset_or_record do
        %Ash.Changeset{} = changeset -> changeset
        record -> Ash.Changeset.new(record)
      end

    with {:ok, opts} <- DestroyOpts.validate(opts),
         opts <- DestroyOpts.to_options(opts),
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

  @transaction_opts_schema [
    timeout: [
      type: :timeout,
      doc: """
      The time in milliseconds (as an integer) to wait for the transaction to finish or `:infinity` to wait indefinitely.

      If not specified then default behaviour is adapter specific - for `Ecto`-based data layers it will be `15_000`.
      """
    ],
    return_notifications?: [
      type: :boolean,
      default: false,
      doc: """
      Use this if you want to manually handle sending notifications.

      If true the returned tuple will contain notifications list as the last element.

      To send notifications use `Ash.Notifier.notify(notifications)`. It sends any notifications that can be sent, and returns the rest.
      """
    ]
  ]

  transaction_opts = @transaction_opts_schema

  defmodule TransactionOpts do
    @moduledoc false

    use Spark.Options.Validator, schema: transaction_opts
  end

  @doc deprecated: " Use Ash.transact/3 instead."

  @doc """
  Wraps the execution of the function in a transaction with the resource's data_layer.

  Collects notifications during the function's execution and sends them if the transaction was successful.

  ## Examples

      iex> Ash.transaction(MyApp.Post, fn ->
      ...>   post = Ash.create!(MyApp.Post, %{title: "Hello"})
      ...>   Ash.update!(post, %{content: "World"})
      ...> end)
      {:ok, %MyApp.Post{title: "Hello", content: "World"}}

      iex> Ash.transaction([MyApp.User, MyApp.Post], fn ->
      ...>   user = Ash.create!(MyApp.User, %{name: "John"})
      ...>   Ash.create!(MyApp.Post, %{title: "Hello", author_id: user.id})
      ...> end)
      {:ok, %MyApp.Post{title: "Hello"}}

      iex> Ash.transaction(MyApp.Post, fn ->
      ...>   Ash.create!(MyApp.Post, %{title: "Test"})
      ...> end, return_notifications?: true)
      {:ok, %MyApp.Post{title: "Test"}, [%Ash.Notifier.Notification{}]}

  ## See also

  - `Ash.transact/3` - recommended replacement that always rolls back on error
  - [Actions Guide](/documentation/topics/actions/actions.md) for understanding action concepts
  - [Development Testing Guide](/documentation/topics/development/testing.md) for testing with transactions

  ## Options

  #{Spark.Options.docs(@transaction_opts_schema)}
  """
  @spec transaction(
          resource_or_resources :: Ash.Resource.t() | [Ash.Resource.t()],
          func :: (-> term),
          opts :: Keyword.t()
        ) ::
          {:ok, term}
          | {:ok, term, list(Ash.Notifier.Notification.t())}
          | {:error, term}
  @doc spark_opts: [{2, @transaction_opts_schema}]
  def transaction(resource_or_resources, func, opts \\ []) do
    notify? = !Process.put(:ash_started_transaction?, true)
    old_notifications = Process.delete(:ash_notifications)

    try do
      with {:ok, opts} <- TransactionOpts.validate(opts),
           opts <- TransactionOpts.to_options(opts),
           {:ok, result} <-
             Ash.DataLayer.transaction(
               resource_or_resources,
               func,
               opts[:timeout],
               %{type: :custom, metadata: %{}}
             ) do
        if opts[:return_notifications?] do
          notifications = Process.delete(:ash_notifications) || []

          {:ok, result, notifications}
        else
          if notify? do
            notifications = Process.delete(:ash_notifications) || []

            remaining = Ash.Notifier.notify(notifications)

            remaining
            |> Enum.group_by(&{&1.resource, &1.action})
            |> Enum.each(fn {{resource, action}, remaining} ->
              Ash.Actions.Helpers.warn_missed!(resource, action, %{notifications: remaining})
            end)
          end

          {:ok, result}
        end
      else
        {:error, error} ->
          Process.delete(:ash_notifications)

          {:error, Ash.Error.to_ash_error(error)}
      end
    rescue
      error ->
        Process.delete(:ash_notifications)

        reraise error, __STACKTRACE__
    after
      if notify? do
        Process.delete(:ash_started_transaction?)
      end

      if old_notifications do
        notifications = Process.get(:ash_notifications) || []

        Process.put(:ash_notifications, old_notifications ++ notifications)
      end
    end
  end

  @doc """
  Wraps the execution of the function in a transaction with the resource's data_layer.

  Collects notifications during the function's execution and sends them if the transaction was successful.

  ## Examples

      iex> Ash.transact(MyApp.Post, fn ->
      ...>   post = Ash.create!(MyApp.Post, %{title: "Hello"})
      ...>   Ash.update!(post, %{content: "World"})
      ...> end)
      {:ok, %MyApp.Post{title: "Hello", content: "World"}}

      # Automatic rollback on error

      iex> Ash.transact(MyApp.Post, fn ->
      ...>   Ash.create(MyApp.Post, %{title: "Valid Post"})
      ...>   {:error, :something_went_wrong}
      ...> end)
      {:error, :something_went_wrong}

      # Transaction was automatically rolled back, no post was created

      iex> Ash.transact(MyApp.Post, fn ->
      ...>   Ash.create!(MyApp.Post, %{title: "Test"})
      ...> end, return_notifications?: true)
      {:ok, %MyApp.Post{title: "Test"}, [%Ash.Notifier.Notification{}]}

  ## See also

  - [Actions Guide](/documentation/topics/actions/actions.md) for understanding action concepts
  - [Development Testing Guide](/documentation/topics/development/testing.md) for testing with transactions

  ## Options

  #{Spark.Options.docs(@transaction_opts_schema)}
  """
  @spec transact(
          resource_or_resources :: Ash.Resource.t() | [Ash.Resource.t()],
          func :: (-> term),
          opts :: Keyword.t()
        ) ::
          {:ok, term}
          | {:ok, term, list(Ash.Notifier.Notification.t())}
          | {:error, term}
  @doc spark_opts: [{2, @transaction_opts_schema}]
  def transact(resource_or_resources, func, opts \\ []) do
    notify? = !Process.put(:ash_started_transaction?, true)
    old_notifications = Process.delete(:ash_notifications)

    try do
      with {:ok, opts} <- TransactionOpts.validate(opts),
           opts <- TransactionOpts.to_options(opts),
           {:ok, result} <-
             Ash.DataLayer.transaction(
               resource_or_resources,
               func,
               opts[:timeout],
               %{type: :custom, metadata: %{}},
               rollback_on_error?: true
             ) do
        if opts[:return_notifications?] do
          notifications = Process.delete(:ash_notifications) || []

          {:ok, result, notifications}
        else
          if notify? do
            notifications = Process.delete(:ash_notifications) || []

            remaining = Ash.Notifier.notify(notifications)

            remaining
            |> Enum.group_by(&{&1.resource, &1.action})
            |> Enum.each(fn {{resource, action}, remaining} ->
              Ash.Actions.Helpers.warn_missed!(resource, action, %{notifications: remaining})
            end)
          end

          {:ok, result}
        end
      else
        {:error, error} ->
          Process.delete(:ash_notifications)

          {:error, Ash.Error.to_ash_error(error)}
      end
    rescue
      error ->
        Process.delete(:ash_notifications)

        reraise error, __STACKTRACE__
    after
      if notify? do
        Process.delete(:ash_started_transaction?)
      end

      if old_notifications do
        notifications = Process.get(:ash_notifications) || []

        Process.put(:ash_notifications, old_notifications ++ notifications)
      end
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
