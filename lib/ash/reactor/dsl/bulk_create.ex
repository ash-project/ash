defmodule Ash.Reactor.Dsl.BulkCreate do
  @moduledoc """
  The `bulk_create` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: true,
            action: nil,
            actor: [],
            assume_casted?: false,
            async?: true,
            authorize_changeset_with: :filter,
            authorize_query_with: :filter,
            authorize?: nil,
            batch_size: nil,
            context: nil,
            description: nil,
            domain: nil,
            initial: nil,
            load: nil,
            max_concurrency: 0,
            name: nil,
            notification_metadata: %{},
            notify?: false,
            read_action: nil,
            resource: nil,
            return_errors?: false,
            return_records?: false,
            return_stream?: false,
            rollback_on_error?: true,
            select: [],
            skip_unknown_inputs: [],
            sorted?: false,
            stop_on_error?: false,
            success_state: :success,
            tenant: [],
            timeout: 30_000,
            transaction: false,
            transform: nil,
            type: :bulk_create,
            undo_action: nil,
            undo: :never,
            upsert_fields: [],
            upsert_identity: nil,
            upsert?: false,
            wait_for: []

  @type t :: %__MODULE__{
          __identifier__: any,
          action_step?: true,
          action: atom,
          actor: nil | Ash.Reactor.Dsl.Actor.t(),
          assume_casted?: boolean,
          async?: boolean,
          authorize_changeset_with: :filter | :error,
          authorize_query_with: :filter | :error,
          authorize?: boolean | nil,
          batch_size: nil | pos_integer(),
          context: nil | Ash.Reactor.Dsl.Context.t(),
          description: String.t() | nil,
          domain: Ash.Domain.t(),
          initial: Reactor.Template.t(),
          load: nil | Ash.Reactor.Dsl.ActionLoad.t(),
          max_concurrency: non_neg_integer(),
          name: atom,
          notification_metadata: map,
          notify?: boolean,
          read_action: atom,
          resource: module,
          return_errors?: boolean,
          return_records?: boolean,
          return_stream?: boolean,
          rollback_on_error?: boolean,
          select: [atom],
          skip_unknown_inputs: list(atom | String.t()),
          sorted?: boolean,
          stop_on_error?: boolean,
          success_state: :success | :partial_success,
          tenant: nil | Ash.Reactor.Dsl.Tenant.t(),
          timeout: nil | timeout,
          transaction: :all | :batch | false,
          type: :bulk_create,
          undo_action: nil,
          undo: :never,
          upsert_fields: [],
          upsert_identity: nil
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :bulk_create,
      describe: """
      Declares a step which will call a create action on a resource with a collection of inputs.

      > ### Check the docs! {: .warning}
      >
      > Make sure to thoroughly read and understand the documentation in `Ash.bulk_create/4` before using. Read each option and note the default values. By default, bulk creates don't return records or errors, and don't emit notifications.

      Caveats/differences from `Ash.bulk_create/4`:

      1. `max_concurrency` specifies the number of tasks that Ash will start to process batches, and has no effect on Reactor concurrency targets.  It's could be possible to create a very large number of processes if a number of steps are running bulk actions with a high degree of concurrency.
      2. Setting `notify?` to `true` will cause both `notify?` and `return_notifications?` to be set to true in the underlying call to `Ash.bulk_create/4`. Notifications will then be managed by the `Ash.Reactor.Notifications` Reactor middleware.
      3. If you specify an undo action it must be a generic action which takes the bulk result as it's only argument.

      #{Ash.Reactor.Dsl.Action.__shared_undo_docs__()}
      """,
      examples: [
        """
        bulk_create :create_posts, MyApp.Post, :create do
          initial input(:titles)
          actor result(:get_user)
          tenant result(:get_organisation, [:id])
        end
        """
      ],
      no_depend_modules: [:domain, :resource],
      target: __MODULE__,
      args: [:name, :resource, {:optional, :action}],
      identifier: :name,
      imports: [Reactor.Dsl.Argument],
      entities: [
        context: [Ash.Reactor.Dsl.Context.__entity__()],
        actor: [Ash.Reactor.Dsl.Actor.__entity__()],
        load: [Ash.Reactor.Dsl.ActionLoad.__entity__()],
        tenant: [Ash.Reactor.Dsl.Tenant.__entity__()],
        wait_for: [Reactor.Dsl.WaitFor.__entity__()]
      ],
      singleton_entity_keys: [:actor, :context, :load, :tenant],
      recursive_as: :steps,
      schema:
        [
          assume_casted?: [
            type: :boolean,
            doc:
              "Whether or not to cast attributes and arguments as input. This is an optimization for cases where the input is already casted and/or not in need of casting",
            required: false,
            default: false
          ],
          authorize_changeset_with: [
            type: {:in, [:filter, :error]},
            doc:
              "If set to `:error`, instead of filtering unauthorized changes, unauthorized changes will raise an appropriate forbidden error",
            required: false,
            default: :filter
          ],
          authorize_query_with: [
            type: {:in, [:filter, :error]},
            doc:
              "If set to `:error`, instead of filtering unauthorized query results, unauthorized query results will raise an appropriate forbidden error",
            required: false,
            default: :filter
          ],
          batch_size: [
            type: {:or, [nil, :pos_integer]},
            doc:
              "The number of records to include in each batch. Defaults to the `default_limit` or `max_page_size` of the action, or 100.",
            required: false
          ],
          initial: [
            type: Reactor.Template.type(),
            required: true,
            doc:
              "A collection of inputs to pass to the create action. Must implement the `Enumerable` protocol."
          ],
          max_concurrency: [
            type: :non_neg_integer,
            doc:
              "If set to a value greater than 0, up to that many tasks will be started to run batches asynchronously.",
            required: false,
            default: 0
          ],
          notification_metadata: [
            type: {:or, [:map, Reactor.Template.type()]},
            doc:
              "Metadata to be merged into the metadata field for all notifications sent from this operation.",
            required: false,
            default: %{}
          ],
          notify?: [
            type: :boolean,
            doc:
              "Whether or not to generate any notifications. This may be intensive for large bulk actions.",
            required: false,
            default: false
          ],
          read_action: [
            type: :atom,
            doc: "The action to use when building the read query.",
            required: false
          ],
          return_errors?: [
            type: :boolean,
            doc:
              "Whether or not to return all of the errors that occur. Defaults to false to account for large inserts.",
            required: false,
            default: false
          ],
          return_records?: [
            type: :boolean,
            doc:
              "Whether or not to return all of the records that were inserted. Defaults to false to account for large inserts.",
            required: false,
            default: false
          ],
          return_stream?: [
            type: :boolean,
            doc: "If set to `true`, instead of an `Ash.BulkResult`, a mixed stream is returned.",
            required: false,
            default: false
          ],
          rollback_on_error?: [
            type: :boolean,
            doc:
              "Whether or not to rollback the transaction on error, if the resource is in a transaction.",
            required: false,
            default: true
          ],
          select: [
            type: {:wrap_list, :atom},
            doc:
              "A select statement to apply to records. Ignored if `return_records?` is not `true`.",
            required: false
          ],
          skip_unknown_inputs: [
            type: {:wrap_list, {:or, [:atom, :string]}},
            doc:
              "A list of inputs that, if provided, will be ignored if they are not recognized by the action. Use `:*` to indicate all unknown keys.",
            required: false
          ],
          sorted?: [
            type: :boolean,
            doc:
              "Whether or not to sort results by their input position, in cases where `return_records?` is set to `true`.",
            required: false,
            default: false
          ],
          stop_on_error?: [
            type: :boolean,
            doc:
              "If `true`, the first encountered error will stop the action and be returned. Otherwise, errors will be skipped.",
            required: false,
            default: false
          ],
          success_state: [
            type: {:in, [:success, :partial_success]},
            doc:
              "Bulk results can be entirely or partially successful. Chooses the `Ash.BulkResult` state to consider the step a success.",
            required: false,
            default: :success
          ],
          timeout: [
            type: :timeout,
            doc:
              "If none is provided, the timeout configured on the domain is used (which defaults to `30_000`).",
            required: false
          ],
          transaction: [
            type: {:in, [:all, :batch, false]},
            doc:
              "Whether or not to wrap the entire execution in a transaction, each batch, or not at all.",
            required: false,
            default: :batch
          ],
          upsert_fields: [
            type: {:wrap_list, :atom},
            doc: "The fields to upsert. If not set, the action's `upsert_fields` is used.",
            required: false
          ],
          upsert_identity: [
            type: :atom,
            required: false,
            doc: "The identity to use for the upsert"
          ],
          upsert?: [
            type: :boolean,
            required: false,
            default: false,
            doc: "Whether or not this action should be executed as an upsert."
          ]
        ]
        |> Spark.Options.merge(
          Ash.Reactor.Dsl.Action.__shared_action_option_schema__(),
          "Shared action options"
        )
    }
end
