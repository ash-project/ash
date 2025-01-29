defmodule Ash.Resource.Change.CascadeDestroy do
  @option_schema [
    relationship: [
      type: :atom,
      doc: "The name of the relationship to work on",
      required: true
    ],
    action: [
      type: :atom,
      doc:
        "The name of the destroy action to call on the related resource. Uses the primary destroy by default.",
      required: false
    ],
    read_action: [
      type: :atom,
      doc:
        "The name of the read action to call on the related resource to find results to be destroyed",
      required: false
    ],
    return_notifications?: [
      type: :boolean,
      doc: "Return notifications for all destroyed records?",
      required: false,
      default: false
    ],
    after_action?: [
      type: :boolean,
      doc:
        "If true all the cascade destroys are done in after_action hooks. This makes it safe to use in atomic actions",
      required: false,
      default: true
    ],
    domain: [
      type: {:spark, Ash.Domain},
      private?: true
    ]
  ]

  option_schema = @option_schema

  defmodule Opts do
    @moduledoc false
    use Spark.Options.Validator, schema: option_schema
  end

  @moduledoc """
  Cascade a resource's destroy action to a related resource's destroy action.

  If after_action? is true this change adds an after-action hook that explicitly
  calls destroy on any records related via the named relationship.  It will optimise
  for bulk destroys where possible. This makes it safe to use in atomic actions, but
  might not be possible depending on the data layer setup (see warning below).

  If after_action? is false this change will add a before-action hook for relationships
  where the child record points to the parent (has_many, has_one, many_to_many).

  > #### Beware database constraints {: .warning}
  >
  > Think carefully before using this change with data layers which enforce
  > referential integrity (ie PostgreSQL and SQLite) and you may need to defer
  > constraints for the relationship in question.
  >
  > See also:
  >   1. [`postgres.references.reference.deferrable` DSL](https://hexdocs.pm/ash_postgres/dsl-ashpostgres-datalayer.html#postgres-references-reference-deferrable)
  >   2. [`sqlite.references.reference.deferrable` DSL](https://hexdocs.pm/ash_sqlite/dsl-ashsqlite-datalayer.html#sqlite-references-reference-deferrable)
  >   3. [PostgreSQL's `SET CONSTRAINTS` documentation](https://www.postgresql.org/docs/current/sql-set-constraints.html)
  >   4. [SQLite's `PRAGMA defer_foreign_keys` documentation](https://www.sqlite.org/pragma.html#pragma_defer_foreign_keys)

  > #### Cascading notifications {: .tip}
  >
  > By default notifications are disabled for the related destroy. This is to avoid potentially sending a **lot** of notifications for high-cardinality relationships.

  ## Options

  #{Opts.docs()}

  ## Example

      change {Ash.Resource.Change.CascadeDestroy, relationship: :comments, action: :destroy}

    or, equivalently using `Ash.Resource.Change.Builtins.cascade_destroy/2`:

      change cascade_destroy(:comments, action: :destroy)

  """
  use Ash.Resource.Change
  require Ash.Query

  @doc false
  @impl true
  def change(changeset, opts, context) do
    with {:ok, %Opts{} = opts} <- Opts.validate(opts),
         {:ok, opts} <- validate_relationship_and_action(opts, changeset.resource) do
      if not opts.after_action? and
           opts.relationship.type in [:many_to_many, :has_many, :has_one] do
        Ash.Changeset.before_action(changeset, fn changeset ->
          case {destroy_related([changeset.data], opts, context, changeset),
                opts.return_notifications?} do
            {_, false} ->
              changeset

            {%{notifications: []}, true} ->
              changeset

            {%{notifications: notifications}, true} ->
              {changeset, %{notifications: notifications}}

            {{:error, error}, _} ->
              Ash.Changeset.add_error(changeset, error)
          end
        end)
      else
        Ash.Changeset.after_action(changeset, fn changeset, result ->
          case {destroy_related([result], opts, context, changeset), opts.return_notifications?} do
            {_, false} ->
              {:ok, result}

            {%{notifications: []}, true} ->
              {:ok, result}

            {%{notifications: notifications}, true} ->
              {:ok, result, notifications}

            {{:error, error}, _} ->
              {:error, error}
          end
        end)
      end
    else
      {:error, reason} ->
        Ash.Changeset.add_error(changeset, reason)
    end
  end

  @doc false
  @impl true
  def atomic(%{resource: resource}, opts, _) do
    with {:ok, %Opts{} = opts} <- Opts.validate(opts),
         {:ok, opts} <- validate_relationship_and_action(opts, resource) do
      if not opts.after_action? and
           opts.relationship.type in [:many_to_many, :has_many, :has_one] do
        {:not_atomic, "Need to destroy relationships in a before batch"}
      else
        :ok
      end
    else
      _ ->
        {:error, "Couldn't validate the opts"}
    end
  end

  @doc false
  @impl true
  def before_batch([%{resource: resource} = changeset | _] = changesets, opts, context) do
    with {:ok, %Opts{} = opts} <- Opts.validate(opts),
         {:ok, opts} <- validate_relationship_and_action(opts, resource) do
      records = Enum.map(changesets, & &1.data)

      if not opts.after_action? and
           opts.relationship.type in [:many_to_many, :has_many, :has_one] do
        case {destroy_related(records, opts, context, changeset), opts.return_notifications?} do
          {_, false} ->
            changesets

          {%{notifications: empty}, true} when empty in [nil, []] ->
            changesets

          {%{notifications: notifications}, true} ->
            Enum.concat(changesets, notifications)

          {{:error, error}, _} ->
            Enum.map(changesets, &Ash.Changeset.add_error(&1, error))
        end
      else
        changesets
      end
    else
      {:error, reason} -> [{:error, reason}]
    end
  end

  @doc false
  @impl true
  def after_batch(
        [{%{resource: resource} = changeset, _} | _] = changesets_and_results,
        opts,
        context
      ) do
    with {:ok, %Opts{} = opts} <- Opts.validate(opts),
         {:ok, opts} <- validate_relationship_and_action(opts, resource) do
      records = Enum.map(changesets_and_results, &elem(&1, 1))
      result = Enum.map(records, &{:ok, &1})

      if not opts.after_action? and
           opts.relationship.type in [:many_to_many, :has_many, :has_one] do
        result
      else
        case {destroy_related(records, opts, context, changeset), opts.return_notifications?} do
          {_, false} ->
            result

          {%{notifications: empty}, true} when empty in [nil, []] ->
            result

          {%{notifications: notifications}, true} ->
            Enum.concat(result, notifications)

          {{:error, error}, _} ->
            Enum.map(result, fn _ -> {:error, error} end)
        end
      end
    else
      {:error, reason} -> [{:error, reason}]
    end
  end

  @doc false
  @impl true
  def batch_callbacks?([], _, _), do: false
  def batch_callbacks?(_, _, _), do: true

  @doc false
  def opt_schema, do: @option_schema

  defp validate_relationship_and_action(opts, resource) do
    case Ash.Resource.Info.relationship(resource, opts.relationship) do
      nil ->
        {:error,
         Ash.Error.Changes.InvalidRelationship.exception(
           relationship: opts.relationship,
           message: "Relationship doesn't exist."
         )}

      relationship ->
        if opts.action do
          case Ash.Resource.Info.action(relationship.destination, opts.action) do
            action when action.type == :destroy ->
              {:ok,
               %{
                 opts
                 | action: action,
                   relationship: relationship,
                   domain:
                     relationship.domain || Ash.Resource.Info.domain(relationship.destination)
               }}

            _ ->
              {:error,
               Ash.Error.Invalid.NoSuchAction.exception(
                 resource: relationship.destination,
                 action: opts.action,
                 destroy: :destroy
               )}
          end
        else
          {:ok,
           %{
             opts
             | action: Ash.Resource.Info.primary_action!(relationship.destination, :destroy),
               relationship: relationship,
               domain: relationship.domain || Ash.Resource.Info.domain(relationship.destination)
           }}
        end
    end
  end

  defp destroy_related([], _, _, _), do: :ok

  defp destroy_related(data, opts, context, changeset) do
    action = opts.action
    relationship = opts.relationship

    context_opts =
      context
      |> Ash.Context.to_opts(
        domain: opts.domain,
        return_errors?: true,
        strategy: [:stream, :atomic, :atomic_batches],
        return_notifications?: opts.return_notifications?
      )

    context = Map.merge(relationship.context || %{}, %{cascade_destroy: true})

    case related_query(data, opts) do
      {:ok, query} ->
        Ash.bulk_destroy!(
          query,
          action.name,
          %{},
          Keyword.update(
            context_opts,
            :context,
            context,
            &Map.merge(&1, context)
          )
        )

      :error ->
        if changeset.phase == :after_action and changeset.action_type == :destroy do
          {:error,
           "Unable to cascade destroy for #{relationship.name} relationship in after_action"}
        else
          data
          |> List.wrap()
          |> Ash.load!(
            [
              {relationship.name,
               Ash.Query.set_context(relationship.destination, %{cascade_destroy: true})}
            ],
            authorize?: false
          )
          |> Enum.flat_map(fn record ->
            record
            |> Map.get(relationship.name)
            |> List.wrap()
          end)
          |> Ash.bulk_destroy!(
            action.name,
            %{},
            Keyword.update(
              context_opts,
              :context,
              context,
              &Map.merge(&1, context)
            )
          )
        end
    end
  end

  defp related_query(_records, opts) when opts.relationship.type == :many_to_many, do: :error

  defp related_query(records, opts) do
    if Ash.Actions.Read.Relationships.has_parent_expr?(opts.relationship) do
      :error
    else
      related_query =
        if opts.read_action do
          Ash.Query.for_read(opts.relationship.destination, opts.read_action, %{})
        else
          Ash.Query.new(opts.relationship.destination)
        end

      {:ok,
       Ash.Actions.Read.Relationships.related_query(
         opts.relationship.name,
         records,
         related_query,
         Ash.Query.new(opts.relationship.source)
       )
       |> elem(1)
       |> filter_by_keys(opts.relationship, records)}
    end
  end

  defp filter_by_keys(query, %{no_attributes?: true}, _records) do
    query
  end

  defp filter_by_keys(
         query,
         %{source_attribute: source_attribute, destination_attribute: destination_attribute},
         records
       ) do
    source_values = Enum.map(records, &Map.get(&1, source_attribute))

    Ash.Query.filter(query, ^ref(destination_attribute) in ^source_values)
  end
end
