defmodule Ash.Resource.Change.CascadeUpdate do
  @option_schema [
    relationship: [
      type: :atom,
      doc: "The name of the relationship to work on",
      required: true
    ],
    action: [
      type: :atom,
      doc:
        "The name of the update action to call on the related resource. Uses the primary update by default.",
      required: false
    ],
    copy_inputs: [
      type: {:list, :atom},
      doc:
        "A list of fields to copy & pass on to the downstream update. The source action cannot be atomic.",
      required: false,
      default: []
    ],
    read_action: [
      type: :atom,
      doc:
        "The name of the read action to call on the related resource to find results to be updated",
      required: false
    ],
    return_notifications?: [
      type: :boolean,
      doc: "Return notifications for all updated records?",
      required: false,
      default: false
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
  Cascade a resource's update action to a related resource's update action.

  Adds an after-action hook that explicitly calls update on any records related
  via the named relationship.  It will optimise for bulk updates where
  possible.

  Allows you to copy fields from the arguments or changes to the destination,
  this way you can cascade a bunch of changes downstream.


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
  > By default notifications are disabled for the related updates. This is to avoid potentially sending a **lot** of notifications for high-cardinality relationships.

  ## Options

  #{Opts.docs()}

  ## Example

      change {Ash.Resource.Change.CascadeUpdate, relationship: :comments, action: :update_all, copy_inputs: [:name]}

      or, equivalently using `Ash.Resource.Change.Builtins.cascade_update/2`:

      change cascade_update(:comments, action: :update_all, copy_inputs: [:name])

  """
  use Ash.Resource.Change
  require Ash.Query

  @doc false
  @impl true
  def change(changeset, opts, context) do
    with {:ok, %Opts{} = opts} <- Opts.validate(opts),
         {:ok, opts} <- validate_relationship_and_action(opts, changeset) do
      Ash.Changeset.after_action(changeset, fn _changeset, result ->
        case {update_related(changeset, [result], opts, context), opts.return_notifications?} do
          {_, false} -> {:ok, result}
          {%{notifications: []}, true} -> {:ok, result}
          {%{notifications: notifications}, true} -> {:ok, result, notifications}
        end
      end)
    else
      {:error, reason} ->
        Ash.Changeset.add_error(changeset, reason)
    end
  end

  @doc false
  @impl true
  def atomic(_, _, _), do: :ok

  @doc false
  @impl true
  def after_batch([{changeset, _} | _] = changesets_and_results, opts, context) do
    with {:ok, %Opts{} = opts} <- Opts.validate(opts),
         {:ok, opts} <- validate_relationship_and_action(opts, changeset) do
      records = Enum.map(changesets_and_results, &elem(&1, 1))
      result = Enum.map(records, &{:ok, &1})

      case {update_related(changeset, records, opts, context), opts.return_notifications?} do
        {_, false} -> result
        {%{notifications: empty}, true} when empty in [nil, []] -> result
        {%{notifications: notifications}, true} -> Enum.concat(result, notifications)
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

  defp validate_relationship_and_action(opts, changeset) do
    relationship = Ash.Resource.Info.relationship(changeset.resource, opts.relationship)

    destination_action =
      if relationship do
        if opts.action do
          Ash.Resource.Info.action(relationship.destination, opts.action)
        else
          Ash.Resource.Info.primary_action!(relationship.destination, :update)
        end
      end

    is_update? =
      if destination_action != nil and destination_action.type == :update, do: true, else: false

    case {relationship, is_update?} do
      {nil, _} ->
        {:error,
         Ash.Error.Changes.InvalidRelationship.exception(
           relationship: opts.relationship,
           message: "Relationship doesn't exist."
         )}

      {_, false} ->
        {:error,
         Ash.Error.Invalid.NoSuchAction.exception(
           resource: relationship.destination,
           action: opts.action,
           update: :update
         )}

      {_, _} ->
        {:ok,
         %{
           opts
           | action: destination_action,
             relationship: relationship,
             domain: relationship.domain || Ash.Resource.Info.domain(relationship.destination)
         }}
    end
  end

  defp update_related(_, [], _, _), do: :ok

  defp update_related(changeset, data, opts, context) do
    action = opts.action
    relationship = opts.relationship
    copies = opts.copy_inputs

    inputs =
      Enum.reduce(copies, %{}, fn key, downstream_input ->
        with :error <- Map.fetch(changeset.params, key),
             :error <- Map.fetch(changeset.params, to_string(key)) do
          downstream_input
        else
          {:ok, value} ->
            Map.put(downstream_input, key, value)
        end
      end)

    context_opts =
      context
      |> Ash.Context.to_opts(
        domain: opts.domain,
        return_errors?: true,
        strategy: [:stream, :atomic, :atomic_batches],
        skip_unknown_inputs: :*,
        return_notifications?: opts.return_notifications?
      )

    case related_query(data, opts) do
      {:ok, query} ->
        Ash.bulk_update(query, action.name, inputs, context_opts)

      :error ->
        data
        |> List.wrap()
        |> Ash.load!(
          [
            {relationship.name,
             Ash.Query.set_context(relationship.destination, %{cascade_update: true})}
          ],
          authorize?: false
        )
        |> Enum.flat_map(fn record ->
          record
          |> Map.get(relationship.name)
          |> List.wrap()
        end)
        |> Ash.bulk_update!(
          action.name,
          inputs,
          Keyword.update(
            context_opts,
            :context,
            %{cascade_update: true},
            &Map.put(&1, :cascade_update, true)
          )
        )
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
