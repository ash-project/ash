defmodule Ash.Resource.Change.CascadeDestroy do
  @option_schema [
    relationship: [
      type: :atom,
      doc: "The name of the relationship to work on",
      required: true
    ],
    action: [
      type: :atom,
      doc: "The name of the destroy action to call on the related resource",
      required: false,
      default: :destroy
    ],
    return_notifications?: [
      type: :boolean,
      doc: "Return notifications for all destroyed records?",
      required: false,
      default: false
    ]
  ]

  @moduledoc """
  Cascade a resource's destroy action to a related resource's destroy action.

  Adds an after-action hook that explicitly calls destroy on any records related
  via the named relationship.  It will optimise for bulk destroys where
  possible.

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

  #{Spark.Options.docs(@option_schema)}

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
    with {:ok, opts} <- Spark.Options.validate(opts, @option_schema),
         {:ok, opts} <- validate_relationship_and_action(opts, changeset.resource) do
      Ash.Changeset.after_action(changeset, fn _changeset, result ->
        case {destroy_related([result], opts, context), opts[:return_notifications?]} do
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
  def after_batch([{%{resource: resource}, _} | _] = changesets_and_results, opts, context) do
    with {:ok, opts} <- Spark.Options.validate(opts, @option_schema),
         {:ok, opts} <- validate_relationship_and_action(opts, resource) do
      records = Enum.map(changesets_and_results, &elem(&1, 1))
      result = Enum.map(records, &{:ok, &1})

      case {destroy_related(records, opts, context), opts[:return_notifications?]} do
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

  defp validate_relationship_and_action(opts, resource) do
    case Ash.Resource.Info.relationship(resource, opts[:relationship]) do
      nil ->
        {:error,
         Ash.Error.Changes.InvalidRelationship.exception(
           relationship: opts[:relationship],
           message: "Relationship doesn't exist."
         )}

      relationship ->
        case Ash.Resource.Info.action(relationship.destination, opts[:action]) do
          action when action.type == :destroy ->
            opts =
              opts
              |> Keyword.put(:action, action)
              |> Keyword.put(:relationship, relationship)
              |> Keyword.put(
                :domain,
                relationship.domain || Ash.Resource.Info.domain(relationship.destination)
              )

            {:ok, opts}

          _ ->
            {:error,
             Ash.Error.Invalid.NoSuchAction.exception(
               resource: relationship.destination,
               action: opts[:action],
               destroy: :destroy
             )}
        end
    end
  end

  defp destroy_related([], _, _), do: :ok

  defp destroy_related(data, opts, context) do
    action = opts[:action]
    relationship = opts[:relationship]

    context_opts =
      context
      |> Ash.Context.to_opts(
        domain: opts[:domain],
        return_errors?: true,
        strategy: [:stream, :atomic, :atomic_batches],
        return_notifications?: opts[:return_notifications?]
      )

    case related_query(data, opts[:relationship]) do
      {:ok, query} ->
        Ash.bulk_destroy!(query, action.name, %{}, context_opts)

      :error ->
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
            %{cascade_destroy: true},
            &Map.put(&1, :cascade_destroy, true)
          )
        )
    end
  end

  defp related_query(_records, relationship) when relationship.type == :many_to_many, do: :error

  defp related_query(records, relationship) do
    if Ash.Actions.Read.Relationships.has_parent_expr?(relationship) do
      :error
    else
      {:ok,
       Ash.Actions.Read.Relationships.related_query(
         relationship.name,
         records,
         Ash.Query.new(relationship.destination),
         Ash.Query.new(relationship.source)
       )
       |> elem(1)
       |> filter_by_keys(relationship, records)}
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
