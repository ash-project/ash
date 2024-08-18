defmodule Ash.Resource.Change.Builtins do
  @moduledoc """
  Built in changes that are available to all resources

  The functions in this module are imported by default in the actions section.
  """

  @doc """
  Applies a filter to the changeset. Has no effect for create actions.

  This ensures that only things matching the provided filter are updated or destroyed.
  """
  @spec filter(expr :: Ash.Expr.t()) :: Ash.Resource.Change.ref()
  def filter(filter) do
    {Ash.Resource.Change.Filter, filter: filter}
  end

  @doc """
  Relates the actor to the data being changed, as the provided relationship.

  ## Options

  #{Spark.Options.docs(Ash.Resource.Change.RelateActor.opt_schema())}

  ## Examples

      change relate_actor(:owner, allow_nil?: true)
  """
  @spec relate_actor(relationship :: atom, opts :: Keyword.t()) :: Ash.Resource.Change.ref()
  def relate_actor(relationship, opts \\ []) do
    opts =
      opts
      |> Keyword.put(:relationship, relationship)
      |> Keyword.put_new(:allow_nil?, false)

    {Ash.Resource.Change.RelateActor, opts}
  end

  @spec debug_log(label :: String.t() | nil) :: Ash.Resource.Change.ref()
  def debug_log(label \\ nil) do
    {Ash.Resource.Change.DebugLog, label: label}
  end

  @doc """
  Apply an "optimistic lock" on a record being updated or destroyed.

  See `Ash.Resource.Change.OptimisticLock` for more.
  """
  def optimistic_lock(attribute) do
    {Ash.Resource.Change.OptimisticLock, attribute: attribute}
  end

  @doc """
  Re-fetches the record being updated and locks it for update.

  Only usable with data layers that support locking `:for_update`.

  This happens in a `before_action` hook (so that it is done as part of the transaction).

  If your resource has global validations (in the top level `validations` block), you may
  want to add `delay_global_validations? true` to your action to ensure they happen on the
  locked record.
  """
  @spec get_and_lock_for_update :: Ash.Resource.Change.ref()
  def get_and_lock_for_update do
    {Ash.Resource.Change.GetAndLockForUpdate, []}
  end

  @doc """
  Re-fetches the record being updated and locks it with the given type.

  This happens in a `before_action` hook (so that it is done as part of the transaction).

  If your resource has global validations (in the top level `validations` block), you may
  want to add `delay_global_validations? true` to your action to ensure they happen on the
  locked record.
  """
  @spec get_and_lock(lock :: Ash.DataLayer.lock_type()) :: Ash.Resource.Change.ref()
  def get_and_lock(lock) do
    {Ash.Resource.Change.GetAndLock, [lock: lock]}
  end

  @doc """
  Updates an existing attribute change by applying a function to it.

  The update function gets called with the value already cast to the correct type, and only gets called
  on valid changesets, so the value is guaranteed to have passed validations and constraints.
  """
  defmacro update_change(attribute, function) do
    {value, function} =
      Spark.CodeHelpers.lift_functions(function, :change_update_change, __CALLER__)

    quote generated: true do
      unquote(function)

      {Ash.Resource.Change.UpdateChange, attribute: unquote(attribute), function: unquote(value)}
    end
  end

  @doc """
  Increments an attribute's value by the amount specified, which defaults to 1.

  Options:

  * `:amount` - Defaults to 1
  * `:overflow_limit` - Defaults to `nil`. If the value is over the overflow limit it will roll-over to the amount being incremented by (for common database limit support)
  """
  @spec increment(attribute :: atom, opts :: Keyword.t()) :: Ash.Resource.Change.ref()
  def increment(attribute, opts \\ []) do
    opts =
      opts
      |> Keyword.put_new(:amount, 1)
      |> Keyword.put_new(:overflow_limit, nil)
      |> Keyword.put(:attribute, attribute)

    {Ash.Resource.Change.Increment, opts}
  end

  @doc """
  Sets the attribute to the value provided.

  If a zero argument function is provided, it is called to determine the value.

  Use `arg(:argument_name)` to use the value of the given argument. If the argument is not supplied then nothing happens.

  ## Options

  #{Spark.Options.docs(Keyword.drop(Ash.Resource.Change.SetAttribute.opt_schema(), [:attribute, :value]))}

  ## Examples

      change set_attribute(:active, false)
      change set_attribute(:opened_at, &DateTime.utc_now/0)
      change set_attribute(:status, arg(:status))
      change set_attribute(:encrypted_data, arg(:data), set_when_nil?: false)
  """
  @spec set_attribute(
          attribute :: atom,
          (-> term) | {:_arg, :status} | term(),
          opts :: Keyword.t()
        ) ::
          Ash.Resource.Change.ref()
  def set_attribute(attribute, value, opts \\ []) do
    opts =
      opts
      |> Keyword.put(:attribute, attribute)
      |> Keyword.put(:value, value)

    {Ash.Resource.Change.SetAttribute, opts}
  end

  @doc """
  Updates an attribute using an expression. See `Ash.Changeset.atomic_update/3` for more.

  Options:

  * `:cast_atomic?` - set to `false` to ignore atomic type casting logic. Defaults to `true`.
  """
  @spec atomic_update(attribute :: atom, expr :: Ash.Expr.t(), opts :: Keyword.t()) ::
          Ash.Resource.Change.ref()
  def atomic_update(attribute, expr, opts \\ []) do
    {Ash.Resource.Change.Atomic,
     attribute: attribute, expr: expr, cast_atomic?: Keyword.get(opts, :cast_atomic?, true)}
  end

  @doc """
  Sets the attribute to the value provided if the attribute is not already being changed.

  If a zero argument function is provided, it is called to determine the value.

  Use `arg(:argument_name)` to use the value of the given argument. If the argument is not supplied then nothing happens.

  ## Examples

      change set_new_attribute(:active, false)
      change set_new_attribute(:opened_at, &DateTime.utc_now/0)
      change set_new_attribute(:status, arg(:status))
  """
  @spec set_new_attribute(relationship :: atom, (-> term) | {:_arg, :status} | term()) ::
          Ash.Resource.Change.ref()
  def set_new_attribute(attribute, value) do
    {Ash.Resource.Change.SetAttribute, attribute: attribute, value: value, new?: true}
  end

  @doc """
  Clears a change off of the changeset before the action runs.

  Does not fail if it is being changed, but ensures it is cleared just before the action.

  Can be useful if a change is only used in validations but shouldn't ultimately be written to the data layer.

  ## Examples

      change prevent_change(:email)
  """
  @spec prevent_change(attribute :: atom) :: Ash.Resource.Change.ref()
  def prevent_change(attribute) do
    {Ash.Resource.Change.PreventChange, field: attribute}
  end

  @doc """
  Calls `Ash.Changeset.manage_relationship/4` with the changeset and relationship provided, using the value provided for the named argument.

  If relationship_name is not specified, it is assumed to be the same as the argument.

  For information on the available options, see `Ash.Changeset.manage_relationship/4`.

  ## Examples

      change manage_relationship(:comments, type: :append)
      change manage_relationship(:remove_comments, :comments, type: :remove)
  """
  @spec manage_relationship(
          argument :: atom,
          relationship_name :: atom | nil,
          opts :: Keyword.t()
        ) ::
          Ash.Resource.Change.ref()
  def manage_relationship(argument, relationship_name \\ nil, opts) do
    relationship_name = relationship_name || argument

    {Ash.Resource.Change.ManageRelationship,
     [argument: argument, relationship: relationship_name, opts: opts]}
  end

  @doc """
  Merges the given query context.

  If an MFA is provided, it will be called with the changeset.
  The MFA should return `{:ok, context_to_be_merged}` or `{:error, term}`

  ## Examples

      change set_context(%{something_used_internally: true})
      change set_context({MyApp.Context, :set_context, []})
  """
  @spec set_context(context :: map | mfa) ::
          Ash.Resource.Change.ref()
  def set_context(context) do
    {Ash.Resource.Change.SetContext, context: context}
  end

  @doc """
  Passes the provided value into `Ash.load` after the action has completed.

  ## Example

      change load(:comments)
      change load([:friend_count, :friends])
  """
  @spec load(load :: term()) :: Ash.Resource.Change.ref()
  def load(value) do
    {Ash.Resource.Change.Load, target: value}
  end

  @doc """
  Passes the provided value into `Ash.Changeset.select/3`

  Keep in mind, this will *limit* the fields that are selected. You may want `ensure_selected/1` if you
  want to make sure that something is selected, without deselecting anything else.

  Selecting in changesets does not actually do a select in the data layer. It nils out any
  fields that were not selected after completing the action. This can be useful if you are writing
  policies that have to do with specific fields being selected.

  ## Example

      change select([:name])
  """
  @spec select(select :: atom | list(atom)) :: Ash.Resource.Change.ref()
  def select(value) do
    {Ash.Resource.Change.Select, target: value}
  end

  @doc """
  Passes the provided value into `Ash.Changeset.ensure_selected/2`

  If the value is not already selected, this makes sure it is. Does not deselect anything else.

  ## Example

      change ensure_selected([:necessary_field])
  """
  @spec ensure_selected(select :: atom | list(atom)) :: Ash.Resource.Change.ref()
  def ensure_selected(value) do
    {Ash.Resource.Change.Select, target: value, ensure?: true}
  end

  @doc """
  Cascade this resource's destroy action to a related resource's destroy action.

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

  #{Ash.Resource.Change.CascadeDestroy.opt_schema() |> Keyword.delete(:resource) |> Spark.Options.docs()}

  ## Example

      change cascade_destroy(:relationship)
  """
  @spec cascade_destroy(relationship :: atom) :: Ash.Resource.Change.ref()
  def cascade_destroy(relationship, opts \\ []) do
    {Ash.Resource.Change.CascadeDestroy, Keyword.put(opts, :relationship, relationship)}
  end

  @doc """
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

  #{Ash.Resource.Change.CascadeUpdate.opt_schema() |> Keyword.delete(:resource) |> Spark.Options.docs()}

  ## Example

      change cascade_update(:relationship1)
      change cascade_update(:relationship2, copy_inputs: [:field1, :field2])
  """
  @spec cascade_update(relationship :: atom) :: Ash.Resource.Change.ref()
  def cascade_update(relationship, opts \\ []) do
    {Ash.Resource.Change.CascadeUpdate, Keyword.put(opts, :relationship, relationship)}
  end

  @doc ~S"""
  Directly attach an `after_action` function to the current change.

  See `Ash.Changeset.after_action/3` for more information.

  Provide the option `prepend?: true` to place the hook before all other hooks instead of after.

  ## Example

      change after_action(fn changeset, record, _context ->
        Logger.debug("Successfully executed action #{changeset.action.name} on #{inspect(changeset.resource)}")
        {:ok, record}
      end)
  """
  # @dialyzer {:nowarn_function, "MACRO-after_action": 3}
  defmacro after_action(callback, opts \\ []) do
    {value, function} =
      Spark.CodeHelpers.lift_functions(callback, :change_after_action, __CALLER__)

    quote generated: true do
      unquote(function)

      {Ash.Resource.Change.AfterAction,
       callback: unquote(value), prepend?: unquote(Keyword.get(opts, :prepend?, false))}
    end
  end

  @doc ~S"""
  Directly attach an `after_transaction` function to the current change.

  See `Ash.Changeset.after_transaction/3` for more information.

  Provide the option `prepend?: true` to place the hook before all other hooks instead of after.

  ## Example

      change after_transaction(fn
        changeset, {:ok, record}, _context ->
          Logger.debug("Successfully executed transaction for action #{changeset.action.name} on #{inspect(changeset.resource)}")
          {:ok, record}
        changeset, {:error, reason}, _context ->
          Logger.debug("Failed to execute transaction for action #{changeset.action.name} on #{inspect(changeset.resource)}, reason: #{inspect(reason)}")
          {:error, reason}
      end)
  """
  defmacro after_transaction(callback, opts \\ []) do
    {value, function} =
      Spark.CodeHelpers.lift_functions(callback, :change_after_transaction, __CALLER__)

    quote generated: true do
      unquote(function)

      {Ash.Resource.Change.AfterTransaction,
       callback: unquote(value), prepend?: unquote(Keyword.get(opts, :prepend?, false))}
    end
  end

  @doc ~S"""
  Directly attach a `before_action` function to the current change.

  See `Ash.Changeset.before_action/3` for more information.

  Provide the option `append?: true` to place the hook after all other hooks instead of before.

  ## Example

      change before_action(fn changeset, _context ->
        Logger.debug("About to execute #{changeset.action.name} on #{inspect(changeset.resource)}")

        changeset
      end)
  """
  defmacro before_action(callback, opts \\ []) do
    {value, function} =
      Spark.CodeHelpers.lift_functions(callback, :change_before_action, __CALLER__)

    quote generated: true do
      unquote(function)

      {Ash.Resource.Change.BeforeAction,
       callback: unquote(value), append?: unquote(Keyword.get(opts, :append?, false))}
    end
  end

  @doc ~S"""
  Directly attach a `before_transaction` function to the current change.

  See `Ash.Changeset.before_transaction/3` for more information.

  Provide the option `append?: true` to place the hook after all other hooks instead of before.

  ## Example

      change before_transaction(fn changeset, _context ->
        Logger.debug("About to execute transaction for #{changeset.action.name} on #{inspect(changeset.resource)}")

        changeset
      end)
  """
  defmacro before_transaction(callback, opts \\ []) do
    {value, function} =
      Spark.CodeHelpers.lift_functions(callback, :change_before_transaction, __CALLER__)

    quote generated: true do
      unquote(function)

      {Ash.Resource.Change.BeforeTransaction,
       callback: unquote(value), append?: unquote(Keyword.get(opts, :append?, false))}
    end
  end
end
