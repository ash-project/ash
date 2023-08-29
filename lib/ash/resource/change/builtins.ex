defmodule Ash.Resource.Change.Builtins do
  @moduledoc """
  Built in changes that are available to all resources

  The functions in this module are imported by default in the actions section.
  """

  @relate_actor_opts [
    relationship: [
      doc: "The relationship to set the actor to.",
      required: true,
      type: :atom
    ],
    allow_nil?: [
      doc: "Whether or not to allow the actor to be nil, in which case nothing will happen.",
      type: :boolean,
      default: false
    ],
    field: [
      doc: "The field of the actor to set the relationship to",
      type: :atom
    ]
  ]

  @set_attribute_opts [
    attribute: [
      doc: "The attribute to change.",
      required: true,
      type: :atom
    ],
    value: [
      doc:
        "The value to set the attribute to; may be a fn/0 which will be called to produce the value.",
      required: true,
      type: {:custom, Ash.Resource.Change.SetAttribute, :validate_value, []}
    ],
    set_when_nil?: [
      doc: "When false, decline setting the attribute if it is nil.",
      type: :boolean,
      default: true
    ],
    new?: [
      doc:
        "When true, sets the attribute to the value provided if the attribute is not already being changed.",
      type: :boolean,
      default: false
    ]
  ]

  @doc """
  Relates the actor to the data being changed, as the provided relationship.

  ## Options

  #{Spark.OptionsHelpers.docs(@relate_actor_opts)}

  ## Examples

      change relate_actor(:owner, allow_nil?: true)
  """
  def relate_actor_opts do
    @relate_actor_opts
  end

  @spec relate_actor(relationship :: atom, opts :: Keyword.t()) :: Ash.Resource.Change.ref()
  def relate_actor(relationship, opts \\ []) do
    opts =
      opts
      |> Keyword.put(:relationship, relationship)
      |> Keyword.put_new(:allow_nil?, false)

    {Ash.Resource.Change.RelateActor, opts}
  end

  @doc """
  Apply an "optimistic lock" on a record being updated or destroyed.

  This is modeled after ecto's implementation of optimistic locking, so to
  read more, see their documentation: https://hexdocs.pm/ecto/Ecto.Changeset.html#optimistic_lock/3

  The primary difference is that we leave it to you to increment the field being used for optimistic locking
  yourself. So in ecto you might do `Changeset.optimistic_lock(changeset, :foo)` and that would add 1 to the `:foo` attribute
  automatically. In Ash, you would combine this with the `increment/1` change.

  ```elixir
  change optimistic_lock(:foo)
  change increment(:foo)
  ```
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

  @doc false
  def set_attribute_opts do
    @set_attribute_opts
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

  #{Spark.OptionsHelpers.docs(Keyword.drop(@set_attribute_opts, [:attribute, :value]))}

  ## Examples

  change set_attribute(:active, false)
  change set_attribute(:opened_at, &DateTime.utc_now/0)
  change set_attribute(:status, arg(:status))
  change set_attribute(:encrypted_data, arg(:data), set_when_nil?: false)
  """
  @spec set_attribute(
          relationship :: atom,
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
  """
  @spec atomic_update(attribute :: atom, expr :: Ash.Expr.t()) :: Ash.Resource.Change.ref()
  def atomic_update(attribute, expr) do
    {Ash.Resource.Change.Atomic, attribute: attribute, expr: expr}
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
  Passes the provided value into `changeset.api.load()`, after the action has completed.

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

  @doc ~S"""
  Directly attach an `after_action` function to the current change.

  See `Ash.Changeset.after_action/3` for more information.

  Provide the option `prepend?: true` to place the hook before all other hooks instead of after.

  ## Example

      change after_action(fn changeset, record ->
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
        changeset, {:ok, record} ->
          Logger.debug("Successfully executed transaction for action #{changeset.action.name} on #{inspect(changeset.resource)}")
          {:ok, record}
        changeset, {:error, reason} ->
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

      change before_action(fn changeset ->
        Logger.debug("About to execute #{changeset.action.name} on #{inspect(changeset.resource)})

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

      change before_transaction(fn changeset ->
        Logger.debug("About to execute transaction for #{changeset.action.name} on #{inspect(changeset.resource)})

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
