defmodule Ash.Resource.Change.Builtins do
  @moduledoc """
  Built in changes that are available to all resources

  The functions in this module are imported by default in the actions section.
  """

  @doc """
  Relates the actor to the data being changed, as the provided relationship.
  Accepts the option `:allow_nil?`, which will not force an actor to be set.
  `:allow_nil?` defaults to `false`.
  """
  def relate_actor(relationship, opts \\ []) do
    opts =
      opts
      |> Keyword.put(:relationship, relationship)
      |> Keyword.put_new(:allow_nil?, false)

    {Ash.Resource.Change.RelateActor, opts}
  end

  @doc """
  Sets the attribute to the value provided.

  If a zero argument function is provided, it is called to determine the value.

  If a `arg(:arg_name)` is provided, the value will be read from the argument if supplied.
  If the argument specified is not given to the action, then nothing happens.
  """
  def set_attribute(attribute, value) do
    {Ash.Resource.Change.SetAttribute, attribute: attribute, value: value}
  end

  @doc """
  Sets the attribute to the value provided if the attribtue is not already being changed.

  If a zero argument function is provided, it is called to determine the value.

  Use `arg(:argument_name)` to use the value of the given argument. If the argument is not supplied then nothing happens.
  """
  def set_new_attribute(attribute, value) do
    {Ash.Resource.Change.SetAttribute, attribute: attribute, value: value, new?: true}
  end

  @doc """
  Clears a change off of the changeset before the action runs.

  Useful if a change is only used in validations but shouldn't ultimately be written to the data layer
  """
  def prevent_change(field) do
    {Ash.Resource.Change.PreventChange, field: field}
  end

  @doc """
  Calls `Ash.Changeset.manage_relationship/4` with the changeset and relationship provided, using the value provided for the named argument

  For example

  ```elixir
  change manage_relationship(:add_comments, :comments, on_missing: :ignore, on_match: :no_match, on_no_match: {:create, :add_comment_to_post}
  ```
  """
  def manage_relationship(argument, relationship_name \\ nil, opts) do
    relationship_name = relationship_name || argument

    {Ash.Resource.Change.ManageRelationship,
     [argument: argument, relationship: relationship_name, opts: opts]}
  end

  @doc """
  Merges the given query context. If an MFA is provided, it will be called with the changeset.

  The MFA should return `{:ok, context_to_be_merged}` or `{:error, term}`
  """
  @spec set_context(map | mfa) ::
          {atom, Keyword.t()}
  def set_context(context) do
    {Ash.Resource.Change.SetContext, context: context}
  end

  @doc """
  Passes the provided value into `changeset.api.load()`, after the action has completed.
  """
  def load(value) do
    {Ash.Resource.Change.Load, target: value}
  end

  @doc """
  Passes the provided value into `Ash.Changeset.select/3`
  """
  def select(value) do
    {Ash.Resource.Change.Select, target: value}
  end

  @doc """
  Passes the provided value into `Ash.Changeset.ensure_selected/2`
  """
  def ensure_selected(value) do
    {Ash.Resource.Change.Select, target: value, ensure?: true}
  end
end
