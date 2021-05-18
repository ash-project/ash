defmodule Ash.Resource.Change.Builtins do
  @moduledoc """
  Built in changes that are available to all resources

  The functions in this module are imported by default in the actions section.
  """

  @doc "Relates the actor to the data being changed, as the provided relationship."
  def relate_actor(relationship) do
    {Ash.Resource.Change.RelateActor, relationship: relationship}
  end

  @doc """
  Sets the attribute to the value provided. If a zero argument function is provided, it is called to determine the value.
  """
  def set_attribute(attribute, value) do
    {Ash.Resource.Change.SetAttribute, attribute: attribute, value: value}
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

  A special `:ignore?` option can be passed to tell the change not to actually happen.
  This is useful specifically if you have tooling that looks for `manage_relationship` changes,
  like `ash_admin`, but you want to actually handle the input yourself.
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
end
