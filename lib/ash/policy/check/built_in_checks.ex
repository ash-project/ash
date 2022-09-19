defmodule Ash.Policy.Check.Builtins do
  @moduledoc "The global authorization checks built into ash"

  @doc "This check always passes"
  def always do
    {Ash.Policy.Check.Static, result: true}
  end

  @doc "this check never passes"
  def never do
    {Ash.Policy.Check.Static, result: false}
  end

  @doc "This check is true when the action type matches the provided type"
  def action_type(action_type) do
    {Ash.Policy.Check.ActionType, type: action_type}
  end

  @doc "This check is true when the action name matches the provided action name"
  def action(action) do
    {Ash.Policy.Check.Action, action: action}
  end

  @doc "This check is true when there is an actor specified, and false when the actor is `nil`"
  def actor_present do
    Ash.Policy.Check.ActorPresent
  end

  @doc "This check is true when the field is being selected and false when it is not"
  def selecting(attribute) do
    {Ash.Policy.Check.Selecting, attribute: attribute}
  end

  @doc "This check is true when the field or relationship, or path to field, is being loaded and false when it is not"
  def loading(field) do
    {Ash.Policy.Check.Loading, field: field}
  end

  @doc " This check passes if the data relates to the actor via the specified relationship or path of relationships"
  def relates_to_actor_via(relationship_path) do
    {Ash.Policy.Check.RelatesToActorVia, relationship_path: List.wrap(relationship_path)}
  end

  @doc "This check is true when a field on the record matches a specific filter"
  def attribute(attribute, filter) do
    {Ash.Policy.Check.Attribute, attribute: attribute, filter: filter}
  end

  @doc "This check is true when the value of the specified attribute equals the specified value"
  def actor_attribute_equals(attribute, value) do
    {Ash.Policy.Check.ActorAttributeEquals, attribute: attribute, value: value}
  end

  @doc "This check is true when the value of the specified key or path in the changeset or query context equals the specified value"
  def context_equals(key, value) do
    {Ash.Policy.Check.ContextEquals, key: key, value: value}
  end

  @doc """
  This check is true when attribute changes correspond to the provided options.

  Provide a keyword list of options or just an atom representing the attribute.

  For example:

  ```elixir
  # if you are changing both first name and last name
  changing_attributes([:first_name, :last_name])

  # if you are changing first name to fred
  changing_attributes(first_name: [to: "fred"])

  # if you are changing last name from bob
  changing_attributes(last_name: [from: "bob"])

  # if you are changing :first_name at all, last_name from "bob" and middle name from "tom" to "george"
  changing_attributes([:first_name, last_name: [from: "bob"], middle_name: [from: "tom", to: "george]])
  ```
  """
  def changing_attributes(opts) do
    opts =
      Enum.map(opts, fn opt ->
        if is_atom(opt) do
          {opt, []}
        end
      end)

    {Ash.Policy.Check.ChangingAttributes, opts}
  end

  @doc "This check is true when the specified relationship is being changed to the current actor"
  def relating_to_actor(relationship) do
    {Ash.Policy.Check.RelatingToActor, relationship: relationship}
  end

  @doc "This check is true when the specified relationship is changing"
  def changing_relationship(relationship) do
    changing_relationships(List.wrap(relationship))
  end

  @doc "This check is true when the specified relationships are changing"
  def changing_relationships(relationships) do
    {Ash.Policy.Check.ChangingRelationships, relationships: relationships}
  end
end
