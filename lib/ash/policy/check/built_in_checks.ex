defmodule Ash.Policy.Check.Builtins do
  @moduledoc "The global authorization checks built into ash"

  @doc """
  This check always passes.

  Can be useful for "deny-list" style authorization. For example:

  ```elixir
  policy action_type(:read) do
    forbid_if actor_attribute_equals(:disabled, true)
    forbid_if actor_attribute_equals(:active, false)
    authorize_if always()
  end
  ```

  Without that last clause, the policy would never pass.
  """
  @spec always() :: Ash.Policy.Check.ref()
  def always do
    {Ash.Policy.Check.Static, result: true}
  end

  @doc """
  This check never passes.

  There is, generally speaking, no reason to use this, but it exists for
  completeness sake.
  """
  @spec never() :: Ash.Policy.Check.ref()
  def never do
    {Ash.Policy.Check.Static, result: false}
  end

  @doc """
  This check is true when the action type matches the provided type

  This is useful for writing policies that apply to all actions of a given type.

  For example:

  ```elixir
  policy action_type(:read) do
    authorize_if relates_to_actor_via(:owner)
  end
  ```
  """
  @spec action_type(Ash.Resource.Actions.action_type()) :: Ash.Policy.Check.ref()
  def action_type(action_type) do
    {Ash.Policy.Check.ActionType, type: List.wrap(action_type)}
  end

  @doc """
  This check is true when the action name matches the provided action name or names.

  This is a very common pattern, allowing action-specific policies.
  """
  @spec action(atom | list(atom)) :: Ash.Policy.Check.ref()
  def action(action) do
    {Ash.Policy.Check.Action, action: List.wrap(action)}
  end

  @doc """
  This check is true when the resource name matches the provided resource name or names.
  """
  @spec resource(atom | list(atom)) :: Ash.Policy.Check.ref()
  def resource(resource) do
    {Ash.Policy.Check.Resource, resource: List.wrap(resource)}
  end

  @doc """
  This check is true when there is an actor specified, and false when the actor is `nil`.
  """
  @spec actor_present() :: Ash.Policy.Check.ref()
  def actor_present do
    Ash.Policy.Check.ActorPresent
  end

  @doc """
  This check is true when the field provided is being referenced anywhere in a filter statement.

  This applies to related filters as well. For example:

  ```elixir
  policy actor_attribute_equals(:is_admin, false) do
    forbid_if filtering_on(:email)
    # a path can be provided as well
    forbid_if filtering_on([:owner], :email)
  end
  ```

  The first will return true in situations like:

  ```elixir
  Ash.Query.filter(User, email == "blah")
  Ash.Query.filter(Tweet, author.email == "blah")
  ```

  The second will return true on queries like:

  ```elixir
  Ash.Query.filter(Post, owner.email == "blah")
  Ash.Query.filter(Comment, post.owner.email == "blah")
  ```
  """
  @spec filtering_on(atom | list(atom), atom) :: Ash.Policy.Check.ref()
  @deprecated """
  `filtering_on/2` check is deprecated. Instead, add arguments and add policies that said arguments are set.

  For complex queries, policies on what is being filtered on require multiple authorization passes of
  the same resource, leading to a large amount of typically unnecessary complexity.

  Additionally, they could yield false negatives in some scenarios, and more work would be needed
  to ensure that they don't.
  """
  def filtering_on(path \\ [], field) do
    {Ash.Policy.Check.FilteringOn, path: List.wrap(path), field: field}
  end

  @doc """
  This check is true when the field is being selected and false when it is not.

  This won't affect filters placed on this resource, so you may also want to either:

  - Mark the given field as `filterable? false`
  - Add another check for `filtering_on(:field)`

  For example:

  ```elixir
  policy action_type(:read) do
    # The actor can read and filter on their own email
    authorize_if expr(id == ^actor(:id))

    # No one else can select or filter on their email
    forbid_if selecting(:email)
    forbid_if filtering_on(:email)

    # Otherwise, the policy passes
    authorize_if always()
  end
  ```
  """
  @spec selecting(atom) :: Ash.Policy.Check.ref()
  def selecting(attribute) do
    {Ash.Policy.Check.Selecting, attribute: attribute}
  end

  @doc """
  This check is true when the field or relationship, or path to field, is being loaded and false when it is not.

  This is always false for `create`/`update`/`destroy` actions, because you cannot load fields on those action types.
  """
  @spec loading(atom) :: Ash.Policy.Check.ref()
  def loading(field) do
    {Ash.Policy.Check.Loading, field: field}
  end

  @doc """
  This check is true when the current action is being run "through" a relationship.

  Cases where this happens:

  1. Loading related data
  2. Managing relationships
  3. Aggregating data
  4. Filtering on relationships
  """
  @spec accessing_from(Ash.Resource.t(), atom) :: Ash.Policy.Check.ref()
  def accessing_from(resource, relationship) do
    {Ash.Policy.Check.AccessingFrom, source: resource, relationship: relationship}
  end

  @doc """
  This check passes if the data relates to the actor via the specified relationship or path of relationships.

  For `update` & `destroy` actions, this check will apply to *the original data* before the changes are applied.

  For `create` actions this check is very unlikely to pass. This is because relationships are modified *after* authorization
  happens, not before.

  For example:

  ```elixir
  policy action_type(:read) do
    authorize_if relates_to_actor_via(:owner)

    # Path of relationships:
    authorize_if relates_to_actor_via([:account, :user])

    # When the resource relates to a field of the actor:
    authorize_if relates_to_actor_via(:roles, field: :role)
  end
  ```
  """
  @spec relates_to_actor_via(atom, keyword) :: Ash.Policy.Check.ref()
  def relates_to_actor_via(relationship_path, opts \\ []) do
    field = Keyword.get(opts, :field)

    {
      Ash.Policy.Check.RelatesToActorVia,
      relationship_path: List.wrap(relationship_path), field: field
    }
  end

  @doc """
  This check is true when the value of the specified attribute of the actor equals the specified value.

  This check will *never* pass if the actor does not have the specified key. For example,
  `actor_attribute_equals(:missing_key, nil)`
  """
  @spec actor_attribute_equals(atom, any()) :: Ash.Policy.Check.ref()
  def actor_attribute_equals(attribute, value) do
    {Ash.Policy.Check.ActorAttributeEquals, attribute: attribute, value: value}
  end

  @doc """
  This check is true when the value of the specified key or path in the changeset or query context equals the specified value.
  """
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
      Enum.map(List.wrap(opts), fn opt ->
        if is_atom(opt) do
          {opt, []}
        else
          opt
        end
      end)

    {Ash.Policy.Check.ChangingAttributes, changing: opts}
  end

  @doc """
  This check is true when the specified relationship is being changed to the current actor.

  This only supports `belongs_to` relationships at the moment, and will detect two cases:

  1. the `source_attribute` is being changed directly
  2. the relationship is being changed with `on_lookup?: :relate`, and a single input is being provided.
  """
  def relating_to_actor(relationship) do
    {Ash.Policy.Check.RelatingToActor, relationship: relationship}
  end

  @doc "This check is true when the specified relationship is changing"
  def changing_relationship(relationship) do
    changing_relationships(relationship)
  end

  @doc "This check is true when the specified relationships are changing"
  def changing_relationships(relationships) do
    {Ash.Policy.Check.ChangingRelationships, relationships: List.wrap(relationships)}
  end

  @doc "This check is true when the specified function returns true"
  defmacro matches(description, func) do
    {value, function} = Spark.CodeHelpers.lift_functions(func, :matches_policy_check, __CALLER__)

    quote generated: true do
      unquote(function)

      {Ash.Policy.Check.Matches, description: unquote(description), func: unquote(value)}
    end
  end
end
