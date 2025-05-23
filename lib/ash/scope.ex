defprotocol Ash.Scope do
  @moduledoc """
  Determines how the `actor`, `tenant` and `context` are extracted from a data structure.

  This is inspired by the same feature in `Phoenix`, however the `actor`, `tenant` and `context`
  options will always remain available, as they are standardized representations of things that
  actions can use to do their work. This exists as a convenience, especially for those who are
  using an application with a version of Phoenix that has a concept of scopes.

  ## Example

  You would implement `Ash.Scope` for a module like so:

  ```elixir
  defmodule MyApp.Scope do
    defstruct [:current_user, :current_tenant, :locale]

    defimpl Ash.Scope do
      def get_actor(%{current_user: current_user}), do: {:ok, current_user}
      def get_tenant(%{current_tenant: current_tenant}), do: {:ok, current_tenant}
      def get_context(%{locale: locale}), do: {:ok, %{locale: locale}}
    end
  end
  ```

  You could then use this in various places by passing the `scope` option.

  For example:

  ```elixir
  scope = %MyApp.Scope{...}
  # with code interfaces
  MyApp.Blog.create_post!("new post", scope: scope)

  # with changesets and queries
  MyApp.Blog
  |> Ash.Changeset.for_create(:create, %{title: "new post"}, scope: scope)
  |> Ash.create!()

  # with the context structs that we provide

  def change(changeset, _, context) do
    Ash.Changeset.after_action(changeset, fn changeset, result ->
      MyApp.Domain.do_something_else(..., scope: context)
      # if not using as a scope, the alternative is this
      # in the future this will be deprecated
      MyApp.Domain.do_somethign_else(..., Ash.Context.to_opts(context))
    end)
  end
  ```

  Extensions should not use this option, only end users.
  """

  @type t :: term()

  @doc "Extracts the actor from the scope"
  @spec get_actor(term()) :: {:ok, term} | :error
  def get_actor(scope)

  @doc "Extracts the tenant from the scope"
  @spec get_tenant(term()) :: {:ok, term} | :error
  def get_tenant(scope)

  @doc "Extracts the context from the scope"
  @spec get_context(term()) :: {:ok, term} | :error
  def get_context(scope)
end

defimpl Ash.Scope,
  for: [
    Ash.Resource.Actions.Implementation.Context,
    Ash.Resource.Calculation.Context,
    Ash.Resource.Change.Context,
    Ash.Resource.ManualCreate.Context,
    Ash.Resource.ManualDestroy.Context,
    Ash.Resource.ManualUpdate.Context,
    Ash.Resource.ManualRelationship.Context,
    Ash.Resource.Preparation.Context,
    Ash.Resource.Validation.Context
  ] do
  def get_actor(%{actor: actor}), do: {:ok, actor}
  def get_tenant(%{tenant: tenant}), do: {:ok, tenant}
  def get_context(%{context: context}), do: {:ok, context || %{}}
end
