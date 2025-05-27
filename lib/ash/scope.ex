defprotocol Ash.Scope do
  @moduledoc """
  Determines how the `actor`, `tenant` and `context` are extracted from a data structure.

  This is inspired by the same feature in `Phoenix`, however the `actor`, `tenant` and `context`
  options will always remain available, as they are standardized representations of things that
  actions can use to do their work.

  When you have a scope, you can group up actor/tenant/context into one struct and pass that around,
  for example:

  ```elixir
  scope = %MyApp.Scope{current_user: user, current_tenant: tenant, locale: "en"}

  # instead of
  MyDomain.create_thing(actor: current_user, tenant: tenant)

  # you can do
  MyDomain.create_thing(scope: scope)
  ```

  ## Setup

  If you are using Phoenix, you will want to assign your `scope` module in a plug that runs
  after your plugs that determine actor/tenant/context. Then, you will want to add an `on_mount`
  hook for LiveViews that sets your `scope` assign. This is especially true for `AshAuthentication`,
  as it does not currently have a concept of scopes.

  ## Passing scope and options

  For the `actor` and `tenant` extracted from scopes, if you also pass those values via options, i.e:

  `scope: %MyApp.Scope{current_user: user1}, actor: user2`

  if both values are non-nil and are not equal, we raise an error.
  Otherwise, we choose the one that is not nil (or nil if both are nil).

  For `context`, the values are deep merged.

  For `tracer`, the value(s) are concatenated.

  ## Example

  You would implement `Ash.Scope` for a module like so:

  ```elixir
  defmodule MyApp.Scope do
    defstruct [:current_user, :current_tenant, :locale]

    defimpl Ash.Scope do
      def get_actor(%{current_user: current_user}), do: {:ok, current_user}
      def get_tenant(%{current_tenant: current_tenant}), do: {:ok, current_tenant}
      def get_context(%{locale: locale}), do: {:ok, %{locale: locale}}
      # You typically configure tracers in config giles
      # so this will typically return :error
      def get_tracer(_), do: :error
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

  @doc "Extracts the tracer(s) from the scope"
  @spec get_tracer(term()) :: {:ok, module | list(module)} | :error
  def get_tracer(scope)
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
  def get_context(_), do: {:ok, %{}}
  def get_tracer(%{tracer: tracer}), do: {:ok, tracer}
end
