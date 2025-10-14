# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Scope do
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

  > ### Scope is left at the front door {: .info}
  >
  > Your scope is "left at the front door". That is, when you pass a scope to an action, the options
  > are extracted and the scope is removed from those options. Within hooks, you are meant to use
  > the `context` provided to your functions as the new `scope`. This is very important, because
  > you don't want a bunch of your code or extension code having to switch on `if opts[:scope]`,
  > extracting the things that it needs, etc.
  >
  > See [the actions guide](/documentation/topics/actions/actions.md#context) for more information.

  ## Setup

  If you are using Phoenix, you will want to assign your `scope` module in a plug that runs
  after your plugs that determine actor/tenant/context. Then, you will want to add an `on_mount`
  hook for LiveViews that sets your `scope` assign. This is especially true for `AshAuthentication`,
  as it does not currently have a concept of scopes.

  ## Passing scope and options

  For the `actor`, `tenant` and `authorize?`, extracted from scopes, the values from the scope are *discarded* if also present in `opts`.

  i.e `scope: scope, actor: nil` will remove the set actor. `scope: scope, actor: some_other_actor` will set the actor to `some_other_actor`.

  For `context`, the values are deep merged.

  For `tracer`, the value(s) are concatenated into a single list.

  ## Example

  You would implement `Ash.Scope.ToOpts` for a module like so:

  ```elixir
  defmodule MyApp.Scope do
    defstruct [:current_user, :current_tenant, :locale]

    defimpl Ash.Scope.ToOpts do
      def get_actor(%{current_user: current_user}), do: {:ok, current_user}
      def get_tenant(%{current_tenant: current_tenant}), do: {:ok, current_tenant}
      def get_context(%{locale: locale}), do: {:ok, %{shared: %{locale: locale}}}
      # You typically configure tracers in config files
      # so this will typically return :error
      def get_tracer(_), do: :error

      # This should likely always return :error
      # unless you want a way to bypass authorization configured in your scope
      def get_authorize?(_), do: :error
    end
  end
  ```

  For more on context, and what the `shared` key is used for, see the [actions guide](/documentation/topics/actions/actions.md#context)

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
      MyApp.Domain.do_something_else(..., Ash.Context.to_opts(context))
    end)
  end
  ```


  Extensions should not use this option, only end users.
  """
  def to_opts(scope, overrides \\ []) do
    [
      actor: Ash.Scope.ToOpts.get_actor(scope),
      tenant: Ash.Scope.ToOpts.get_tenant(scope),
      context: Ash.Scope.ToOpts.get_context(scope),
      tracer: Ash.Scope.ToOpts.get_tracer(scope),
      authorize?: Ash.Scope.ToOpts.get_authorize?(scope)
    ]
    |> Enum.flat_map(fn
      {key, {:ok, value}} when not is_nil(value) or key == :actor ->
        [{key, value}]

      _ ->
        []
    end)
    |> Keyword.merge(overrides)
  end

  @type t :: Ash.Scope.ToOpts.t()

  defprotocol ToOpts do
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

    @doc "Extracts the `authorize?` option from the scope"
    @spec get_authorize?(term()) :: {:ok, boolean()} | :error
    def get_authorize?(scope)
  end

  defimpl Ash.Scope.ToOpts, for: Ash.Policy.Authorizer do
    def get_actor(%{actor: actor}), do: {:ok, actor}

    def get_tenant(%{subject: %{tenant: tenant}}), do: {:ok, tenant}
    def get_tenant(_), do: :error

    def get_context(%{context: context}),
      do: {:ok, Map.take(context || %{}, [:shared])}

    def get_tracer(%{subject: %{context: %{private: %{tracer: tracer}}}}), do: {:ok, tracer}
    def get_authorize?(_), do: :error
  end

  defimpl Ash.Scope.ToOpts,
    for: [
      Ash.Resource.Actions.Implementation.Context,
      Ash.Resource.Calculation.Context,
      Ash.Resource.Change.Context,
      Ash.Resource.ManualCreate.Context,
      Ash.Resource.ManualCreate.BulkContext,
      Ash.Resource.ManualDestroy.Context,
      Ash.Resource.ManualDestroy.BulkContext,
      Ash.Resource.ManualUpdate.Context,
      Ash.Resource.ManualUpdate.BulkContext,
      Ash.Resource.ManualRelationship.Context,
      Ash.Resource.Preparation.Context,
      Ash.Resource.Validation.Context
    ] do
    def get_actor(%{actor: actor}), do: {:ok, actor}
    def get_tenant(%{tenant: tenant}), do: {:ok, tenant}

    def get_context(%{source_context: source_context}),
      do: {:ok, Map.take(source_context, [:shared])}

    def get_tracer(%{tracer: tracer}), do: {:ok, tracer}
    def get_authorize?(%{authorize?: authorize?}), do: {:ok, authorize?}
  end

  defimpl Ash.Scope.ToOpts, for: Map do
    def get_actor(map), do: Map.fetch(map, :actor)
    def get_tenant(map), do: Map.fetch(map, :tenant)

    def get_context(%{shared: shared0, context: %{shared: shared1}}),
      do: {:ok, %{shared: Map.merge(shared1, shared0)}}

    def get_context(%{shared: shared}), do: {:ok, %{shared: shared}}
    def get_context(%{context: %{shared: shared}}), do: {:ok, %{shared: shared}}
    def get_context(%{context: _}), do: {:ok, %{}}
    def get_context(_), do: :error

    def get_tracer(map), do: Map.fetch(map, :tracer)
    def get_authorize?(map), do: Map.fetch(map, :authorize?)
  end
end
