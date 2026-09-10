# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Authorizer.MultipleAuthorizersTest do
  @moduledoc """
  Authorizing a changeset against a base query returns `{:ok, true, changeset, query}`.

  That four element shape must flow through every authorizer on the resource. Each
  authorizer must also alter the query that the authorizer before it produced.
  """
  use ExUnit.Case, async: true

  require Ash.Expr

  defmodule NotArchivedAuthorizer do
    @moduledoc false
    @behaviour Ash.Authorizer

    @impl true
    def initial_state(actor, _resource, _action, _domain), do: %{actor: actor}

    @impl true
    def strict_check_context(_), do: []

    @impl true
    def strict_check(state, _context), do: {:authorized, state}

    @impl true
    def check_context(_), do: []

    @impl true
    def check(_state, _context), do: :authorized

    @impl true
    def alter_filter(filter, _state, context) do
      addition = Ash.Filter.parse!(context.resource, Ash.Expr.expr(archived == false))

      {:ok, Ash.Filter.add_to_filter!(filter, addition)}
    end
  end

  defmodule NotHiddenAuthorizer do
    @moduledoc false
    @behaviour Ash.Authorizer

    @impl true
    def initial_state(actor, _resource, _action, _domain), do: %{actor: actor}

    @impl true
    def strict_check_context(_), do: []

    @impl true
    def strict_check(state, _context), do: {:authorized, state}

    @impl true
    def check_context(_), do: []

    @impl true
    def check(_state, _context), do: :authorized

    @impl true
    def alter_filter(filter, _state, context) do
      addition = Ash.Filter.parse!(context.resource, Ash.Expr.expr(hidden == false))

      {:ok, Ash.Filter.add_to_filter!(filter, addition)}
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain, validate_config_inclusion?: false

    resources do
      allow_unregistered? true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [NotArchivedAuthorizer, NotHiddenAuthorizer]

    ets do
      private? true
    end

    actions do
      defaults [:read, create: [:body], update: [:body]]
    end

    attributes do
      uuid_primary_key :id
      attribute :body, :string, public?: true
      attribute :archived, :boolean, default: false, public?: true
      attribute :hidden, :boolean, default: false, public?: true
    end
  end

  test "every authorizer alters the base query a changeset is authorized against" do
    record = Ash.create!(Post, %{body: "before"}, authorize?: false)

    changeset = Ash.Changeset.for_update(record, :update, %{body: "after"})

    assert {:ok, true, %Ash.Changeset{}, %Ash.Query{} = query} =
             Ash.can(changeset, nil,
               alter_source?: true,
               run_queries?: false,
               base_query: Ash.Query.for_read(Post, :read),
               maybe_is: false,
               return_forbidden_error?: true
             )

    filter = inspect(query.filter)

    assert filter =~ "archived == false"
    assert filter =~ "hidden == false"
  end
end
