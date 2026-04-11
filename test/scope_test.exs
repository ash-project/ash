# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.ScopeTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule MyScope do
    defstruct [:actor, :tenant]

    defimpl Ash.Scope.ToOpts do
      def get_actor(%{actor: actor}), do: {:ok, actor}
      def get_tenant(%{tenant: tenant}), do: {:ok, tenant}
      def get_context(_), do: :error
      def get_tracer(_), do: :error
      def get_authorize?(_), do: {:ok, false}
    end
  end

  defmodule MultiTenantResource do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    multitenancy do
      strategy :attribute
      attribute :tenant_id
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :tenant_id, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end
  end

  describe "Ash.Scope.to_opts/1 with Map" do
    test "handles nested shared context pattern" do
      context = %{
        actor: :some_actor,
        context: %{
          shared: %{application: "test_app", user_id: 123}
        }
      }

      opts = Ash.Scope.to_opts(context)

      assert Keyword.has_key?(opts, :actor)
      assert Keyword.has_key?(opts, :context)
      assert opts[:actor] == :some_actor
      assert opts[:context] == %{shared: %{application: "test_app", user_id: 123}}
    end

    test "handles top-level shared context pattern" do
      context = %{
        actor: :some_actor,
        shared: %{application: "test_app", user_id: 123}
      }

      opts = Ash.Scope.to_opts(context)

      assert Keyword.has_key?(opts, :actor)
      assert Keyword.has_key?(opts, :context)
      assert opts[:actor] == :some_actor
      assert opts[:context] == %{shared: %{application: "test_app", user_id: 123}}
    end

    test "handles empty shared context" do
      context = %{
        actor: :some_actor,
        shared: %{}
      }

      opts = Ash.Scope.to_opts(context)

      assert Keyword.has_key?(opts, :actor)
      assert Keyword.has_key?(opts, :context)
      assert opts[:actor] == :some_actor
      assert opts[:context] == %{shared: %{}}
    end

    test "handles context without shared key" do
      context = %{
        actor: :some_actor,
        tenant: "tenant_123"
      }

      opts = Ash.Scope.to_opts(context)

      assert Keyword.has_key?(opts, :actor)
      assert Keyword.has_key?(opts, :tenant)
      refute Keyword.has_key?(opts, :context)
      assert opts[:actor] == :some_actor
      assert opts[:tenant] == "tenant_123"
    end

    test "prioritizes top-level shared over nested context" do
      context = %{
        actor: :some_actor,
        shared: %{application: "top_level"},
        context: %{
          shared: %{application: "nested"}
        }
      }

      opts = Ash.Scope.to_opts(context)

      assert opts[:context] == %{shared: %{application: "top_level"}}
    end

    test "handles all standard context keys" do
      context = %{
        actor: :test_actor,
        tenant: "test_tenant",
        authorize?: true,
        tracer: :test_tracer,
        shared: %{custom_data: "test"}
      }

      opts = Ash.Scope.to_opts(context)

      assert opts[:actor] == :test_actor
      assert opts[:tenant] == "test_tenant"
      assert opts[:authorize?] == true
      assert opts[:tracer] == :test_tracer
      assert opts[:context] == %{shared: %{custom_data: "test"}}
    end
  end

  describe "scope with update (issue #2662)" do
    test "scope tenant is used when record metadata has no tenant" do
      record =
        MultiTenantResource
        |> Ash.Changeset.for_create(:create, %{name: "original", tenant_id: "tenant_1"},
          tenant: "tenant_1"
        )
        |> Ash.create!()

      # Simulate a record whose metadata does not have a tenant set
      # (e.g. loaded from a context where tenant wasn't propagated)
      record = put_in(record.__metadata__[:tenant], nil)

      scope = %MyScope{actor: nil, tenant: "tenant_1"}

      assert {:ok, updated} =
               Ash.update(record, %{name: "updated"}, scope: scope)

      assert updated.name == "updated"
    end

    test "scope tenant is used via Ash.Scope.to_opts workaround" do
      record =
        MultiTenantResource
        |> Ash.Changeset.for_create(:create, %{name: "original", tenant_id: "tenant_1"},
          tenant: "tenant_1"
        )
        |> Ash.create!()

      record = put_in(record.__metadata__[:tenant], nil)

      scope = %MyScope{actor: nil, tenant: "tenant_1"}
      opts = Ash.Scope.to_opts(scope, action: :update)

      assert {:ok, updated} = Ash.update(record, %{name: "updated"}, opts)
      assert updated.name == "updated"
    end
  end

  describe "scope with destroy (issue #2662)" do
    test "scope tenant is used when record metadata has no tenant" do
      record =
        MultiTenantResource
        |> Ash.Changeset.for_create(:create, %{name: "to_delete", tenant_id: "tenant_1"},
          tenant: "tenant_1"
        )
        |> Ash.create!()

      record = put_in(record.__metadata__[:tenant], nil)

      scope = %MyScope{actor: nil, tenant: "tenant_1"}

      assert :ok = Ash.destroy(record, scope: scope)
    end
  end

  describe "Ash.Context.to_opts/1 (deprecated)" do
    test "delegates to Ash.Scope.to_opts/1" do
      context = %{
        actor: :some_actor,
        shared: %{application: "test_app"}
      }

      scope_opts = Ash.Scope.to_opts(context)
      context_opts = Ash.Context.to_opts(context)

      assert scope_opts == context_opts
    end
  end
end
