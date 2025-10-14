# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.ScopeTest do
  use ExUnit.Case, async: true

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
