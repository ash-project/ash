# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.ContextSharedTest do
  @moduledoc """
  Verifies that a policy filter referencing `^context([:shared, ...])` resolves
  against the *parent* query's shared context wherever a child resource is
  authorized as a side effect of the parent read — direct reads, has_many
  loads, and aggregates. Regression coverage for context.shared propagation
  through the aggregate/relationship authorization path.
  """
  use ExUnit.Case, async: false

  require Ash.Query

  defmodule Child do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.ContextSharedTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:dept_id, :integer, allow_nil?: false, public?: true)
      attribute(:amt, :integer, allow_nil?: false, public?: true)
    end

    relationships do
      belongs_to(:parent, Ash.Test.Policy.ContextSharedTest.Parent,
        public?: true,
        allow_nil?: false
      )
    end

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    policies do
      policy action_type(:read) do
        authorize_if(expr(dept_id == ^context([:shared, :dept_id])))
      end

      policy action_type([:create, :update, :destroy]) do
        authorize_if(always())
      end
    end
  end

  defmodule Parent do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.ContextSharedTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:dept_id, :integer, allow_nil?: false, public?: true)
    end

    relationships do
      has_many(:children, Child)
    end

    aggregates do
      sum(:children_total, :children, :amt) do
        default(0)
      end
    end

    calculations do
      calculate(:has_any_child, :boolean, expr(exists(children, true)))
    end

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    policies do
      policy always() do
        authorize_if(always())
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource(Parent)
      resource(Child)
    end
  end

  describe "child policy referencing ^context([:shared, ...])" do
    setup do
      dept_id = 1
      ash_opts = [context: %{shared: %{dept_id: dept_id}}, authorize?: true]

      {:ok, parent} =
        Parent
        |> Ash.Changeset.for_create(:create, %{dept_id: dept_id})
        |> Ash.create(authorize?: false)

      {:ok, _matching_child} =
        Child
        |> Ash.Changeset.for_create(:create, %{
          dept_id: dept_id,
          amt: 11_000,
          parent_id: parent.id
        })
        |> Ash.create(authorize?: false)

      {:ok, _non_matching_child} =
        Child
        |> Ash.Changeset.for_create(:create, %{
          dept_id: dept_id + 1,
          amt: 999,
          parent_id: parent.id
        })
        |> Ash.create(authorize?: false)

      %{parent: parent, dept_id: dept_id, ash_opts: ash_opts}
    end

    test "direct read of child filters by context.shared", %{ash_opts: ash_opts} do
      assert {:ok, [%Child{amt: 11_000}]} = Ash.read(Child, ash_opts)
    end

    test "has_many load on parent filters child by context.shared",
         %{parent: parent, ash_opts: ash_opts} do
      loaded = Ash.load!(parent, [:children], ash_opts)
      assert [%Child{amt: 11_000}] = loaded.children
    end

    test "sum aggregate on parent applies context.shared to its source query",
         %{parent: parent, ash_opts: ash_opts} do
      loaded = Ash.load!(parent, [:children_total], ash_opts)
      assert loaded.children_total == 11_000
    end

    test "parent filter referencing children applies context.shared to child policy",
         %{parent: parent, ash_opts: ash_opts} do
      [filtered] =
        Parent
        |> Ash.Query.filter(exists(children, amt == 11_000))
        |> Ash.read!(ash_opts)

      assert filtered.id == parent.id
    end

    # Pins the intentional aggregate-vs-calc divergence in the same setup.
    # See "filter policies bypassed for calculations" in
    # `test/policy/simple_test.exs` — calc paths bypass child read policies
    # on purpose.
    test "calculation referencing children diverges from aggregate (intentional bypass)",
         %{parent: parent, ash_opts: ash_opts} do
      loaded = Ash.load!(parent, [:children_total, :has_any_child], ash_opts)

      assert loaded.children_total == 11_000
      assert loaded.has_any_child == false
    end
  end
end
