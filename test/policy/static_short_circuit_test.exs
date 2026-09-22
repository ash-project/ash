# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.StaticShortCircuitTest do
  @moduledoc false
  # Deliberately synchronous: the Crux call counter below is VM-global.
  use ExUnit.Case, async: false

  defmodule TrackingCheck do
    @moduledoc false
    use Ash.Policy.SimpleCheck

    def describe(opts), do: "tracking check #{inspect(opts[:label])}"

    def match?(_actor, _context, opts) do
      send(self(), {:evaluated, opts[:label]})
      opts[:result]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:title, :string, allow_nil?: false, public?: true)
    end

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])

      read :visible do
        filter expr(title == "visible")
      end
    end

    policies do
      bypass action_type(:read) do
        authorize_if {TrackingCheck, label: :bypass_body, result: false}
      end

      policy action(:visible) do
        authorize_if expr(title == "visible")
      end

      policy action_type(:read) do
        forbid_if {TrackingCheck, label: :read_forbid, result: false}
        authorize_if {TrackingCheck, label: :read_authorize, result: true}
      end

      policy action_type(:create) do
        authorize_if {TrackingCheck, label: :create_body, result: false}
      end

      policy action_type([:update, :destroy]) do
        authorize_if {TrackingCheck, label: :write_body, result: true}
      end
    end
  end

  defmodule OnlyImpossiblePolicy do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    actions do
      default_accept(:*)
      defaults([:read, create: :*])
    end

    policies do
      # `cond and not cond`: statically false, so the conditions never need to run.
      # (Checked through a create: forbidden reads deliberately re-evaluate
      # policy conditions to decide between an error and an empty filter.)
      policy [
        {TrackingCheck, label: :impossible_c1, result: true},
        {TrackingCheck, label: :impossible_c2, result: false}
      ] do
        forbid_if always()
      end
    end
  end

  @simplify {Crux.Expression, :simplify, 1}

  setup do
    Code.ensure_loaded!(Crux.Expression)
    :erlang.trace_pattern(@simplify, true, [:call_count])
    on_exit(fn -> :erlang.trace_pattern(@simplify, false, [:call_count]) end)
    :ok
  end

  defp simplify_calls(fun) do
    :erlang.trace_pattern(@simplify, :restart, [:call_count])
    result = fun.()
    {:call_count, count} = :erlang.trace_info(@simplify, :call_count)
    {result, count}
  end

  describe "when every check resolves during strict checking" do
    test "an authorized result needs no runtime simplification and no solver" do
      {result, simplifies} = simplify_calls(fn -> Ash.can?({Post, :read}, nil) end)

      assert result
      # The expression was built and simplified at compile time.
      assert simplifies == 0
      assert_received {:evaluated, :bypass_body}
      assert_received {:evaluated, :read_forbid}
      assert_received {:evaluated, :read_authorize}
      refute_received {:evaluated, _}
    end

    test "a forbidden result needs no runtime simplification" do
      {result, simplifies} =
        simplify_calls(fn -> Ash.can?({Post, :create, %{title: "title"}}, nil) end)

      refute result
      assert simplifies == 0
    end

    test "checks in policies whose condition is false are never evaluated" do
      post = Ash.create!(Post, %{title: "title"}, authorize?: false)

      assert Ash.can?({post, :update, %{title: "new"}}, nil)
      assert Ash.can?({post, :destroy}, nil)

      assert_received {:evaluated, :write_body}
      assert_received {:evaluated, :write_body}
      refute_received {:evaluated, _}
    end

    test "checks that cannot influence the result are never evaluated" do
      refute Ash.can?({OnlyImpossiblePolicy, :create, %{}}, nil)
      refute_received {:evaluated, _}
    end
  end

  describe "when checks remain unknown" do
    test "filter checks still produce a filter" do
      Ash.create!(Post, %{title: "visible"}, authorize?: false)
      Ash.create!(Post, %{title: "hidden"}, authorize?: false)

      assert [%{title: "visible"}] = Ash.read!(Post, action: :visible, actor: nil)
    end
  end
end
