# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.TemporalTest do
  use ExUnit.Case, async: true

  require Ash.Expr

  @as_of ~U[2020-06-15 12:00:00.000000Z]

  describe "Ash.Query.as_of/2" do
    test "sets the field and stashes as_of in the shared context so it propagates" do
      query = Ash.Query.as_of(Ash.Test.Temporal.Thing, @as_of)
      assert query.as_of == @as_of
      assert query.context[:shared][:as_of] == @as_of
    end

    test "defaults to nil" do
      assert Ash.Query.new(Ash.Test.Temporal.Thing).as_of == nil
    end

    test "set_context/2 picks up a propagated shared as_of onto the field" do
      # simulates a related query receiving the parent's shared context
      query =
        Ash.Test.Temporal.Thing
        |> Ash.Query.new()
        |> Ash.Query.set_context(%{shared: %{as_of: @as_of}})

      assert query.as_of == @as_of
    end
  end

  describe "Ash.Changeset temporal API" do
    test "as_of/2 sets the field and threads it into the private context" do
      changeset =
        Ash.Test.Temporal.Thing
        |> Ash.Changeset.new()
        |> Ash.Changeset.as_of(@as_of)

      assert changeset.as_of == @as_of
      assert changeset.context[:private][:as_of] == @as_of
    end
  end

  alias Ash.Test.Temporal.Thing

  describe "as_of threaded through builder opts (like tenant)" do
    test "Ash.Query.for_read/4 reads :as_of from opts" do
      query = Ash.Query.for_read(Thing, :read, %{}, as_of: @as_of)
      assert query.as_of == @as_of
      assert query.context[:shared][:as_of] == @as_of
    end

    test "Ash.Changeset.for_create/4 reads :as_of from opts" do
      changeset = Ash.Changeset.for_create(Thing, :create, %{}, as_of: @as_of)
      assert changeset.as_of == @as_of
      assert changeset.context[:private][:as_of] == @as_of
    end

    test "Ash.Changeset.for_update/4 reads :as_of from opts" do
      thing = Ash.create!(Thing, %{})
      changeset = Ash.Changeset.for_update(thing, :update, %{}, as_of: @as_of)
      assert changeset.as_of == @as_of
    end

    test "Ash.ActionInput.for_action/4 reads :as_of from opts" do
      input = Ash.ActionInput.for_action(Thing, :reveal_as_of, %{}, as_of: @as_of)
      assert input.as_of == @as_of
      assert input.context[:private][:as_of] == @as_of
    end

    test ":now is accepted and carried symbolically" do
      query = Ash.Query.for_read(Thing, :read, %{}, as_of: :now)
      assert query.as_of == :now
    end

    test "absent as_of leaves the subject unset (no as_of context key)" do
      query = Ash.Query.for_read(Thing, :read, %{})
      assert query.as_of == nil
      refute Map.has_key?(query.context, :as_of)
    end
  end

  describe "as_of threaded through Ash.* opts (like tenant)" do
    test "Ash.read/2 threads :as_of from opts onto the query" do
      Ash.create!(Thing, %{name: "a"})
      assert {:ok, _} = Ash.read(Thing, action: :capture_as_of, as_of: @as_of)
      assert_received {:captured_as_of, @as_of}
    end

    test "Ash.get/3 threads :as_of from opts onto the query" do
      thing = Ash.create!(Thing, %{name: "g"})
      assert {:ok, _} = Ash.get(Thing, thing.id, action: :capture_as_of, as_of: @as_of)
      assert_received {:captured_as_of, @as_of}
    end

    test "Ash.run_action/2 threads :as_of into the generic action context" do
      assert {:ok, @as_of} =
               Thing
               |> Ash.ActionInput.for_action(:reveal_as_of, %{})
               |> Ash.run_action(as_of: @as_of)
    end

    test "Ash.run_action/1 sees as_of set on the input itself" do
      input = Ash.ActionInput.for_action(Thing, :reveal_as_of, %{}, as_of: @as_of)
      assert {:ok, @as_of} = Ash.run_action(input)
    end
  end

  describe "as_of threaded through code interfaces (like tenant)" do
    test "generic action interface forwards :as_of into the action context" do
      assert {:ok, @as_of} = Thing.reveal_as_of(as_of: @as_of)
    end

    test "read interface forwards :as_of onto the query" do
      Ash.create!(Thing, %{name: "b"})
      assert {:ok, _} = Thing.capture_as_of(as_of: @as_of)
      assert_received {:captured_as_of, @as_of}
    end
  end

  describe "as_of anchors now() in authorization (policies)" do
    alias Ash.Test.Temporal.Gated

    @t_before ~U[2026-01-15 00:00:00.000000Z]
    @t_after ~U[2026-05-01 00:00:00.000000Z]

    setup do
      Gated |> Ash.Changeset.for_create(:create, %{name: "x"}) |> Ash.create!(authorize?: false)
      :ok
    end

    test "read policy now() is evaluated at as_of, not the wall clock" do
      # now() anchored to @t_before (< cutoff) -> policy passes -> record visible.
      # If early eval resolved now() against the wall clock (2026+), this would be [].
      assert [%{name: "x"}] =
               Gated
               |> Ash.Query.for_read(:read, %{}, actor: %{id: 1}, as_of: @t_before)
               |> Ash.read!(authorize?: true)

      # now() anchored to @t_after (>= cutoff) -> policy filters everything out.
      assert [] =
               Gated
               |> Ash.Query.for_read(:read, %{}, actor: %{id: 1}, as_of: @t_after)
               |> Ash.read!(authorize?: true)
    end

    test "Ash.can threads as_of onto the subject it builds from the action" do
      # can builds the read query from {resource, action}; `as_of` from opts must land on it
      # (Part A). `alter_source?` hands back that built query so we can inspect it directly.
      assert {:ok, _, %Ash.Query{as_of: @t_before}} =
               Ash.can({Gated, :read}, %{id: 1}, as_of: @t_before, alter_source?: true)

      assert {:ok, _, %Ash.Query{as_of: @t_after}} =
               Ash.can({Gated, :read}, %{id: 1}, as_of: @t_after, alter_source?: true)
    end

    test "raises when the actor was fetched as of a different instant than the authorization" do
      actor = %{id: 1, __metadata__: %{as_of: @t_after}}

      assert_raise ArgumentError, ~r/Mismatched `as_of`/, fn ->
        Ash.can?({Gated, :read}, actor, as_of: @t_before)
      end
    end

    test "no raise when the actor's as_of matches the authorization as_of" do
      actor = %{id: 1, __metadata__: %{as_of: @t_before}}
      assert Ash.can?({Gated, :read}, actor, as_of: @t_before)
    end

    test "no raise when the actor carries no as_of stamp (e.g. a non-temporal actor)" do
      assert Ash.can?({Gated, :read}, %{id: 1}, as_of: @t_before)
    end
  end

  describe "fill_template anchors relative time to as_of" do
    # `fill_template` runs on already-hydrated expressions, so it matches the
    # `%Function.Now/Ago/FromNow{}` structs (not the raw `%Ash.Query.Call{}` that `expr/1`
    # produces). Construct the hydrated structs directly to exercise the anchoring.
    alias Ash.Query.Function.{Ago, FromNow, Now}

    @anchor ~U[2026-06-15 12:00:00.000000Z]

    defp anchored(template),
      do: Ash.Expr.fill_template(template, context: %{shared: %{as_of: @anchor}})

    test "now() resolves to as_of" do
      assert anchored(%Now{arguments: []}) == @anchor
    end

    test "ago(n, unit) resolves to as_of shifted into the past" do
      assert anchored(%Ago{arguments: [7, :day]}) == Ago.datetime_add(@anchor, -7, :day)
    end

    test "from_now(n, unit) resolves to as_of shifted into the future" do
      assert anchored(%FromNow{arguments: [7, :day]}) == Ago.datetime_add(@anchor, 7, :day)
    end

    test "with no as_of in context, relative time is left intact (wall-clock fallback)" do
      no_ctx = [context: %{}]
      assert %Now{arguments: []} = Ash.Expr.fill_template(%Now{arguments: []}, no_ctx)
      assert %Ago{} = Ash.Expr.fill_template(%Ago{arguments: [7, :day]}, no_ctx)
      assert %FromNow{} = Ash.Expr.fill_template(%FromNow{arguments: [7, :day]}, no_ctx)
    end
  end

  describe "&DateTime.utc_now/0 defaults use as_of" do
    test "a create_timestamp default resolves to the write's as_of, not the wall clock" do
      # Thing has `create_timestamp :inserted_at` (default `&DateTime.utc_now/0`).
      thing =
        Thing
        |> Ash.Changeset.for_create(:create, %{name: "x"}, as_of: @as_of)
        |> Ash.create!()

      assert thing.inserted_at == @as_of
    end

    test "without as_of the default is the wall clock" do
      before = DateTime.utc_now()
      thing = Ash.create!(Thing, %{name: "x"})
      assert DateTime.compare(thing.inserted_at, before) in [:eq, :gt]
    end
  end
end
