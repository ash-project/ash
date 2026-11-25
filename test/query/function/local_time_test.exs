# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.LocalTimeTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  require Ash.Query

  alias Ash.Test.Temporal.Thing

  @anchor ~U[2026-11-01 05:30:00Z]
  @hong_kong "Asia/Hong_Kong"
  @new_york "America/New_York"

  test "local_now returns the current local clock time" do
    before = DateTime.now!(@hong_kong) |> DateTime.to_naive()
    assert {:ok, %NaiveDateTime{} = result} = Ash.Expr.eval(expr(local_now(@hong_kong)))
    after_eval = DateTime.now!(@hong_kong) |> DateTime.to_naive()

    assert NaiveDateTime.compare(result, before) in [:eq, :gt]
    assert NaiveDateTime.compare(result, after_eval) in [:eq, :lt]
  end

  test "local_now and local_today use the local date at as_of" do
    anchor = ~U[2026-06-15 20:00:00.123456Z]

    assert evaluate_as_of(expr(local_now(@hong_kong)), anchor) ==
             {:ok, ~N[2026-06-16 04:00:00.123456]}

    assert evaluate_as_of(expr(local_today(@hong_kong)), anchor) == {:ok, ~D[2026-06-16]}
  end

  test "relative local times apply intervals before converting across daylight saving time" do
    assert evaluate_as_of(expr(local_from_now(1, :day, @new_york)), @anchor) ==
             {:ok, ~N[2026-11-02 00:30:00]}

    assert evaluate_as_of(expr(local_ago(1, :day, @new_york)), @anchor) ==
             {:ok, ~N[2026-10-31 01:30:00]}
  end

  test "relative local times accept durations" do
    duration = Duration.new!(day: 1)

    assert evaluate_as_of(expr(local_from_now(^duration, @new_york)), @anchor) ==
             {:ok, ~N[2026-11-02 00:30:00]}

    assert evaluate_as_of(expr(local_ago(^duration, @new_york)), @anchor) ==
             {:ok, ~N[2026-10-31 01:30:00]}
  end

  test "a read can filter by a local date using a time zone from the record" do
    Ash.create!(Thing, %{name: @hong_kong})

    assert [%Thing{name: @hong_kong}] =
             Thing
             |> Ash.Query.as_of(~U[2026-06-15 20:00:00Z])
             |> Ash.Query.filter(local_today(name) == ^~D[2026-06-16])
             |> Ash.read!()
  end

  test "nil time zones produce nil" do
    assert {:ok, nil} = Ash.Expr.eval(expr(local_now(nil)))
    assert {:ok, nil} = Ash.Expr.eval(expr(local_today(nil)))
  end

  test "an invalid time zone returns an evaluation error" do
    assert {:error, message} = Ash.Expr.eval(expr(local_now("Invalid/Zone")))
    assert message =~ "Invalid/Zone"
    assert message =~ "time_zone_not_found"
  end

  defp evaluate_as_of(expression, as_of) do
    with {:ok, hydrated} <- Ash.Filter.hydrate_refs(expression, %{resource: nil, eval?: true}) do
      hydrated
      |> Ash.Expr.fill_template(context: %{shared: %{as_of: as_of}})
      |> Ash.Expr.eval_hydrated([])
    end
  end
end
