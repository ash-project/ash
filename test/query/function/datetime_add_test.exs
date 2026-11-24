# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.DateTimeAddTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr
  require Ash.Query

  alias Ash.Query.Function.DateTimeAdd
  alias Ash.Test.Domain, as: Domain

  defmodule Event do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:happened_at, :naive_datetime, public?: true)
      attribute(:recorded_at, :utc_datetime_usec, public?: true)
    end

    actions do
      default_accept(:*)
      defaults([:read, :create])
    end
  end

  setup do
    Event
    |> Ash.Changeset.for_create(:create, %{
      happened_at: ~N[2024-01-31 12:00:00],
      recorded_at: ~U[2024-01-31 12:00:00.123456Z]
    })
    |> Ash.create!()

    :ok
  end

  test "accepts a utc_datetime_usec reference" do
    assert [_] =
             Event
             |> Ash.Query.filter(datetime_add(recorded_at, 1, :day) > ^~U[2024-02-01 00:00:00Z])
             |> Ash.read!()
  end

  test "accepts a naive_datetime reference with an interval" do
    assert [_] =
             Event
             |> Ash.Query.filter(datetime_add(happened_at, 1, :month) == ^~N[2024-02-29 12:00:00])
             |> Ash.read!()

    assert [] =
             Event
             |> Ash.Query.filter(datetime_add(happened_at, 1, :hour) > ^~N[2024-01-31 14:00:00])
             |> Ash.read!()
  end

  test "accepts a naive_datetime reference with a duration" do
    assert [_] =
             Event
             |> Ash.Query.filter(
               datetime_add(happened_at, ^Duration.new!(day: 1)) == ^~N[2024-02-01 12:00:00]
             )
             |> Ash.read!()
  end

  describe "datetime_add query function" do
    test "1 year from today" do
      today = DateTime.utc_now()

      assert {:known, %DateTime{} = datetime} =
               DateTimeAdd.evaluate(%{arguments: [today, 1, :year]})

      assert datetime.year == today.year + 1
    end
  end

  describe "datetime_add :duration query function" do
    test "1 year from today" do
      today = DateTime.utc_now()

      assert {:known, %DateTime{} = datetime} =
               DateTimeAdd.evaluate(%{arguments: [today, Duration.new!(year: 1)]})

      assert datetime.year == today.year + 1
    end
  end

  test "evaluates naive datetimes directly" do
    assert {:ok, ~N[2024-01-31 12:00:30]} =
             Ash.Expr.eval(expr(datetime_add(^~N[2024-01-31 12:00:00], 30, :second)))

    assert {:ok, ~N[2024-02-07 12:00:00]} =
             Ash.Expr.eval(expr(datetime_add(^~N[2024-01-31 12:00:00], ^Duration.new!(week: 1))))
  end

  describe "with every argument known" do
    test "is computed before it reaches the data layer" do
      query =
        Ash.Query.filter(Event, recorded_at > datetime_add(^~U[2024-01-31 12:00:00Z], 2, :day))

      assert %Ash.Query.Operator.GreaterThan{right: %DateTime{} = bound} = query.filter.expression
      assert DateTime.compare(bound, ~U[2024-02-02 12:00:00Z]) == :eq
    end

    test "is computed once an argument template is filled" do
      filter =
        expr(recorded_at > datetime_add(^arg(:at), -1, :day))
        |> Ash.Expr.fill_template(args: %{at: ~U[2024-01-31 12:00:00Z]})
        |> then(&Ash.Filter.parse!(Event, &1))

      assert %Ash.Query.Operator.GreaterThan{right: %DateTime{} = bound} = filter.expression
      assert DateTime.compare(bound, ~U[2024-01-30 12:00:00Z]) == :eq
    end

    test "stays an expression when an argument isn't known" do
      query = Ash.Query.filter(Event, recorded_at > datetime_add(now(), 2, :day))

      assert %Ash.Query.Operator.GreaterThan{right: %DateTimeAdd{}} = query.filter.expression
    end
  end

  test "29 February plus a year is 28 February, as in postgres and DateTime.shift/2" do
    assert {:known, ~U[2025-02-28 12:00:00Z]} =
             DateTimeAdd.evaluate(%{arguments: [~U[2024-02-29 12:00:00Z], 1, :year]})

    assert {:known, ~U[2028-02-29 12:00:00Z]} =
             DateTimeAdd.evaluate(%{arguments: [~U[2024-02-29 12:00:00Z], 4, :year]})

    assert DateTime.shift(~U[2024-02-29 12:00:00Z], year: 1) == ~U[2025-02-28 12:00:00Z]
  end
end
