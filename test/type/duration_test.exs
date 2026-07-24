# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.DurationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  import Ash.Expr
  alias Ash.Query.Operator.Basic

  import Ash.Expr
  alias Ash.Query.Operator.Basic

  @year1 Duration.new!(year: 1)
  @month5 Duration.new!(month: 5)
  @hour1 Duration.new!(hour: 1)
  @minute30 Duration.new!(minute: 30)
  @millisecond1 Duration.new!(microsecond: {1000, 6})

  @today Date.utc_today()
  @datetime_now DateTime.utc_now()
  @naive_datetime_now NaiveDateTime.utc_now()
  @time_now Time.utc_now()

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :duration_a, :duration do
        public?(true)
      end

      attribute :duration_b, :duration, allow_nil?: false, public?: true

      attribute :duration_c, :duration, allow_nil?: true, public?: true

      attribute :duration_d, :duration, allow_nil?: true, public?: true

      attribute :duration_calendar_free, :duration,
        allow_nil?: true,
        public?: true,
        constraints: [units: :day_time]

      attribute :date, :date, allow_nil?: true, public?: true

      attribute :datetime, :datetime, allow_nil?: true, public?: true

      attribute :naive_datetime, :naive_datetime, allow_nil?: true, public?: true

      attribute :time, :time, allow_nil?: true, public?: true

      attribute :time_usec, :time_usec, allow_nil?: true, public?: true

      attribute :utc_datetime, :utc_datetime, allow_nil?: true, public?: true

      attribute :utc_datetime_usec, :utc_datetime_usec, allow_nil?: true, public?: true
    end

    calculations do
      calculate :duration_a_plus_b, :duration, expr(duration_a + duration_b)
      calculate :duration_b_minus_a, :duration, expr(duration_b - duration_a)
      calculate :duration_b_times_three, :duration, expr(duration_b * 3)
      calculate :duration_two_times_b, :duration, expr(2 * duration_b)
      calculate :duration_a_negated, :duration, expr(-duration_a)
      calculate :date_plus_duration_d, :date, expr(datetime + duration_d)
      calculate :date_minus_duration_d, :date, expr(datetime - duration_d)
      calculate :datetime_plus_duration_a, :datetime, expr(datetime + duration_a)
      calculate :datetime_minus_duration_b, :datetime, expr(datetime - duration_b)

      calculate :naive_datetime_plus_duration_a,
                :naive_datetime,
                expr(naive_datetime + duration_a)

      calculate :naive_datetime_minus_duration_b,
                :naive_datetime,
                expr(naive_datetime - duration_b)

      calculate :time_plus_duration_a, :time, expr(time + duration_a)
      calculate :time_minus_duration_b, :time, expr(time - duration_b)
      calculate :time_usec_plus_duration_c, :time_usec, expr(time_usec + duration_c)
      calculate :time_usec_minus_duration_c, :time_usec, expr(time_usec - duration_c)
      calculate :utc_datetime_plus_duration_a, :utc_datetime, expr(utc_datetime + duration_a)
      calculate :utc_datetime_minus_duration_b, :utc_datetime, expr(utc_datetime - duration_b)

      calculate :utc_datetime_usec_plus_duration_c,
                :utc_datetime_usec,
                expr(utc_datetime_usec + duration_c)

      calculate :utc_datetime_usec_minus_duration_c,
                :utc_datetime_usec,
                expr(utc_datetime_usec - duration_c)
    end
  end

  describe "units constraint" do
    @calendar_free [:week, :day, :hour, :minute, :second, :microsecond]

    test "with no constraint, any unit is permitted" do
      assert {:ok, _} = Ash.Type.Duration.apply_constraints(Duration.new!(year: 1, month: 2), [])
    end

    test "accepts durations that use only whitelisted units" do
      assert {:ok, _} =
               Ash.Type.Duration.apply_constraints(
                 Duration.new!(day: 3, hour: 4, minute: 30),
                 units: @calendar_free
               )

      assert {:ok, _} =
               Ash.Type.Duration.apply_constraints(Duration.new!(week: 2), units: @calendar_free)
    end

    test "rejects durations that use a non-whitelisted unit" do
      assert {:error, [[message: message, units: _, disallowed: disallowed]]} =
               Ash.Type.Duration.apply_constraints(
                 Duration.new!(month: 1, day: 3),
                 units: @calendar_free
               )

      assert message =~ "must only use the units"
      assert disallowed =~ "month"
    end

    test "the :day_time shorthand accepts day/time units and rejects year/month" do
      assert {:ok, _} =
               Ash.Type.Duration.apply_constraints(Duration.new!(day: 3, hour: 4),
                 units: :day_time
               )

      assert {:error, [[message: _, units: units, disallowed: disallowed]]} =
               Ash.Type.Duration.apply_constraints(Duration.new!(month: 1), units: :day_time)

      # the reported permitted units are the expanded list, not the shorthand atom
      assert units =~ "week"
      assert disallowed =~ "month"
    end

    test "the :year_month shorthand accepts year/month units and rejects day/time" do
      assert {:ok, _} =
               Ash.Type.Duration.apply_constraints(
                 Duration.new!(year: 2, month: 6),
                 units: :year_month
               )

      assert {:error, [[message: _, units: units, disallowed: disallowed]]} =
               Ash.Type.Duration.apply_constraints(Duration.new!(day: 1), units: :year_month)

      assert units =~ "year"
      assert disallowed =~ "day"
    end

    test "treats the microsecond precision tuple as zero/non-zero on its value only" do
      assert {:ok, _} =
               Ash.Type.Duration.apply_constraints(
                 Duration.new!(second: 5, microsecond: {0, 6}),
                 units: [:second]
               )

      assert {:error, _} =
               Ash.Type.Duration.apply_constraints(
                 Duration.new!(second: 5, microsecond: {1, 6}),
                 units: [:second]
               )
    end

    test "nil passes regardless of constraint" do
      assert {:ok, nil} = Ash.Type.Duration.apply_constraints(nil, units: [:day])
    end

    test "does not support atomic updates when a units constraint is set" do
      refute Ash.Type.Duration.may_support_atomic_update?(units: @calendar_free)
      assert Ash.Type.Duration.may_support_atomic_update?([])
    end

    test "is enforced when casting through a resource attribute" do
      assert {:error, _} =
               Post
               |> Ash.Changeset.for_create(:create, %{
                 duration_b: @minute30,
                 duration_calendar_free: @month5
               })
               |> Ash.create()

      assert {:ok, post} =
               Post
               |> Ash.Changeset.for_create(:create, %{
                 duration_b: @minute30,
                 duration_calendar_free: Duration.new!(day: 3, hour: 12)
               })
               |> Ash.create()

      assert post.duration_calendar_free == Duration.new!(day: 3, hour: 12)
    end
  end

  test "it handles non-empty values" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        duration_a: @hour1,
        duration_b: @minute30
      })
      |> Ash.create!()

    assert post.duration_a == @hour1
    assert post.duration_b == @minute30
    assert post.duration_c == nil
  end

  describe "functions resulting in duration" do
    test "minus function performs negation" do
      assert Ash.Query.Function.Minus.evaluate(%{arguments: [@year1]}) ==
               {:known, Duration.new!(year: -1)}
    end
  end

  describe "operators resulting in duration" do
    test "plus operator performs addition of two durations" do
      assert Basic.Plus.evaluate(%{left: @year1, right: @month5}) ==
               {:known, Duration.add(@year1, @month5)}
    end

    test "minus operator performs subtraction of two durations" do
      assert Basic.Minus.evaluate(%{left: @year1, right: @month5}) ==
               {:known, Duration.subtract(@year1, @month5)}
    end

    test "times operator performs duration times integer" do
      assert Basic.Times.evaluate(%{left: @year1, right: 2}) ==
               {:known, Duration.multiply(@year1, 2)}
    end
  end

  describe "operators on other temporal types with duration" do
    test "plus operator performs addition of duration to date" do
      assert Basic.Plus.evaluate(%{left: @today, right: @year1}) ==
               {:known, Date.shift(@today, @year1)}
    end

    test "minus operator performs subtraction of duration from date" do
      assert Basic.Minus.evaluate(%{left: @today, right: @year1}) ==
               {:known, Date.shift(@today, Duration.negate(@year1))}
    end

    test "plus operator performs addition of duration to datetime" do
      assert Basic.Plus.evaluate(%{left: @datetime_now, right: @year1}) ==
               {:known, DateTime.shift(@datetime_now, @year1)}
    end

    test "minus operator performs subtraction of duration from datetime" do
      assert Basic.Minus.evaluate(%{left: @datetime_now, right: @year1}) ==
               {:known, DateTime.shift(@datetime_now, Duration.negate(@year1))}
    end

    test "plus operator performs addition of duration to naive_datetime" do
      assert Basic.Plus.evaluate(%{left: @naive_datetime_now, right: @year1}) ==
               {:known, NaiveDateTime.shift(@naive_datetime_now, @year1)}
    end

    test "minus operator performs subtraction of duration from naive_datetime" do
      assert Basic.Minus.evaluate(%{left: @naive_datetime_now, right: @year1}) ==
               {:known, NaiveDateTime.shift(@naive_datetime_now, Duration.negate(@year1))}
    end

    test "plus operator performs addition of duration to time" do
      assert Basic.Plus.evaluate(%{left: @time_now, right: @minute30}) ==
               {:known, Time.shift(@time_now, @minute30)}
    end

    test "minus operator performs subtraction of duration from time" do
      assert Basic.Minus.evaluate(%{left: @time_now, right: @minute30}) ==
               {:known, Time.shift(@time_now, Duration.negate(@minute30))}
    end
  end

  test "calculations" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        duration_a: @hour1,
        duration_b: @minute30,
        duration_c: @millisecond1,
        duration_d: @year1,
        date: @today,
        datetime: @datetime_now,
        naive_datetime: @naive_datetime_now,
        time: @time_now,
        time_usec: @time_now,
        utc_datetime: @datetime_now,
        utc_datetime_usec: @datetime_now
      })
      |> Ash.create!()
      |> Ash.load!([
        :duration_a_plus_b,
        :duration_b_minus_a,
        :duration_b_times_three,
        :duration_two_times_b,
        :duration_a_negated,
        :date_plus_duration_d,
        :date_minus_duration_d,
        :datetime_plus_duration_a,
        :datetime_minus_duration_b,
        :naive_datetime_plus_duration_a,
        :naive_datetime_minus_duration_b,
        :time_plus_duration_a,
        :time_minus_duration_b,
        :time_usec_plus_duration_c,
        :time_usec_minus_duration_c,
        :utc_datetime_plus_duration_a,
        :utc_datetime_minus_duration_b,
        :utc_datetime_usec_plus_duration_c,
        :utc_datetime_usec_minus_duration_c
      ])

    assert post.duration_a_plus_b == %Duration{hour: 1, minute: 30}
    assert post.duration_b_minus_a == %Duration{hour: -1, minute: 30}
    assert post.duration_b_times_three == %Duration{minute: 90}
    assert post.duration_two_times_b == %Duration{minute: 60}
    assert post.duration_a_negated == %Duration{hour: -1}
    assert post.date_plus_duration_d == Date.shift(@today, @year1)
    assert post.date_minus_duration_d == Date.shift(@today, Duration.negate(@year1))

    assert post.datetime_plus_duration_a ==
             DateTime.truncate(DateTime.shift(@datetime_now, @hour1), :second)

    assert post.datetime_minus_duration_b ==
             DateTime.truncate(DateTime.shift(@datetime_now, Duration.negate(@minute30)), :second)

    assert post.naive_datetime_plus_duration_a ==
             NaiveDateTime.truncate(NaiveDateTime.shift(@naive_datetime_now, @hour1), :second)

    assert post.naive_datetime_minus_duration_b ==
             NaiveDateTime.truncate(
               NaiveDateTime.shift(@naive_datetime_now, Duration.negate(@minute30)),
               :second
             )

    assert post.time_plus_duration_a == Time.truncate(Time.shift(@time_now, @hour1), :second)

    assert post.time_minus_duration_b ==
             Time.truncate(Time.shift(@time_now, Duration.negate(@minute30)), :second)

    assert post.time_usec_plus_duration_c == Time.shift(@time_now, @millisecond1)

    assert post.time_usec_minus_duration_c ==
             Time.shift(@time_now, Duration.negate(@millisecond1))

    assert post.utc_datetime_plus_duration_a ==
             DateTime.truncate(DateTime.shift(@datetime_now, @hour1), :second)

    assert post.utc_datetime_minus_duration_b ==
             DateTime.truncate(DateTime.shift(@datetime_now, Duration.negate(@minute30)), :second)

    assert post.utc_datetime_usec_plus_duration_c == DateTime.shift(@datetime_now, @millisecond1)

    assert post.utc_datetime_usec_minus_duration_c ==
             DateTime.shift(@datetime_now, Duration.negate(@millisecond1))
  end
end
