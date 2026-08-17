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

  defmodule Embedded do
    @moduledoc false
    use Ash.Resource, data_layer: :embedded

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      attribute :duration, :duration, public?: true
      attribute :hours_only, :duration, public?: true, constraints: [units: [:hour]]
    end
  end

  defmodule Holder do
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
      attribute :embedded, Embedded, public?: true
    end
  end

  describe "embedded resources" do
    test "a duration is written and read back" do
      duration = Duration.new!(week: 1)

      holder =
        Holder
        |> Ash.Changeset.for_create(:create, %{embedded: %{duration: duration}})
        |> Ash.create!()

      assert holder.embedded.duration == duration

      assert [%{embedded: %{duration: ^duration}}] = Ash.read!(Holder)
    end

    test "a nil duration is written and read back" do
      holder =
        Holder
        |> Ash.Changeset.for_create(:create, %{embedded: %{duration: nil}})
        |> Ash.create!()

      assert holder.embedded.duration == nil
    end

    # ISO 8601 round trips faithfully, so this is for consistency, not repair.
    test "an embedded duration is normalized, and a read agrees with the write" do
      holder =
        Holder
        |> Ash.Changeset.for_create(:create, %{embedded: %{duration: Duration.new!(hour: 36)}})
        |> Ash.create!()

      assert holder.embedded.duration == Duration.new!(day: 1, hour: 12)

      assert [%{embedded: %{duration: read_back}}] = Ash.read!(Holder)
      assert read_back == holder.embedded.duration
    end

    test "an embedded duration honours its units constraint" do
      # converted, because 2 days is exactly 48 hours
      holder =
        Holder
        |> Ash.Changeset.for_create(:create, %{embedded: %{hours_only: Duration.new!(day: 2)}})
        |> Ash.create!()

      assert holder.embedded.hours_only == Duration.new!(hour: 48)

      assert [%{embedded: %{hours_only: read_back}}] = Ash.read!(Holder)
      assert read_back == holder.embedded.hours_only
    end

    test "an embedded duration is rejected when the permitted units cannot express it" do
      assert {:error, _} =
               Holder
               |> Ash.Changeset.for_create(:create, %{
                 embedded: %{hours_only: Duration.new!(minute: 90)}
               })
               |> Ash.create()
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

  describe "normalizing stored durations into the permitted units" do
    # A store keeps only what it can: Postgres returns `week: 1` as `day: 7`.
    test "re-expresses a stored value in the permitted unit" do
      assert {:ok, Duration.new!(week: 1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: 7), units: [:week])

      assert {:ok, Duration.new!(hour: 3)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(second: 10_800), units: [:hour])
    end

    test "what is read back can be written again" do
      constraints = [units: [:hour]]

      assert {:ok, loaded} =
               Ash.Type.Duration.cast_stored(Duration.new!(second: 10_800), constraints)

      # the invariant the defect broke
      assert {:ok, ^loaded} = Ash.Type.Duration.apply_constraints(loaded, constraints)
    end

    test "fills the largest permitted unit first" do
      assert {:ok, Duration.new!(day: 1, hour: 1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(second: 90_000),
                 units: [:day, :hour]
               )
    end

    test "a stored value the permitted units cannot express is refused, not rewritten" do
      # 10801 seconds is not a whole number of hours
      assert {:error, _} =
               Ash.Type.Duration.cast_stored(Duration.new!(second: 10_801), units: [:hour])

      # under [:day] the 5h 3min has nowhere to go
      assert {:error, _} =
               Ash.Type.Duration.cast_stored(Duration.new!(day: 1, hour: 5, minute: 3),
                 units: [:day]
               )
    end

    test "a read refuses a bucket the permitted units cannot speak for at all" do
      # [:week] can say nothing about months
      assert {:error, _} =
               Ash.Type.Duration.cast_stored(Duration.new!(month: 18), units: [:week])
    end

    test "a read applies the constraint exactly as a write does" do
      for {units, stored} <- [
            {[:hour], Duration.new!(second: 10_801)},
            {[:week], Duration.new!(month: 18)},
            {[:week], Duration.new!(second: 129_600)},
            {[:year], Duration.new!(month: 6)},
            {[:day], Duration.new!(day: 1, hour: 5, minute: 3)}
          ] do
        write = Ash.Type.Duration.apply_constraints(stored, units: units)
        read = Ash.Type.Duration.cast_stored(stored, units: units)

        assert match?({:error, _}, write) and match?({:error, _}, read),
               "#{inspect(stored)} under #{inspect(units)}: " <>
                 "write #{inspect(write)}, read #{inspect(read)}"
      end
    end

    test "a read never loses anything when every unit is permitted" do
      # microsecond is the floor, so there is never a remainder
      odd = Duration.new!(day: 1, hour: 5, minute: 3, microsecond: {7, 6})

      assert {:ok, %Duration{week: 0, day: 1, hour: 5, minute: 3, microsecond: {7, 6}}} =
               Ash.Type.Duration.cast_stored(odd, [])
    end

    test "normalizes the year/month and day/time buckets independently" do
      # months and microseconds are not interconvertible
      assert {:ok, Duration.new!(year: 1, week: 1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(month: 12, day: 7),
                 units: [:year, :week]
               )
    end

    test "one inexpressible side refuses the whole value on read" do
      # the year/month side is exact, but the day/time side is not, so the read fails
      assert {:error, _} =
               Ash.Type.Duration.cast_stored(Duration.new!(month: 12, second: 10_801),
                 units: [:year, :hour]
               )
    end

    test "a write reports only the bucket it cannot express" do
      assert {:error, [[message: _, units: _, disallowed: disallowed]]} =
               Ash.Type.Duration.apply_constraints(Duration.new!(month: 12, second: 10_801),
                 units: [:year, :hour]
               )

      assert disallowed =~ "second"
      refute disallowed =~ "month"
    end

    test "with no units constraint, all units are permitted and still filled greedily" do
      assert {:ok, Duration.new!(week: 1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: 7), [])

      assert {:ok, Duration.new!(year: 1, month: 6)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(month: 18), [])
    end

    test "normalizes on the way in as well, so a write and a read agree" do
      # apply_constraints is the write side; cast_stored is the read side
      assert {:ok, normalized} =
               Ash.Type.Duration.apply_constraints(Duration.new!(hour: 36), units: :day_time)

      assert normalized == Duration.new!(day: 1, hour: 12)
      assert {:ok, ^normalized} = Ash.Type.Duration.cast_stored(normalized, units: :day_time)
    end

    test "accepts a unit outside the permitted set when it converts exactly" do
      # `day` cannot be said here, but 1 day is exactly 24 hours
      assert {:ok, Duration.new!(hour: 24)} ==
               Ash.Type.Duration.apply_constraints(Duration.new!(day: 1), units: [:week, :hour])
    end

    test "still rejects across the bucket divide, which no conversion can cross" do
      assert {:error, _} =
               Ash.Type.Duration.apply_constraints(Duration.new!(month: 1), units: :day_time)

      assert {:error, _} =
               Ash.Type.Duration.apply_constraints(Duration.new!(day: 1), units: :year_month)
    end

    test "keeps the microsecond precision it was stored with" do
      assert {:ok, %Duration{hour: 3, microsecond: {0, 6}}} =
               Ash.Type.Duration.cast_stored(
                 %Duration{second: 10_800, microsecond: {0, 6}},
                 units: [:hour]
               )
    end

    test "normalizes a negative duration without changing its sign" do
      assert {:ok, Duration.new!(week: -1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: -7), units: [:week])
    end

    test "normalizes a value stored as an ISO 8601 string" do
      assert {:ok, Duration.new!(week: 1)} ==
               Ash.Type.Duration.cast_stored("P7D", units: [:week])
    end

    test "nil is unaffected" do
      assert {:ok, nil} = Ash.Type.Duration.cast_stored(nil, units: [:week])
    end

    test "spills a unit it cannot say into the next one it can" do
      # :day is missing, so a stored day is carried by hours
      assert {:ok, Duration.new!(week: 1, hour: 29)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(week: 1, day: 1, hour: 5),
                 units: [:week, :hour]
               )

      assert {:ok, Duration.new!(week: 1, hour: 72)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: 10), units: [:week, :hour])
    end

    test "fills greedily even when every unit used is already permitted" do
      # the permitted set says what may be said, not what shape a value keeps
      assert {:ok, Duration.new!(week: 1, day: 3)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: 10), units: :day_time)

      assert {:ok, Duration.new!(year: 1, month: 6)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(month: 18), units: :year_month)
    end

    test "what a create returns is what a subsequent read returns" do
      written = Duration.new!(day: 3, hour: 12)

      assert {:ok, post} =
               Post
               |> Ash.Changeset.for_create(:create, %{
                 duration_b: @minute30,
                 duration_calendar_free: written
               })
               |> Ash.create()

      assert post.duration_calendar_free == written

      assert {:ok, reread} = Ash.get(Post, post.id)
      assert reread.duration_calendar_free == post.duration_calendar_free
    end

    test "what an update returns is what a subsequent read returns" do
      assert {:ok, post} =
               Post
               |> Ash.Changeset.for_create(:create, %{duration_b: @minute30})
               |> Ash.create()

      assert {:ok, post} =
               post
               |> Ash.Changeset.for_update(:update, %{
                 duration_calendar_free: Duration.new!(hour: 36)
               })
               |> Ash.update()

      # normalized on the way in
      assert post.duration_calendar_free == Duration.new!(day: 1, hour: 12)

      assert {:ok, reread} = Ash.get(Post, post.id)
      assert reread.duration_calendar_free == post.duration_calendar_free
    end
  end

  describe "normalization preserves what a duration means" do
    # Elixir's own shift is the oracle here rather than compare/2: if normalizing
    # changed a magnitude, the two would land on different instants.
    test "a normalized duration shifts a datetime to the same instant" do
      anchors = [~U[2024-01-01 00:00:00Z], ~U[2024-02-29 00:00:00Z], ~U[2025-03-01 00:00:00Z]]

      inputs = [
        Duration.new!(hour: 36),
        Duration.new!(minute: 90),
        Duration.new!(day: 365),
        Duration.new!(month: 18),
        Duration.new!(year: 1),
        Duration.new!(year: 1, day: 5),
        Duration.new!(week: 1, day: 1, hour: 5),
        Duration.new!(second: 129_600),
        Duration.new!(day: -10),
        Duration.new!(month: -18),
        Duration.new!(day: 1, hour: -5),
        Duration.new!(year: 1, day: -5)
      ]

      for input <- inputs, anchor <- anchors do
        assert {:ok, normalized} = Ash.Type.Duration.cast_stored(input, [])

        assert DateTime.shift(anchor, input) == DateTime.shift(anchor, normalized),
               "#{inspect(input)} normalized to #{inspect(normalized)}, " <>
                 "which shifts #{anchor} differently"
      end
    end

    test "a year still lands a year later, across a leap day" do
      assert {:ok, normalized} = Ash.Type.Duration.cast_stored(Duration.new!(year: 1), [])

      assert Date.shift(~D[2024-02-29], normalized) ==
               Date.shift(~D[2024-02-29], Duration.new!(year: 1))

      assert Date.shift(~D[2024-02-29], normalized) == ~D[2025-02-28]
    end

    test "365 days is not a year, before or after normalizing" do
      assert {:ok, normalized} = Ash.Type.Duration.cast_stored(Duration.new!(day: 365), [])

      # 2024 is a leap year, so 365 days from its start is a day short of a year
      assert Date.shift(~D[2024-01-01], normalized) == ~D[2024-12-31]

      refute Date.shift(~D[2024-01-01], normalized) ==
               Date.shift(~D[2024-01-01], Duration.new!(year: 1))
    end
  end

  describe "negative durations" do
    test "a negative duration normalizes with a consistent sign" do
      assert {:ok, Duration.new!(week: -1, day: -3)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: -10), [])

      assert {:ok, Duration.new!(year: -1, month: -6)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(month: -18), [])
    end

    test "mixed signs within a side collapse to one representation" do
      # a day less five hours is nineteen hours
      assert {:ok, Duration.new!(hour: 19)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: 1, hour: -5), [])

      assert {:ok, Duration.new!(week: -1, day: -4)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(week: -2, day: 3), [])
    end

    test "the net sign wins, even against the largest unit" do
      # a week less ten days is three days short, not a week and a bit
      assert {:ok, Duration.new!(day: -3)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(week: 1, day: -10), [])

      assert {:ok, Duration.new!(hour: 6)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: -1, hour: 30), [])

      assert {:ok, Duration.new!(month: -6)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(year: 1, month: -18), [])
    end

    test "the two sides keep their own signs, independently" do
      assert {:ok, Duration.new!(year: 1, day: -5)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(year: 1, day: -5), [])

      # one side positive, the other negative, each normalized on its own
      assert {:ok, Duration.new!(year: 1, week: -57, day: -1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(year: 1, day: -400), [])
    end

    test "a duration that nets to zero normalizes to zero" do
      assert {:ok, zero} = Ash.Type.Duration.cast_stored(Duration.new!(day: 1, hour: -24), [])
      assert Ash.Type.Duration.compare(zero, Duration.new!([])) == :eq
    end

    test "a negative microsecond borrows from seconds" do
      assert {:ok, %Duration{second: -1, microsecond: {-500_000, 6}}} =
               Ash.Type.Duration.cast_stored(Duration.new!(microsecond: {-1_500_000, 6}), [])
    end
  end

  describe "the year/month to week/day divide" do
    test "a year is never expressed in days, whatever units are permitted" do
      assert {:ok, Duration.new!(year: 1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(year: 1), [])

      assert {:ok, Duration.new!(year: 1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(year: 1), units: :year_month)
    end

    test "days are never expressed in years, whatever units are permitted" do
      # 365 days is 52 weeks and a day, not a year — the divide holds even though
      # :year is permitted here
      assert {:ok, Duration.new!(week: 52, day: 1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(day: 365), [])
    end

    test "the two sides normalize independently, keeping both" do
      assert {:ok, Duration.new!(year: 1, day: 5)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(year: 1, day: 5), [])
    end

    test "a read across the divide is refused, like a write" do
      # [:day] can say nothing about years, so a stored year has nowhere to go
      assert {:error, _} =
               Ash.Type.Duration.cast_stored(Duration.new!(year: 1), units: [:day])
    end

    test "a write across the divide is refused rather than lost" do
      assert {:error, _} =
               Ash.Type.Duration.apply_constraints(Duration.new!(year: 1), units: [:day])
    end

    test "within a side, a remainder is refused on both paths" do
      # 365 days is 52 weeks and a day; [:week] cannot say the odd day
      assert {:error, _} =
               Ash.Type.Duration.cast_stored(Duration.new!(day: 365), units: [:week])

      assert {:error, _} =
               Ash.Type.Duration.apply_constraints(Duration.new!(day: 365), units: [:week])
    end

    test "comparison still crosses the divide, by the documented convention" do
      # compare/2 is unchanged: it converts a month to 30 days so durations stay
      # totally ordered. Normalization makes no such conversion.
      assert Ash.Type.Duration.compare(Duration.new!(year: 1), Duration.new!(day: 360)) == :eq

      assert {:ok, Duration.new!(year: 1)} ==
               Ash.Type.Duration.cast_stored(Duration.new!(year: 1), [])
    end
  end

  describe "atomic updates" do
    defmodule AtomicSpan do
      @moduledoc false
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      ets do
        private?(true)
      end

      attributes do
        uuid_primary_key :id
        attribute :free, :duration, public?: true
        attribute :hours_only, :duration, public?: true, constraints: [units: [:hour]]
      end

      actions do
        default_accept :*
        defaults [:read, :create]

        update :atomically do
          accept [:free]
          require_atomic? true
        end

        update :non_atomically do
          accept [:hours_only]
          require_atomic? false
        end
      end
    end

    test "an atomic update returns the canonical form, as a read does" do
      # an atomic update skips apply_constraints/2, but the record returns via cast_stored/2
      assert {:ok, span} = Ash.create(AtomicSpan, %{free: Duration.new!(hour: 1)})

      assert {:ok, updated} =
               span
               |> Ash.Changeset.for_update(:atomically, %{free: Duration.new!(hour: 36)})
               |> Ash.update()

      assert updated.free == Duration.new!(day: 1, hour: 12)

      assert {:ok, reread} = Ash.get(AtomicSpan, span.id)
      assert reread.free == updated.free
    end

    test "a non-atomic write normalizes into the permitted units" do
      assert {:ok, span} = Ash.create(AtomicSpan, %{hours_only: Duration.new!(hour: 3)})

      assert {:ok, updated} =
               span
               |> Ash.Changeset.for_update(:non_atomically, %{hours_only: Duration.new!(day: 2)})
               |> Ash.update()

      assert updated.hours_only == Duration.new!(hour: 48)
    end
  end

  describe "operand types" do
    # Without these declarations a duration is typed as whatever it is added to.
    @temporal_fields [
      {:datetime, Ash.Type.DateTime},
      {:utc_datetime, Ash.Type.UtcDatetime},
      {:utc_datetime_usec, Ash.Type.UtcDatetimeUsec},
      {:naive_datetime, Ash.Type.NaiveDatetime},
      {:date, Ash.Type.Date},
      {:time, Ash.Type.Time},
      {:time_usec, Ash.Type.TimeUsec}
    ]

    defp operand_types(expression) do
      {:ok, %op{} = hydrated} = Ash.Filter.hydrate_refs(expression, %{resource: Post})

      {[{left, _}, {right, _}], {returns, _}} =
        Ash.Expr.determine_types(op, [hydrated.left, hydrated.right])

      {left, right, returns}
    end

    test "adding a duration keeps it a duration, and returns the temporal type" do
      for {field, type} <- @temporal_fields do
        assert {^type, Ash.Type.Duration, ^type} =
                 operand_types(expr(^ref(field) + ^Duration.new!(day: 1)))
      end
    end

    test "subtracting a duration does the same" do
      for {field, type} <- @temporal_fields do
        assert {^type, Ash.Type.Duration, ^type} =
                 operand_types(expr(^ref(field) - ^Duration.new!(day: 1)))
      end
    end

    test "a duration on the left of an addition is kept too" do
      assert {Ash.Type.Duration, Ash.Type.UtcDatetime, Ash.Type.UtcDatetime} =
               operand_types(expr(^Duration.new!(day: 1) + ^ref(:utc_datetime)))
    end

    test "two durations add to a duration" do
      assert {Ash.Type.Duration, Ash.Type.Duration, Ash.Type.Duration} =
               operand_types(expr(^ref(:duration_a) + ^ref(:duration_b)))
    end
  end

  describe "comparison" do
    alias Ash.Query.Operator.{Eq, GreaterThan, LessThan}

    test "orders day/time units across representations at microsecond precision" do
      # 25 hours is more than a day
      assert Ash.Type.Duration.compare(Duration.new!(hour: 25), Duration.new!(day: 1)) == :gt
      # 90 minutes is more than an hour
      assert Ash.Type.Duration.compare(Duration.new!(minute: 90), Duration.new!(hour: 1)) == :gt
      # equal magnitudes expressed differently are equal
      assert Ash.Type.Duration.compare(Duration.new!(minute: 60), Duration.new!(hour: 1)) == :eq
      assert Ash.Type.Duration.compare(Duration.new!(week: 1), Duration.new!(day: 7)) == :eq
    end

    test "orders year/month units by total months (year = 12 months)" do
      # 2 years is more than 1 year
      assert Ash.Type.Duration.compare(Duration.new!(year: 2), Duration.new!(year: 1)) == :gt
      # P1Y6M and P18M are the same duration
      assert Ash.Type.Duration.compare(Duration.new!(year: 1, month: 6), Duration.new!(month: 18)) ==
               :eq

      # 12 months is less than 18 months
      assert Ash.Type.Duration.compare(Duration.new!(year: 1), Duration.new!(month: 18)) == :lt
    end

    test "does not truncate sub-millisecond precision the way to_timeout/1 would" do
      a = Duration.new!(second: 1, microsecond: {0, 6})
      b = Duration.new!(second: 1, microsecond: {500, 6})
      assert to_timeout(a) == to_timeout(b)
      assert Ash.Type.Duration.compare(a, b) == :lt
    end

    test "handles negative durations" do
      assert Ash.Type.Duration.compare(Duration.new!(hour: -1), Duration.new!(hour: 1)) == :lt
      assert Ash.Type.Duration.compare(Duration.new!(day: -1), Duration.new!(hour: -25)) == :gt
      assert Ash.Type.Duration.compare(Duration.new!(month: -1), Duration.new!(year: 1)) == :lt
    end

    test "a wholly-zero duration compares as less than any positive duration" do
      zero = Duration.new!([])
      assert Ash.Type.Duration.compare(zero, Duration.new!(day: 1)) == :lt
      assert Ash.Type.Duration.compare(zero, Duration.new!(month: 1)) == :lt
      assert Ash.Type.Duration.compare(zero, zero) == :eq
    end

    test "flows through comparison operators correctly" do
      assert GreaterThan.evaluate(%{left: Duration.new!(hour: 25), right: Duration.new!(day: 1)}) ==
               {:known, true}

      assert LessThan.evaluate(%{left: Duration.new!(hour: 25), right: Duration.new!(day: 1)}) ==
               {:known, false}

      assert Eq.evaluate(%{left: Duration.new!(minute: 60), right: Duration.new!(hour: 1)}) ==
               {:known, true}

      assert Eq.evaluate(%{
               left: Duration.new!(year: 1, month: 6),
               right: Duration.new!(month: 18)
             }) ==
               {:known, true}
    end

    test "compares across the year/month vs day/time boundary using the Postgres convention (month = 30 days)" do
      # month = 30 days
      assert Ash.Type.Duration.compare(Duration.new!(month: 1), Duration.new!(day: 30)) == :eq
      assert Ash.Type.Duration.compare(Duration.new!(month: 1), Duration.new!(day: 31)) == :lt
      # year = 360 days (12 * 30)
      assert Ash.Type.Duration.compare(Duration.new!(year: 1), Duration.new!(day: 360)) == :eq
      assert Ash.Type.Duration.compare(Duration.new!(year: 1), Duration.new!(day: 365)) == :lt
    end

    test "compares a duration that mixes both unit groups (no raise)" do
      # P1M15D = 30 + 15 = 45 days; P2M = 60 days
      assert Ash.Type.Duration.compare(Duration.new!(month: 1, day: 15), Duration.new!(month: 2)) ==
               :lt

      # and the reverse holds — no argument-order dependence (this case used to raise)
      assert Ash.Type.Duration.compare(Duration.new!(month: 2), Duration.new!(month: 1, day: 15)) ==
               :gt
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
