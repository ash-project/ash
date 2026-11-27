# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.TemporalRelativeTimeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr

  require Ash.Query

  defmodule TimeZone do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, _opts, _context), do: [:time_zone]

    @impl true
    def calculate(records, _opts, _context), do: Enum.map(records, & &1.time_zone)
  end

  defmodule Days do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, _opts, _context), do: [:days]

    @impl true
    def calculate(records, _opts, _context), do: Enum.map(records, & &1.days)
  end

  defmodule SeenAsOf do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def calculate(records, _opts, context) do
      Enum.map(records, fn _record -> context.source_context[:shared][:as_of] end)
    end
  end

  defmodule Event do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :time_zone, :string, public?: true
      attribute :days, :integer, public?: true
      attribute :dur, :duration, public?: true
    end

    calculations do
      calculate :current_time, :utc_datetime_usec, expr(now())
      calculate :current_date, :date, expr(today())
      calculate :day_ago, :utc_datetime_usec, expr(ago(1, :day))

      calculate :now_for_row,
                :utc_datetime_usec,
                expr(if is_nil(time_zone), do: now(), else: now())

      calculate :local_now, :naive_datetime, expr(at_zone(now(), time_zone))
      calculate :local_today, :date, expr(type(at_zone(now(), time_zone), :date))

      calculate :days_ago, :utc_datetime_usec, expr(ago(days, :day))
      calculate :dur_ago, :utc_datetime_usec, expr(ago(dur))
      calculate :dur_from_now, :utc_datetime_usec, expr(from_now(dur))

      calculate :seen_as_of, :utc_datetime_usec, SeenAsOf

      # Each expression calculation below references a module calculation, so it cannot
      # run in the data layer and is evaluated in Elixir by
      # `Ash.Resource.Calculation.Expression`.
      calculate :time_zone_mod, :string, TimeZone
      calculate :days_mod, :integer, Days

      calculate :now_at_runtime,
                :utc_datetime_usec,
                expr(if is_nil(time_zone_mod), do: now(), else: now())

      calculate :local_now_at_runtime, :naive_datetime, expr(at_zone(now(), time_zone_mod))

      calculate :day_ago_at_runtime,
                :utc_datetime_usec,
                expr(if is_nil(time_zone_mod), do: now(), else: ago(1, :day))

      calculate :days_ago_at_runtime, :utc_datetime_usec, expr(ago(days_mod, :day))
    end
  end

  # 02:00 UTC on 16 June is 22:00 on 15 June in New York (UTC-4 in June).
  @as_of ~U[2026-06-16 02:00:00.000000Z]
  @as_of_usec ~U[2026-06-16 02:00:00.123456Z]
  @day_before ~U[2026-06-15 02:00:00.000000Z]
  @local_as_of ~N[2026-06-15 22:00:00]

  defp create_event(_context) do
    %{
      event:
        Ash.create!(Event, %{
          time_zone: "America/New_York",
          days: 1,
          dur: Duration.new!(hour: 2, minute: 30)
        })
    }
  end

  defp as_of(query), do: Ash.Query.as_of(query, @as_of)

  describe "now() inside a custom expression" do
    setup :create_event

    test "is anchored to as_of when the custom expression is used in a filter" do
      assert [_] =
               Event
               |> Ash.Query.filter(at_zone(now(), time_zone) == ^@local_as_of)
               |> as_of()
               |> Ash.read!()
    end

    test "is anchored to as_of when a calculation using it is referenced from a filter" do
      assert [_] =
               Event
               |> Ash.Query.filter(local_now == ^@local_as_of)
               |> as_of()
               |> Ash.read!()
    end
  end

  describe "loaded calculations" do
    setup :create_event

    test "that reference the row are anchored to as_of" do
      assert [%{now_for_row: @as_of}] =
               Event |> Ash.Query.load(:now_for_row) |> as_of() |> Ash.read!()
    end

    test "that use a custom expression are anchored to as_of" do
      assert [%{local_now: @local_as_of, local_today: ~D[2026-06-15]}] =
               Event |> Ash.Query.load([:local_now, :local_today]) |> as_of() |> Ash.read!()
    end
  end

  describe "ago and from_now" do
    setup :create_event

    test "with an amount from the row are anchored to as_of in a filter" do
      assert [_] =
               Event
               |> Ash.Query.filter(
                 ago(days, :day) == ^~U[2026-06-15 02:00:00Z] and
                   from_now(days, :day) == ^~U[2026-06-17 02:00:00Z]
               )
               |> as_of()
               |> Ash.read!()
    end

    test "with an amount from the row are anchored to as_of in a loaded calculation" do
      assert [%{days_ago: days_ago}] =
               Event |> Ash.Query.load(:days_ago) |> as_of() |> Ash.read!()

      assert DateTime.compare(days_ago, ~U[2026-06-15 02:00:00Z]) == :eq
    end

    test "from_now with a Duration is anchored to as_of" do
      duration = Duration.new!(day: 1)

      assert [_] =
               Event
               |> Ash.Query.filter(
                 from_now(^duration) == ^~U[2026-06-17 02:00:00Z] and not is_nil(time_zone)
               )
               |> as_of()
               |> Ash.read!()
    end

    test "ago with a Duration is anchored to as_of" do
      duration = Duration.new!(day: 1)

      assert [_] =
               Event
               |> Ash.Query.filter(
                 ago(^duration) == ^~U[2026-06-15 02:00:00Z] and not is_nil(time_zone)
               )
               |> as_of()
               |> Ash.read!()
    end

    test "with a duration from the row are anchored to as_of in a loaded calculation" do
      assert [%{dur_ago: dur_ago, dur_from_now: dur_from_now}] =
               Event |> Ash.Query.load([:dur_ago, :dur_from_now]) |> as_of() |> Ash.read!()

      # Evaluated in Elixir, `datetime_add` cuts its result to whole seconds.
      assert DateTime.compare(dur_ago, ~U[2026-06-15 23:30:00Z]) == :eq
      assert DateTime.compare(dur_from_now, ~U[2026-06-16 04:30:00Z]) == :eq
    end

    test "with a duration from the row are anchored to as_of in a filter" do
      assert [_] =
               Event
               |> Ash.Query.filter(
                 ago(dur) == ^~U[2026-06-15 23:30:00Z] and
                   from_now(dur) == ^~U[2026-06-16 04:30:00Z]
               )
               |> as_of()
               |> Ash.read!()
    end
  end

  describe "an expression calculation evaluated in Elixir" do
    setup :create_event

    defp load_as_of(calculation) do
      [event] =
        Event |> Ash.Query.load(calculation) |> Ash.Query.as_of(@as_of_usec) |> Ash.read!()

      Map.fetch!(event, calculation)
    end

    test "anchors now() to as_of" do
      assert load_as_of(:now_at_runtime) == @as_of_usec
    end

    test "anchors now() inside a custom expression to as_of" do
      # `:naive_datetime` keeps whole seconds, so the microseconds of `as_of` are cast away.
      assert load_as_of(:local_now_at_runtime) == @local_as_of
    end

    test "anchors ago with a literal amount to as_of" do
      assert load_as_of(:day_ago_at_runtime) == ~U[2026-06-15 02:00:00.123456Z]
    end

    test "anchors ago with an amount from a calculation to as_of" do
      # Evaluated in Elixir, `datetime_add` cuts its result to whole seconds.
      assert DateTime.compare(load_as_of(:days_ago_at_runtime), ~U[2026-06-15 02:00:00Z]) ==
               :eq
    end

    test "uses the wall clock without an as_of" do
      [event] = Event |> Ash.Query.load(:now_at_runtime) |> Ash.read!()

      assert abs(DateTime.diff(event.now_at_runtime, DateTime.utc_now())) < 5
    end
  end

  describe "Ash.calculate/3 given no record" do
    test "anchors now() to as_of" do
      assert @as_of == Ash.calculate!(Event, :current_time, as_of: @as_of)
    end

    test "anchors today() to as_of" do
      assert ~D[2026-06-16] == Ash.calculate!(Event, :current_date, as_of: @as_of)
    end

    test "anchors ago() to as_of" do
      assert @day_before == Ash.calculate!(Event, :day_ago, as_of: @as_of)
    end

    test "anchors ago() with an amount given in refs to as_of" do
      assert @day_before == Ash.calculate!(Event, :days_ago, refs: %{days: 1}, as_of: @as_of)
    end

    test "anchors now() inside a custom expression to as_of" do
      assert @local_as_of ==
               Ash.calculate!(Event, :local_now,
                 refs: %{time_zone: "America/New_York"},
                 as_of: @as_of
               )
    end

    test "gives a module calculation the as_of in its context" do
      assert @as_of == Ash.calculate!(Event, :seen_as_of, as_of: @as_of)
    end

    test "anchors now() when the data layer runs the calculation" do
      assert @as_of == Ash.calculate!(Event, :current_time, as_of: @as_of, data_layer?: true)
    end

    test "anchors ago() when the data layer runs the calculation" do
      assert @day_before == Ash.calculate!(Event, :day_ago, as_of: @as_of, data_layer?: true)
    end

    test "resolves an as_of of :now to the wall clock" do
      before = DateTime.utc_now()
      result = Ash.calculate!(Event, :current_time, as_of: :now)

      assert DateTime.compare(result, before) != :lt
      assert DateTime.compare(result, DateTime.utc_now()) != :gt
    end

    test "without as_of, takes the wall clock" do
      before = DateTime.utc_now()

      assert DateTime.compare(Ash.calculate!(Event, :current_time), before) != :lt
    end
  end

  describe "Ash.calculate/3 given a record" do
    setup :create_event

    test "anchors now() to as_of", %{event: event} do
      assert @as_of == Ash.calculate!(event, :current_time, as_of: @as_of)
    end

    test "anchors ago() with the record's amount to as_of", %{event: event} do
      assert @day_before == Ash.calculate!(event, :days_ago, as_of: @as_of)
    end

    test "anchors now() inside a custom expression to as_of", %{event: event} do
      assert @local_as_of == Ash.calculate!(event, :local_now, as_of: @as_of)
    end

    test "anchors without reusing the record's values", %{event: event} do
      assert @day_before ==
               Ash.calculate!(event, :days_ago, as_of: @as_of, reuse_values?: false)
    end

    test "anchors an expression that is evaluated in Elixir", %{event: event} do
      assert @day_before == Ash.calculate!(event, :days_ago_at_runtime, as_of: @as_of)
    end

    test "gives a module calculation the as_of in its context", %{event: event} do
      assert @as_of == Ash.calculate!(event, :seen_as_of, as_of: @as_of)
    end

    test "takes the as_of in the record's metadata when given none", %{event: event} do
      event = Ash.Resource.put_metadata(event, :as_of, @as_of)

      assert @as_of == Ash.calculate!(event, :current_time)
    end

    test "prefers the as_of option to the one in the record's metadata", %{event: event} do
      event = Ash.Resource.put_metadata(event, :as_of, @day_before)

      assert @as_of == Ash.calculate!(event, :current_time, as_of: @as_of)
    end

    test "without as_of, takes the wall clock", %{event: event} do
      before = DateTime.utc_now()

      assert DateTime.compare(Ash.calculate!(event, :current_time), before) != :lt
    end

    test "anchors ago() with the amount of a record that has no primary key" do
      assert @day_before == Ash.calculate!(%Event{id: nil, days: 1}, :days_ago, as_of: @as_of)
    end
  end

  describe "Ash.Temporal.anchor_relative_time/2" do
    defp hydrate!(expression) do
      {:ok, hydrated} = Ash.Filter.hydrate_refs(expression, %{resource: Event, public?: false})
      hydrated
    end

    test "leaves the expression alone without an as_of" do
      now = hydrate!(expr(now()))
      assert Ash.Temporal.anchor_relative_time(now, nil) == now
    end

    test "replaces now and today with the instant and its date" do
      assert Ash.Temporal.anchor_relative_time(hydrate!(expr(now())), @as_of) == @as_of

      assert Ash.Temporal.anchor_relative_time(hydrate!(expr(today())), @as_of) ==
               ~D[2026-06-16]
    end

    test "computes ago and from_now when the amount is known" do
      duration = Duration.new!(hour: 2)

      assert Ash.Temporal.anchor_relative_time(hydrate!(expr(ago(1, :day))), @as_of) ==
               @day_before

      assert Ash.Temporal.anchor_relative_time(hydrate!(expr(from_now(1, :day))), @as_of) ==
               ~U[2026-06-17 02:00:00.000000Z]

      assert Ash.Temporal.anchor_relative_time(hydrate!(expr(ago(^duration))), @as_of) ==
               ~U[2026-06-16 00:00:00.000000Z]

      assert Ash.Temporal.anchor_relative_time(hydrate!(expr(from_now(^duration))), @as_of) ==
               ~U[2026-06-16 04:00:00.000000Z]
    end

    test "turns ago and from_now into datetime_add when the amount is a reference" do
      assert %Ash.Query.Function.DateTimeAdd{
               arguments: [@as_of, %Ash.Query.Function.Minus{arguments: [%Ash.Query.Ref{}]}, :day]
             } = Ash.Temporal.anchor_relative_time(hydrate!(expr(ago(days, :day))), @as_of)

      assert %Ash.Query.Function.DateTimeAdd{arguments: [@as_of, %Ash.Query.Ref{}, :day]} =
               Ash.Temporal.anchor_relative_time(hydrate!(expr(from_now(days, :day))), @as_of)
    end

    test "turns ago and from_now into datetime_add when the duration is a reference" do
      assert %Ash.Query.Function.DateTimeAdd{
               arguments: [@as_of, %Ash.Query.Function.Minus{arguments: [%Ash.Query.Ref{}]}]
             } = Ash.Temporal.anchor_relative_time(hydrate!(expr(ago(dur))), @as_of)

      assert %Ash.Query.Function.DateTimeAdd{arguments: [@as_of, %Ash.Query.Ref{}]} =
               Ash.Temporal.anchor_relative_time(hydrate!(expr(from_now(dur))), @as_of)
    end

    test "leaves other expressions alone" do
      ref = hydrate!(expr(days))
      assert Ash.Temporal.anchor_relative_time(ref, @as_of) == ref
    end

    test "relative_time_function?/1 recognizes the relative time functions" do
      assert Ash.Temporal.relative_time_function?(hydrate!(expr(now())))
      assert Ash.Temporal.relative_time_function?(hydrate!(expr(today())))
      assert Ash.Temporal.relative_time_function?(hydrate!(expr(ago(days, :day))))
      assert Ash.Temporal.relative_time_function?(hydrate!(expr(from_now(days, :day))))
      refute Ash.Temporal.relative_time_function?(hydrate!(expr(days)))
      refute Ash.Temporal.relative_time_function?(@as_of)
    end
  end
end
