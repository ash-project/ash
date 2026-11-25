# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.TemporalRelativeTimeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr

  require Ash.Query

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
      calculate :local_now, :naive_datetime, expr(at_zone(now(), time_zone))
      calculate :local_today, :date, expr(type(at_zone(now(), time_zone), :date))

      calculate :now_for_row,
                :utc_datetime_usec,
                expr(if is_nil(time_zone), do: now(), else: now())

      calculate :days_ago, :utc_datetime_usec, expr(ago(days, :day))
      calculate :dur_ago, :utc_datetime_usec, expr(ago(dur))
      calculate :dur_from_now, :utc_datetime_usec, expr(from_now(dur))
    end
  end

  # 02:00 UTC on 16 June is 22:00 on 15 June in New York (UTC-4 in June).
  @as_of ~U[2026-06-16 02:00:00.000000Z]
  @local_as_of ~N[2026-06-15 22:00:00]

  setup do
    Ash.create!(Event, %{
      time_zone: "America/New_York",
      days: 1,
      dur: Duration.new!(hour: 2, minute: 30)
    })

    :ok
  end

  defp as_of(query), do: Ash.Query.as_of(query, @as_of)

  describe "now() inside a custom expression" do
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
               ~U[2026-06-15 02:00:00.000000Z]

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
