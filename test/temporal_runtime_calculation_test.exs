# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.TemporalRuntimeCalculationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule TimeZone do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, _opts, _context), do: [:tz]

    @impl true
    def calculate(records, _opts, _context), do: Enum.map(records, & &1.tz)
  end

  defmodule Days do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, _opts, _context), do: [:days]

    @impl true
    def calculate(records, _opts, _context), do: Enum.map(records, & &1.days)
  end

  # Each expression calculation references a module calculation, so it cannot run in the
  # data layer and is evaluated in Elixir by `Ash.Resource.Calculation.Expression`.
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
      attribute :tz, :string, public?: true
      attribute :days, :integer, public?: true
    end

    calculations do
      calculate :tz_mod, :string, TimeZone
      calculate :days_mod, :integer, Days

      calculate :now_at_runtime,
                :utc_datetime_usec,
                expr(if is_nil(tz_mod), do: now(), else: now())

      calculate :local_now_at_runtime, :naive_datetime, expr(at_zone(now(), tz_mod))

      calculate :day_ago_at_runtime,
                :utc_datetime_usec,
                expr(if is_nil(tz_mod), do: now(), else: ago(1, :day))

      calculate :days_ago_at_runtime, :utc_datetime_usec, expr(ago(days_mod, :day))
    end
  end

  # 02:00 UTC on 16 June is 22:00 on 15 June in New York (UTC-4 in June).
  @as_of ~U[2026-06-16 02:00:00.123456Z]

  setup do
    Ash.create!(Event, %{tz: "America/New_York", days: 2})
    :ok
  end

  defp load_as_of(calculation) do
    [event] = Event |> Ash.Query.load(calculation) |> Ash.Query.as_of(@as_of) |> Ash.read!()
    Map.fetch!(event, calculation)
  end

  describe "an expression calculation evaluated in Elixir under an as_of" do
    test "anchors now() to as_of" do
      assert load_as_of(:now_at_runtime) == @as_of
    end

    test "anchors now() inside a custom expression to as_of" do
      # `:naive_datetime` keeps whole seconds, so the microseconds of `as_of` are cast away.
      assert load_as_of(:local_now_at_runtime) == ~N[2026-06-15 22:00:00]
    end

    test "anchors ago with a literal amount to as_of" do
      assert load_as_of(:day_ago_at_runtime) == ~U[2026-06-15 02:00:00.123456Z]
    end

    test "anchors ago with an amount from a calculation to as_of" do
      # Evaluated in Elixir, `datetime_add` cuts its result to whole seconds.
      assert DateTime.compare(load_as_of(:days_ago_at_runtime), ~U[2026-06-14 02:00:00Z]) ==
               :eq
    end
  end

  test "an expression calculation evaluated in Elixir without an as_of uses the wall clock" do
    [event] = Event |> Ash.Query.load(:now_at_runtime) |> Ash.read!()

    assert abs(DateTime.diff(event.now_at_runtime, DateTime.utc_now())) < 5
  end
end
