# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.TemporalCalculateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule SeenAsOf do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def calculate(records, _opts, context) do
      Enum.map(records, fn _record -> context.source_context[:shared][:as_of] end)
    end
  end

  defmodule Days do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, _opts, _context), do: [:days]

    @impl true
    def calculate(records, _opts, _context), do: Enum.map(records, & &1.days)
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
    end

    calculations do
      calculate :current_time, :utc_datetime_usec, expr(now())
      calculate :current_date, :date, expr(today())
      calculate :day_ago, :utc_datetime_usec, expr(ago(1, :day))
      calculate :days_ago, :utc_datetime_usec, expr(ago(days, :day))
      calculate :local_now, :naive_datetime, expr(at_zone(now(), time_zone))

      calculate :seen_as_of, :utc_datetime_usec, SeenAsOf

      # Refers to a module calculation, so it can only be evaluated in Elixir.
      calculate :days_mod, :integer, Days
      calculate :days_ago_at_runtime, :utc_datetime_usec, expr(ago(days_mod, :day))
    end
  end

  # 02:00 UTC on 16 June is 22:00 on 15 June in New York (UTC-4 in June).
  @as_of ~U[2026-06-16 02:00:00.000000Z]
  @day_before ~U[2026-06-15 02:00:00.000000Z]
  @local_as_of ~N[2026-06-15 22:00:00]

  describe "a calculation given no record" do
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

  describe "a calculation given a record" do
    setup do
      %{event: Ash.create!(Event, %{time_zone: "America/New_York", days: 1})}
    end

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
  end

  describe "a calculation given a record with no primary key" do
    test "anchors ago() with the record's amount to as_of" do
      assert @day_before == Ash.calculate!(%Event{id: nil, days: 1}, :days_ago, as_of: @as_of)
    end
  end
end
