# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.TemporalAggregateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr

  require Ash.Query
  require Ash.Sort

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
      attribute :name, :string, public?: true
      attribute :starts_at, :utc_datetime_usec, public?: true
    end

    calculations do
      calculate :seen_as_of, :utc_datetime_usec, SeenAsOf
    end
  end

  # Both events start within a day of `@as_of`, and months before these tests can run.
  @as_of ~U[2026-06-15 20:00:00.000000Z]
  @after_as_of ~U[2026-07-01 00:00:00.000000Z]

  setup do
    Ash.create!(Event, %{name: "morning", starts_at: ~U[2026-06-15 08:00:00.000000Z]})
    Ash.create!(Event, %{name: "evening", starts_at: ~U[2026-06-15 18:00:00.000000Z]})

    :ok
  end

  defp started_in_the_last_day, do: Ash.Query.filter(Event, starts_at >= ago(1, :day))

  describe "the filter of an aggregated query" do
    test "count anchors ago() to as_of" do
      assert 2 == started_in_the_last_day() |> Ash.Query.as_of(@as_of) |> Ash.count!()
    end

    test "count takes as_of as an option" do
      assert 2 == Ash.count!(started_in_the_last_day(), as_of: @as_of)
    end

    test "count anchors now() to as_of" do
      query = Ash.Query.filter(Event, now() < ^@after_as_of)

      assert 2 == query |> Ash.Query.as_of(@as_of) |> Ash.count!()
    end

    test "count anchors from_now() to as_of" do
      query = Ash.Query.filter(Event, from_now(1, :day) < ^@after_as_of)

      assert 2 == query |> Ash.Query.as_of(@as_of) |> Ash.count!()
    end

    test "exists? anchors ago() to as_of" do
      assert started_in_the_last_day() |> Ash.Query.as_of(@as_of) |> Ash.exists?()
    end

    test "max anchors ago() to as_of" do
      assert ~U[2026-06-15 18:00:00.000000Z] ==
               started_in_the_last_day() |> Ash.Query.as_of(@as_of) |> Ash.max!(:starts_at)
    end

    test "without as_of, ago() counts back from the wall clock" do
      assert 0 == Ash.count!(started_in_the_last_day())
      refute Ash.exists?(started_in_the_last_day())
    end
  end

  describe "the sort of an aggregated query" do
    # Sorted by "started in the last eight hours" first, then by name from z to a: only the
    # evening event started that recently as of `@as_of`, and neither has by the wall clock.
    defp recent_first do
      Ash.Query.sort(Event, [
        {Ash.Sort.expr_sort(starts_at >= ago(8, :hour), :boolean), :desc},
        {:name, :desc}
      ])
    end

    test "first anchors ago() to as_of" do
      assert "evening" == recent_first() |> Ash.Query.as_of(@as_of) |> Ash.first!(:name)
    end

    test "without as_of, ago() counts back from the wall clock" do
      assert "morning" == Ash.first!(recent_first(), :name)
    end
  end

  describe "the filter of the aggregate itself" do
    test "anchors ago() to as_of" do
      aggregate = {:recent, :count, query: [filter: expr(starts_at >= ago(1, :day))]}

      assert %{recent: 2} == Event |> Ash.Query.as_of(@as_of) |> Ash.aggregate!(aggregate)
    end
  end

  describe "a calculation aggregated as a field" do
    test "is loaded at the query's as_of" do
      assert @as_of == Event |> Ash.Query.as_of(@as_of) |> Ash.max!(:seen_as_of)
    end

    test "without as_of, is loaded with none" do
      assert nil == Ash.max!(Event, :seen_as_of)
    end
  end
end
