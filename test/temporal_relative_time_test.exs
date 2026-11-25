# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.TemporalRelativeTimeTest do
  @moduledoc false
  use ExUnit.Case, async: true

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
    end

    calculations do
      calculate :local_now, :naive_datetime, expr(at_zone(now(), time_zone))
      calculate :local_today, :date, expr(type(at_zone(now(), time_zone), :date))

      calculate :now_for_row,
                :utc_datetime_usec,
                expr(if is_nil(time_zone), do: now(), else: now())

      calculate :days_ago, :utc_datetime_usec, expr(ago(days, :day))
    end
  end

  # 02:00 UTC on 16 June is 22:00 on 15 June in New York (UTC-4 in June).
  @as_of ~U[2026-06-16 02:00:00.000000Z]
  @local_as_of ~N[2026-06-15 22:00:00]

  setup do
    Ash.create!(Event, %{time_zone: "America/New_York", days: 1})
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
end
