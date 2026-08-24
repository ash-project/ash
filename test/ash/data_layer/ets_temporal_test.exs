# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.DataLayer.EtsTemporalTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Temporal.EtsVersioned

  require Ash.Query

  setup do
    on_exit(fn -> Ash.DataLayer.Ets.stop(EtsVersioned) end)
  end

  @early %Ash.Range{
    lower: ~U[2020-01-01 00:00:00Z],
    upper: ~U[2021-01-01 00:00:00Z],
    bounds: :"[)"
  }
  @open %Ash.Range{lower: ~U[2021-01-01 00:00:00Z], upper: nil, bounds: :"[)"}

  describe "a read is a point in time" do
    setup do
      # Distinct ids: the table is keyed by the primary key alone until the period
      # joins it, so two versions of one record would collide.
      Ash.Seed.seed!(%EtsVersioned{id: 1, name: "early", valid_at: @early})
      Ash.Seed.seed!(%EtsVersioned{id: 2, name: "open", valid_at: @open})
      :ok
    end

    test "an instant selects the records whose period holds it" do
      assert [%{name: "early"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2020-06-01 00:00:00Z]) |> Ash.read!()

      assert [%{name: "open"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2026-06-01 00:00:00Z]) |> Ash.read!()
    end

    test "a shared boundary belongs to the later period" do
      assert [%{name: "open"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2021-01-01 00:00:00Z]) |> Ash.read!()
    end

    test "an instant before every period returns nothing" do
      assert [] = EtsVersioned |> Ash.Query.as_of(~U[2019-01-01 00:00:00Z]) |> Ash.read!()
    end

    test "a read with no as_of is anchored to now, so it sees current state" do
      assert [%{name: "open"}] = EtsVersioned |> Ash.read!()
    end

    test "narrowing happens before the filter, not instead of it" do
      assert [] =
               EtsVersioned
               |> Ash.Query.filter(name == "early")
               |> Ash.Query.as_of(~U[2026-06-01 00:00:00Z])
               |> Ash.read!()
    end
  end

  test "a record with no period is not returned for any instant" do
    Ash.Seed.seed!(%EtsVersioned{id: 3, name: "undated"})

    assert [] = EtsVersioned |> Ash.Query.as_of(~U[2020-06-01 00:00:00Z]) |> Ash.read!()
    assert [] = Ash.read!(EtsVersioned)
  end

  test "a non-temporal resource is untouched by an as_of" do
    name = "unversioned-#{System.unique_integer([:positive])}"
    Ash.create!(Ash.Test.Temporal.Thing, %{name: name})

    assert [%{name: ^name}] =
             Ash.Test.Temporal.Thing
             |> Ash.Query.filter(name == ^name)
             |> Ash.Query.as_of(~U[2020-06-01 00:00:00Z])
             |> Ash.read!()
  end
end
