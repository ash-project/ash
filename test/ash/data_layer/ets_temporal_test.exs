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

  describe "a create establishes the record's period" do
    test "from the write's instant, with no end" do
      record = Ash.create!(EtsVersioned, %{id: 1, name: "a"}, as_of: ~U[2020-06-01 00:00:00Z])

      assert record.valid_at == %Ash.Range{
               lower: ~U[2020-06-01 00:00:00Z],
               upper: nil,
               bounds: :"[)"
             }
    end

    test "on this layer's clock when the write does not say when" do
      before = DateTime.truncate(DateTime.utc_now(), :second)
      record = Ash.create!(EtsVersioned, %{id: 1, name: "a"})

      assert DateTime.compare(record.valid_at.lower, before) in [:eq, :gt]
      refute record.valid_at.upper
    end

    test "and the record is readable at that instant, but not before it" do
      Ash.create!(EtsVersioned, %{id: 1, name: "a"}, as_of: ~U[2020-06-01 00:00:00Z])

      assert [%{name: "a"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2020-06-01 00:00:00Z]) |> Ash.read!()

      assert [] = EtsVersioned |> Ash.Query.as_of(~U[2020-05-31 23:59:59Z]) |> Ash.read!()
    end

    test "leaving a period the caller wrote alone" do
      record = Ash.Seed.seed!(%EtsVersioned{id: 1, name: "a", valid_at: @early})

      assert record.valid_at == @early
    end

    test "on the bulk path too" do
      assert %Ash.BulkResult{records: [record]} =
               Ash.bulk_create!([%{id: 1, name: "bulked"}], EtsVersioned, :create,
                 return_records?: true
               )

      assert %Ash.Range{lower: %DateTime{}, upper: nil, bounds: :"[)"} = record.valid_at
      assert [%{name: "bulked"}] = Ash.read!(EtsVersioned)
    end
  end

  describe "the period is part of the storage key" do
    test "and only on a temporal resource" do
      record = %EtsVersioned{id: 1, name: "x", valid_at: @open}

      assert Ash.DataLayer.Ets.pkey_map(EtsVersioned, record) == %{id: 1, valid_at: @open}

      assert Ash.DataLayer.Ets.pkey_map(Ash.Test.Temporal.Thing, %{id: "abc", name: "y"}) ==
               %{id: "abc"}
    end

    test "so one record can have several versions" do
      Ash.Seed.seed!(%EtsVersioned{id: 1, name: "early", valid_at: @early})
      Ash.Seed.seed!(%EtsVersioned{id: 1, name: "open", valid_at: @open})

      assert [%{name: "early"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2020-06-01 00:00:00Z]) |> Ash.read!()

      assert [%{name: "open"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2026-06-01 00:00:00Z]) |> Ash.read!()
    end

    test "and a destroy removes the version it was given, not the record" do
      Ash.Seed.seed!(%EtsVersioned{id: 1, name: "early", valid_at: @early})
      open = Ash.Seed.seed!(%EtsVersioned{id: 1, name: "open", valid_at: @open})

      Ash.destroy!(open)

      assert [%{name: "early"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2020-06-01 00:00:00Z]) |> Ash.read!()

      assert [] = EtsVersioned |> Ash.Query.as_of(~U[2026-06-01 00:00:00Z]) |> Ash.read!()
    end

    test "and an upsert conflicts with the version holding its instant, not with every version" do
      Ash.Seed.seed!(%EtsVersioned{id: 1, name: "early", valid_at: @early})
      Ash.Seed.seed!(%EtsVersioned{id: 1, name: "open", valid_at: @open})

      assert {:ok, upserted} =
               Ash.create(EtsVersioned, %{id: 1, name: "later"},
                 action: :upsert,
                 as_of: ~U[2026-06-01 00:00:00Z]
               )

      assert upserted.name == "later"

      assert [%{name: "early"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2020-06-01 00:00:00Z]) |> Ash.read!()
    end
  end

  describe "versions of one record must not overlap" do
    defp create_at(id, name, as_of) do
      EtsVersioned
      |> Ash.Changeset.for_create(:create, %{id: id, name: name})
      |> Ash.Changeset.as_of(as_of)
      |> Ash.create()
    end

    # Both open a period with no end, so the later holds every instant the earlier does.
    test "a second open-ended version of one record is refused" do
      assert {:ok, _} = create_at(1, "first", ~U[2020-01-01 00:00:00Z])

      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Changes.InvalidAttribute{} = error]}} =
               create_at(1, "second", ~U[2021-01-01 00:00:00Z])

      assert error.field == :valid_at
      assert error.message =~ "overlaps the period of an existing version"

      assert [%{name: "first"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2021-06-01 00:00:00Z]) |> Ash.read!()
    end

    test "adjacent versions of one record are accepted, since they share no instant" do
      Ash.Seed.seed!(%EtsVersioned{id: 1, name: "early", valid_at: @early})

      assert {:ok, _} = create_at(1, "later", ~U[2021-01-01 00:00:00Z])

      assert [%{name: "early"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2020-06-01 00:00:00Z]) |> Ash.read!()

      assert [%{name: "later"}] =
               EtsVersioned |> Ash.Query.as_of(~U[2021-06-01 00:00:00Z]) |> Ash.read!()
    end

    test "another record's overlapping period is no concern of this one's" do
      assert {:ok, _} = create_at(1, "one", ~U[2020-01-01 00:00:00Z])
      assert {:ok, _} = create_at(2, "two", ~U[2020-01-01 00:00:00Z])
    end

    test "a bulk create must not overlap what is already stored" do
      Ash.Seed.seed!(%EtsVersioned{id: 1, name: "open", valid_at: @open})

      assert %Ash.BulkResult{status: :error, errors: [error]} =
               Ash.bulk_create([%{id: 1, name: "clash"}], EtsVersioned, :create,
                 return_errors?: true
               )

      assert %Ash.Error.Invalid{errors: [%Ash.Error.Changes.InvalidAttribute{field: :valid_at}]} =
               error
    end

    test "nor overlap its own earlier records" do
      assert %Ash.BulkResult{status: :error, errors: [error]} =
               Ash.bulk_create(
                 [%{id: 1, name: "first"}, %{id: 1, name: "second"}],
                 EtsVersioned,
                 :create,
                 return_errors?: true
               )

      assert %Ash.Error.Invalid{errors: [%Ash.Error.Changes.InvalidAttribute{field: :valid_at}]} =
               error
    end
  end

  describe "a period holding no instant" do
    @empty %Ash.Range{
      lower: ~U[2020-01-01 00:00:00Z],
      upper: ~U[2020-01-01 00:00:00Z],
      bounds: :"[)"
    }

    test "is refused by the type before any layer sees it" do
      assert_raise Ash.Error.Invalid, ~r/range must not be empty/, fn ->
        Ash.Seed.seed!(%EtsVersioned{id: 1, name: "nowhen", valid_at: @empty})
      end
    end

    test "is refused by the layer itself" do
      changeset = Ash.Changeset.for_create(EtsVersioned, :create, %{id: 1, name: "nowhen"})
      changeset = %{changeset | attributes: Map.put(changeset.attributes, :valid_at, @empty)}

      assert {:error, %Ash.Error.Changes.InvalidAttribute{field: :valid_at}} =
               Ash.DataLayer.create(EtsVersioned, changeset)
    end
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

  # Temporal says nothing about the inner type. Storage works over any ordered extent;
  # the `as_of` that reads it does not, in two different ways.
  describe "an extent that is not a period" do
    alias Ash.Test.Temporal.EtsIntegerExtent

    @first %Ash.Range{lower: 0, upper: 100, bounds: :"[)"}
    @second %Ash.Range{lower: 100, upper: nil, bounds: :"[)"}

    setup do
      on_exit(fn -> Ash.DataLayer.Ets.stop(EtsIntegerExtent) end)
    end

    test "the extent joins the storage key" do
      record = %EtsIntegerExtent{id: 1, name: "x", valid_over: @first}

      assert Ash.DataLayer.Ets.pkey_map(EtsIntegerExtent, record) == %{id: 1, valid_over: @first}
    end

    test "versions of one record that overlap on the extent are refused" do
      Ash.Seed.seed!(%EtsIntegerExtent{id: 1, name: "first", valid_over: @first})
      Ash.Seed.seed!(%EtsIntegerExtent{id: 1, name: "second", valid_over: @second})

      assert_raise Ash.Error.Invalid, ~r/overlap/i, fn ->
        Ash.Seed.seed!(%EtsIntegerExtent{
          id: 1,
          name: "clash",
          valid_over: %Ash.Range{lower: 50, upper: 150, bounds: :"[)"}
        })
      end
    end

    # `resolve_query_as_of/2` takes `:now`, a `DateTime` and `nil`, and nothing else.
    test "narrowing to a point on the extent is refused by core" do
      Ash.Seed.seed!(%EtsIntegerExtent{id: 1, name: "first", valid_over: @first})

      assert_raise FunctionClauseError, fn ->
        EtsIntegerExtent |> Ash.Query.as_of(50) |> Ash.read!()
      end
    end

    # Comparing the resolved clock value to an integer bound falls back to term order,
    # where every struct sorts above every number.
    test "a read naming no point silently answers from term order" do
      Ash.Seed.seed!(%EtsIntegerExtent{id: 1, name: "first", valid_over: @first})
      Ash.Seed.seed!(%EtsIntegerExtent{id: 1, name: "second", valid_over: @second})

      assert ["second"] = EtsIntegerExtent |> Ash.read!() |> Enum.map(& &1.name)
    end
  end
end
