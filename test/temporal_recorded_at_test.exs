# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.TemporalRecordedAtTest do
  @moduledoc false
  use ExUnit.Case, async: false

  defmodule Recorded do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    temporal do
      recorded_at :recorded_at
    end

    actions do
      defaults [:read, :destroy, create: [:id, :name]]

      update :update do
        primary? true
        accept [:name]
      end

      update :update_atomic do
        accept [:name]
        require_atomic? true
      end

      update :stamp do
        change set_attribute(:touched_at, &DateTime.utc_now/0)
        change set_attribute(:recorded_at, &DateTime.utc_now/0)
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :name, :string, public?: true
      attribute :touched_at, :utc_datetime_usec
      create_timestamp :inserted_at
      update_timestamp :updated_at
    end
  end

  defmodule Declared do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    temporal do
      recorded_at :written_at
    end

    actions do
      defaults [:read, create: [:id]]
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      create_timestamp :inserted_at
      create_timestamp :written_at, public?: true
    end
  end

  setup do
    on_exit(fn ->
      Ash.DataLayer.Ets.stop(Recorded)
      Ash.DataLayer.Ets.stop(Declared)
    end)
  end

  @past ~U[2020-06-15 12:00:00.000000Z]
  @later ~U[2021-06-15 12:00:00.000000Z]

  describe "a resource that names a recorded_at attribute it doesn't declare" do
    test "gets a private, unwritable microsecond timestamp" do
      attribute = Ash.Resource.Info.attribute(Recorded, :recorded_at)

      assert attribute.type == Ash.Type.UtcDatetimeUsec
      refute attribute.public?
      refute attribute.writable?
      refute attribute.allow_nil?
      assert Ash.Resource.Info.temporal_recorded_at(Recorded) == :recorded_at
    end

    test "a resource that doesn't name one has none" do
      refute Ash.Resource.Info.temporal_recorded_at(Ash.Test.Temporal.EtsVersioned)
    end
  end

  describe "a write that isn't back-dated" do
    test "records the instant its period starts at" do
      record = Ash.create!(Recorded, %{id: 1, name: "a"})

      assert record.recorded_at == record.valid_at.lower
      assert record.inserted_at == record.valid_at.lower
    end
  end

  describe "a back-dated write" do
    test "stamps its other timestamps with as_of, and recorded_at with the wall clock" do
      before = DateTime.utc_now()
      record = Ash.create!(Recorded, %{id: 1, name: "a"}, as_of: @past)

      assert record.inserted_at == @past
      assert record.updated_at == @past
      assert record.valid_at.lower == @past
      assert DateTime.compare(record.recorded_at, before) in [:eq, :gt]
    end

    test "an update restamps the version it opens and leaves the one it split alone" do
      created = Ash.create!(Recorded, %{id: 1, name: "a"}, as_of: @past)
      before = DateTime.utc_now()

      updated =
        created
        |> Ash.Changeset.for_update(:update, %{name: "b"}, as_of: @later)
        |> Ash.update!()

      assert updated.updated_at == @later
      assert DateTime.compare(updated.recorded_at, before) in [:eq, :gt]

      assert [%{name: "a", recorded_at: recorded_at}] =
               Recorded |> Ash.Query.as_of(@past) |> Ash.read!()

      assert recorded_at == created.recorded_at
    end

    test "an atomic update restamps it with the wall clock" do
      created = Ash.create!(Recorded, %{id: 1, name: "a"}, as_of: @past)
      before = DateTime.utc_now()

      updated =
        created
        |> Ash.Changeset.for_update(:update_atomic, %{name: "b"}, as_of: @later)
        |> Ash.update!()

      assert updated.updated_at == @later
      assert DateTime.compare(updated.recorded_at, before) in [:eq, :gt]
    end

    test "set_attribute with &DateTime.utc_now/0 is the wall clock on recorded_at only" do
      created = Ash.create!(Recorded, %{id: 1, name: "a"}, as_of: @past)
      before = DateTime.utc_now()

      updated =
        created
        |> Ash.Changeset.for_update(:stamp, %{}, as_of: @later)
        |> Ash.update!()

      assert updated.touched_at == @later
      assert DateTime.compare(updated.recorded_at, before) in [:eq, :gt]
    end
  end

  describe "a resource that declares its own recorded_at attribute" do
    test "keeps it as declared, and it is not rewritten to as_of" do
      attribute = Ash.Resource.Info.attribute(Declared, :written_at)
      assert attribute.public?

      before = DateTime.utc_now()
      record = Ash.create!(Declared, %{id: 1}, as_of: @past)

      assert record.inserted_at == @past
      assert DateTime.compare(record.written_at, before) in [:eq, :gt]
    end
  end
end
