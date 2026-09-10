defmodule Ash.Test.Resource.TemporalUnsupportedTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Error.Query.TemporalNotSupported
  alias Ash.Test.Temporal.RuntimeGated
  alias Ash.Test.Temporal.RuntimeGatedDataLayer
  alias Ash.Test.Temporal.Thing

  @as_of ~U[2026-03-01 00:00:00Z]

  setup do
    on_exit(fn -> RuntimeGatedDataLayer.supported?(true) end)
  end

  defp record, do: %RuntimeGated{id: 1, name: "one"}

  describe "when the data layer declines temporal" do
    setup do
      RuntimeGatedDataLayer.supported?(false)
      :ok
    end

    test "a read is refused" do
      assert {:error, %Ash.Error.Invalid{errors: [%TemporalNotSupported{resource: RuntimeGated}]}} =
               RuntimeGated |> Ash.Query.as_of(@as_of) |> Ash.read()
    end

    test "a create is refused" do
      changeset = Ash.Changeset.for_create(RuntimeGated, :create, %{id: 1, name: "one"})

      refute changeset.valid?
      assert [%TemporalNotSupported{resource: RuntimeGated}] = changeset.errors
    end

    test "an update is refused" do
      changeset = Ash.Changeset.for_update(record(), :update, %{name: "two"})

      refute changeset.valid?
      assert [%TemporalNotSupported{resource: RuntimeGated}] = changeset.errors
    end

    test "a destroy is refused" do
      changeset = Ash.Changeset.for_destroy(record(), :destroy, %{})

      refute changeset.valid?
      assert [%TemporalNotSupported{resource: RuntimeGated}] = changeset.errors
    end

    test "a bulk create is refused" do
      assert %Ash.BulkResult{status: :error, errors: [error]} =
               Ash.bulk_create([%{id: 1, name: "one"}], RuntimeGated, :create,
                 return_errors?: true
               )

      assert %Ash.Error.Invalid{errors: [%TemporalNotSupported{resource: RuntimeGated}]} = error
    end

    # A non-temporal resource may carry an as_of for a temporal relationship to use.
    test "a non-temporal resource is unaffected, read or write" do
      assert {:ok, _} = Thing |> Ash.Query.as_of(@as_of) |> Ash.read()
      assert Ash.Changeset.for_create(Thing, :create, %{name: "fine"}).valid?
    end
  end

  describe "when the data layer serves temporal" do
    test "a read is honoured" do
      assert {:ok, []} = RuntimeGated |> Ash.Query.as_of(@as_of) |> Ash.read()
    end

    test "a create is honoured" do
      assert Ash.Changeset.for_create(RuntimeGated, :create, %{id: 1, name: "one"}).valid?
    end
  end
end
