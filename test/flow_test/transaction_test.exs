defmodule Ash.FlowTest.TransactionTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Flow.{Api, Org, User}

  alias Ash.Test.Flow.Flows.UnapproveAllUsers

  setup do
    ExUnit.CaptureLog.capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Api)
    end)

    on_exit(fn ->
      ExUnit.CaptureLog.capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)
  end

  test "a flow in a transaction can be run" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Api.create!()
    |> Ash.Changeset.for_update(:approve, %{})
    |> Api.update!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()
    |> Ash.Changeset.for_update(:approve, %{})
    |> Api.update!()

    UnapproveAllUsers.run!("Org 1")

    assert User |> Api.read!() |> Enum.all?(&(not &1.approved))
  end

  test "a flow in a transaction will be rolled back if an error is raised" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Api.create!()
    |> Ash.Changeset.for_update(:approve, %{})
    |> Api.update!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()
    |> Ash.Changeset.for_update(:approve, %{})
    |> Api.update!()

    assert_raise(Ash.Error.Unknown, ~r/uh oh!/, fn ->
      UnapproveAllUsers.run!("Org 1", %{error: :raise})
    end)

    assert User |> Api.read!() |> Enum.all?(& &1.approved)
  end
end
