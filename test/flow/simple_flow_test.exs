defmodule Ash.Flow.SimpleFlowTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Flow.Result
  alias Ash.Test.Flow.{Api, Org, User}

  alias Ash.Test.Flow.Flows.{
    GetOrgAndUsers,
    GetOrgAndUsersAndDestroyThem,
    GetOrgByName,
    SignUpAndDeleteUser,
    SignUpUser
  }

  setup do
    ExUnit.CaptureLog.capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Ash.Test.Flow.Api)
    end)

    on_exit(fn ->
      ExUnit.CaptureLog.capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)
  end

  test "a simple flow can be run" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    org_id = org.id

    assert %Result{result: %{id: ^org_id}} = GetOrgByName.run!("Org 1")
  end

  test "a flow with multiple steps and dependencies can be run" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Ash.Changeset.manage_relationship(:org, org.id, type: :append_and_remove)
    |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()

    org_id = org.id

    assert %Result{result: %{org: %{id: ^org_id}, users: users}} = GetOrgAndUsers.run!("Org 1")

    assert users |> Enum.map(& &1.first_name) |> Enum.sort() == ["abc", "def"]
  end

  test "a flow with a destroy inside of a map works" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()

    GetOrgAndUsersAndDestroyThem.run!("Org 1")

    assert User |> Api.read!() |> Enum.empty?()
  end

  test "a flow with a create and an update step works" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    assert %Result{
             result: %{
               org: %{name: "Org 1"},
               user: %{first_name: "Bruce", last_name: "Wayne", approved: true}
             }
           } = SignUpUser.run!(org.name, "Bruce", "Wayne")
  end

  test "a flow with a create and an update and a destroy step works" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    assert %Result{
             result: %{
               org: %{name: "Org 1"},
               user: %{first_name: "Bruce", last_name: "Wayne", approved: true}
             }
           } = SignUpAndDeleteUser.run!(org.name, "Bruce", "Wayne")

    assert Api.read!(User) == []
  end
end
