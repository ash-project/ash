defmodule Ash.Flow.SimpleFlowTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Flow.Result
  alias Ash.Test.Flow.{Domain, Org, User}

  alias Ash.Test.Flow.Flows.{
    GetOrgAndUsers,
    GetOrgAndUsersAndDestroyThem,
    GetOrgByName,
    SignUpAndDeleteUser,
    SignUpUser
  }

  setup do
    ExUnit.CaptureLog.capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Ash.Test.Flow.Domain)
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
      |> Domain.create!()

    org_id = org.id

    assert %Result{result: %{id: ^org_id}} = GetOrgByName.run!("Org 1")
  end

  test "a flow with multiple steps and dependencies can be run" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Domain.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Ash.Changeset.manage_relationship(:org, org.id, type: :append_and_remove)
    |> Domain.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Domain.create!()

    org_id = org.id

    assert %Result{result: %{org: %{id: ^org_id}, users: users}} = GetOrgAndUsers.run!("Org 1")

    assert users |> Enum.map(& &1.first_name) |> Enum.sort() == ["abc", "def"]
  end

  test "a flow with a destroy inside of a map works" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Domain.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Domain.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Domain.create!()

    GetOrgAndUsersAndDestroyThem.run!("Org 1")

    assert User |> Domain.read!() |> Enum.empty?()
  end

  test "a flow with a create and an update step works" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Domain.create!()

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
      |> Domain.create!()

    assert %Result{
             result: %{
               org: %{name: "Org 1"},
               user: %{first_name: "Bruce", last_name: "Wayne", approved: true}
             }
           } = SignUpAndDeleteUser.run!(org.name, "Bruce", "Wayne")

    assert Domain.read!(User) == []
  end
end
