defmodule Ash.Flow.FlowCompositionTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Flow.Result
  alias Ash.Test.Flow.{Domain, Org, User}

  alias Ash.Test.Flow.Flows.{
    GetOrgAndUsers,
    GetOrgAndUsersAndUnapproveThem,
    GetOrgAndUsersAndUnapproveThemReturningCount
  }

  setup do
    ExUnit.CaptureLog.capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Domain)
    end)

    on_exit(fn ->
      ExUnit.CaptureLog.capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)
  end

  test "a flow can reference other flows" do
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

    org_id = org.id

    assert %Ash.Flow.Result{result: %{org: %{id: ^org_id}, users: users}} =
             GetOrgAndUsers.run!("Org 1")

    assert users |> Enum.map(& &1.first_name) |> Enum.sort() == ["abc", "def"]
  end

  test "a map will run nested steps over all elements" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Domain.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Ash.Changeset.force_change_attribute(:approved, true)
    |> Domain.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Ash.Changeset.force_change_attribute(:approved, true)
    |> Domain.create!()

    GetOrgAndUsersAndUnapproveThem.run!("Org 1")

    assert Enum.all?(User |> Domain.read!(), &(&1.approved == false))
  end

  test "a custom step can be used to introduce custom logic" do
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

    assert %Result{result: 2} = GetOrgAndUsersAndUnapproveThemReturningCount.run!("Org 1")
  end
end
