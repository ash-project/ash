defmodule Ash.FlowTest.BranchingTransactionMappingTest do
  @moduledoc false
  use ExUnit.Case, async: false
  alias Ash.Test.Flow.{Api, Org, User}
  alias Ash.Test.Flow.Flows.SignUpUser

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

    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    SignUpUser.run!(org.name, "Bruce", "Wayne")
    :ok
  end

  test "transaction map with internal branching and custom create step returns properly" do
    %{result: created_users} = Ash.Test.Flow.Flows.BranchingTransactionMapping.run("Org 1", [%{firstname: "Clark", lastname: "Kent"}])

    assert length(created_users) == 1
    assert hd(created_users).lastname == "Kent"
    assert hd(created_users).approved == true
  end

  test "transaction map with internal branching and custom update step returns properly" do
    %{result: updated_users} = Ash.Test.Flow.Flows.BranchingTransactionMapping.run("Org 1", [%{firstname: "Bruce", lastname: "Willis"}], %{update_users: true})

    assert length(updated_users) == 1
    assert hd(updated_users).lastname == "Willis"
    assert hd(updated_users).approved == true
  end
end
