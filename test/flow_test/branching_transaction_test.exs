defmodule Ash.FlowTest.BranchingTransactionTest do
  @moduledoc false
  use ExUnit.Case, async: false

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
  end

  test "branch with transaction inside BOOMS!" do
    user = User
           |> Ash.Query.for_read(:by_name, %{name: "Bruce"})
           |> Api.read!()
           |> List.first()
    assert user.email == nil
    assert user.approved == false
    Ash.Test.Flow.Flows.BranchingTransaction.run!(%{name: "Bruce"})
    assert user.email == "changed@example.com"
    assert user.approved == true
  end
end
