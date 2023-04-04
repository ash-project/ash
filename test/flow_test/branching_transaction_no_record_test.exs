defmodule Ash.FlowTest.BranchingTransactionNoRecordTest do
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

  test "if user exists does not create user" do
    assert %Ash.Flow.Result{result: %{user_not_found: nil, get_user: user}} =
             Ash.Test.Flow.Flows.BranchingTransactionNoRecord.run!("Bruce")

    assert user.first_name == "Bruce"
  end

  test "if user does not exist create new org and user" do
    user =
      User
      |> Ash.Query.for_read(:by_name, %{name: "Bat"})
      |> Api.read!()

    assert user == []

    %Ash.Flow.Result{result: %{user_not_found: user, get_user: nil}} =
      Ash.Test.Flow.Flows.BranchingTransactionNoRecord.run!("Bat")

    assert user.create_user.first_name == "Bat"

    users =
      User
      |> Ash.Query.for_read(:read)
      |> Api.read!()

    assert length(users) == 2
  end
end
