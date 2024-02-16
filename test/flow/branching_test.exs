defmodule Ash.Flow.BranchingTest do
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
  end

  test "branches return `nil` if they don't execute" do
    assert %Ash.Flow.Result{result: %{branch: nil, second_branch: nil}} =
             Ash.Test.Flow.Flows.Branching.run!()
  end

  test "branches return their output if they execute" do
    assert %Ash.Flow.Result{result: %{branch: "inner_branch didn't happen", second_branch: nil}} =
             Ash.Test.Flow.Flows.Branching.run!(%{do_branch: true})
  end

  test "nested branches return their output if they execute" do
    assert %Ash.Flow.Result{result: %{branch: "inner_branch happened", second_branch: nil}} =
             Ash.Test.Flow.Flows.Branching.run!(%{do_branch: true, do_inner_branch: true})
  end

  test "following branches return their output if they execute" do
    assert %Ash.Flow.Result{
             result: %{branch: "inner_branch happened", second_branch: "second_branch happened"}
           } =
             Ash.Test.Flow.Flows.Branching.run!(%{
               do_branch: true,
               do_inner_branch: true,
               do_second_branch: true
             })
  end
end
