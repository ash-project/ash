defmodule Ash.Flow.HaltingTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Flow.Api

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

  test "A step can halt" do
    assert %Ash.Flow.Result{errors: [%Ash.Error.Flow.Halted{reason: :not_on_step_a}]} =
             Ash.Test.Flow.Flows.Halting.run()
  end

  test "A step that halts will continue if its `halt_if` clause is no longer true" do
    assert %Ash.Flow.Result{errors: [%Ash.Error.Flow.Halted{reason: :not_on_step_a}]} =
             result = Ash.Test.Flow.Flows.Halting.run()

    assert %Ash.Flow.Result{errors: [%Ash.Error.Flow.Halted{reason: :not_on_step_b}]} =
             Ash.Test.Flow.Flows.Halting.run(%{on_step: :a}, resume: result)
  end

  test "A step that halts will continue if its `halt_if` clause is no longer true, and the result of steps is properly resumed" do
    assert %Ash.Flow.Result{errors: [%Ash.Error.Flow.Halted{reason: :not_on_step_b}]} =
             result = Ash.Test.Flow.Flows.Halting.run(%{on_step: :a})

    assert %Ash.Flow.Result{errors: [%Ash.Error.Flow.Halted{reason: :not_on_step_c}]} =
             result = Ash.Test.Flow.Flows.Halting.run(%{on_step: :b}, resume: result)

    assert %Ash.Flow.Result{errors: [], result: "c"} =
             Ash.Test.Flow.Flows.Halting.run(%{on_step: :c}, resume: result)
  end
end
