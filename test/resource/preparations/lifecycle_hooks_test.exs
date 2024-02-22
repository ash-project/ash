defmodule Ash.Test.Resource.Preparations.LifecycleHooksTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.AnyApi, as: Api

  defmodule TimeMachine do
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    actions do
      read :read_with_before_action do
        argument :caller, :term

        prepare before_action(fn query ->
                  send(query.arguments.caller, query.phase)
                  query
                end)
      end

      read :read_with_after_action do
        argument :caller, :term

        prepare after_action(fn query, records ->
                  send(query.arguments.caller, query.phase)
                  {:ok, records}
                end)
      end

      read :read_with_multiple_before_actions do
        argument :caller, :term

        prepare before_action(fn query ->
                  send(query.arguments.caller, {query.phase, 1})
                  query
                end)

        prepare before_action(fn query ->
                  send(query.arguments.caller, {query.phase, 2})
                  query
                end)
      end

      read :read_with_multiple_after_actions do
        argument :caller, :term

        prepare after_action(fn query, records ->
                  send(query.arguments.caller, {query.phase, 1})
                  {:ok, records}
                end)

        prepare after_action(fn query, records ->
                  send(query.arguments.caller, {query.phase, 2})
                  {:ok, records}
                end)
      end
    end
  end

  describe "before_action/1" do
    test "it is called before the action is run" do
      TimeMachine
      |> Ash.Query.for_read(:read_with_before_action, caller: self())
      |> Api.read!()

      assert_received :before_action
    end

    test "multiple before actions have the same phase" do
      TimeMachine
      |> Ash.Query.for_read(:read_with_multiple_before_actions, caller: self())
      |> Api.read!()

      assert_received {:before_action, 1}
      assert_received {:before_action, 2}
    end
  end

  describe "after_action/1" do
    test "it is called after the action is run" do
      TimeMachine
      |> Ash.Query.for_read(:read_with_after_action, caller: self())
      |> Api.read!()

      assert_received :after_action
    end

    test "multiple after actions have the same phase" do
      TimeMachine
      |> Ash.Query.for_read(:read_with_multiple_after_actions, caller: self())
      |> Api.read!()

      assert_received {:after_action, 1}
      assert_received {:after_action, 2}
    end
  end
end
