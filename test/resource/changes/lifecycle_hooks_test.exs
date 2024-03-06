defmodule Ash.Test.Resource.Changes.LifecycleHooksTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule TimeMachine do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :update, :destroy]

      create :create_with_before_action do
        argument :caller, :term

        change before_action(fn changeset, _context ->
                 send(changeset.arguments.caller, changeset.phase)
                 changeset
               end)
      end

      create :create_with_before_transaction do
        argument :caller, :term

        change before_transaction(fn changeset, _context ->
                 send(changeset.arguments.caller, changeset.phase)

                 changeset
               end)
      end

      create :create_with_after_action do
        argument :caller, :term

        change after_action(fn changeset, record, _context ->
                 send(changeset.arguments.caller, changeset.phase)

                 {:ok, record}
               end)
      end

      create :create_with_after_transaction do
        argument :caller, :term

        change after_transaction(fn changeset, {:ok, record}, _context ->
                 send(changeset.arguments.caller, changeset.phase)

                 {:ok, record}
               end)
      end
    end
  end

  describe "before_action/2" do
    test "it is called before the action is run" do
      TimeMachine
      |> Ash.Changeset.for_create(:create_with_before_action,
        name: "Delorean DMC-12",
        caller: self()
      )
      |> Domain.create!()

      assert_received :before_action
    end
  end

  describe "before_transaction/2" do
    test "it is called before the transaction is run" do
      TimeMachine
      |> Ash.Changeset.for_create(:create_with_before_transaction,
        name: "Delorean DMC-12",
        caller: self()
      )
      |> Domain.create!()

      assert_received :before_transaction
    end
  end

  describe "after_action/2" do
    test "it is called after the action is run" do
      TimeMachine
      |> Ash.Changeset.for_create(:create_with_after_action,
        name: "Delorean DMC-12",
        caller: self()
      )
      |> Domain.create!()

      assert_received :after_action
    end
  end

  describe "after_transaction/2" do
    test "it is called after the transaction is run" do
      TimeMachine
      |> Ash.Changeset.for_create(:create_with_after_transaction,
        name: "Delorean DMC-12",
        caller: self()
      )
      |> Domain.create!()

      assert_received :after_transaction
    end
  end
end
