# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkReadErrorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Order do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    policies do
      policy action_type(:read) do
        access_type :strict
        authorize_if actor_present()
      end

      policy action_type([:create, :update, :destroy]) do
        authorize_if always()
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :state, :atom, allow_nil?: false, default: :pending, public?: true
    end

    actions do
      defaults [:read, create: []]

      update :cancel do
        require_atomic? false
        change fn changeset, _context -> changeset end
        change set_attribute(:state, :cancelled)
      end

      destroy :delete do
        require_atomic? false
        change fn changeset, _context -> changeset end
      end
    end

    code_interface do
      define :cancel_by_id, action: :cancel, get_by: [:id]
      define :delete_by_id, action: :delete, get_by: [:id]
    end
  end

  setup do
    order = Ash.create!(Order, %{}, authorize?: false)
    %{order: order, query: Ash.Query.do_filter(Order, id: order.id)}
  end

  describe "a non-atomic bulk update whose read fails" do
    test "returns the error from a get_by code interface", %{order: order} do
      assert {:error, %Ash.Error.Forbidden{}} = Order.cancel_by_id(order.id)
    end

    test "still raises from a bang get_by code interface", %{order: order} do
      assert_raise Ash.Error.Forbidden, fn -> Order.cancel_by_id!(order.id) end
    end

    test "returns the error in the bulk result", %{query: query} do
      assert %Ash.BulkResult{status: :error, error_count: 1, errors: [%Ash.Error.Forbidden{}]} =
               Ash.bulk_update(query, :cancel, %{},
                 strategy: [:atomic, :stream],
                 return_errors?: true,
                 stop_on_error?: false
               )
    end

    test "counts the error when errors aren't returned", %{query: query} do
      assert %Ash.BulkResult{status: :error, error_count: 1, errors: nil} =
               Ash.bulk_update(query, :cancel, %{},
                 strategy: [:atomic, :stream],
                 return_errors?: false,
                 stop_on_error?: false
               )
    end

    test "returns the error with stop_on_error?", %{query: query} do
      assert %Ash.BulkResult{status: :error, errors: [%Ash.Error.Forbidden{}]} =
               Ash.bulk_update(query, :cancel, %{},
                 strategy: [:atomic, :stream],
                 return_errors?: true,
                 stop_on_error?: true
               )
    end

    test "emits the error into a returned stream", %{query: query} do
      assert [{:error, %Ash.Error.Forbidden{}}] =
               query
               |> Ash.bulk_update(:cancel, %{},
                 strategy: [:atomic, :stream],
                 return_stream?: true,
                 return_records?: true
               )
               |> Enum.to_list()
    end

    test "updates when the read is allowed", %{order: order} do
      assert {:ok, %{state: :cancelled}} = Order.cancel_by_id(order.id, actor: %{id: 1})
    end
  end

  describe "a non-atomic bulk destroy whose read fails" do
    test "returns the error from a get_by code interface", %{order: order} do
      assert {:error, %Ash.Error.Forbidden{}} = Order.delete_by_id(order.id)
    end

    test "still raises from a bang get_by code interface", %{order: order} do
      assert_raise Ash.Error.Forbidden, fn -> Order.delete_by_id!(order.id) end
    end

    test "returns the error in the bulk result", %{query: query} do
      assert %Ash.BulkResult{status: :error, error_count: 1, errors: [%Ash.Error.Forbidden{}]} =
               Ash.bulk_destroy(query, :delete, %{},
                 strategy: [:atomic, :stream],
                 return_errors?: true,
                 stop_on_error?: false
               )
    end

    test "returns the error with stop_on_error?", %{query: query} do
      assert %Ash.BulkResult{status: :error, errors: [%Ash.Error.Forbidden{}]} =
               Ash.bulk_destroy(query, :delete, %{},
                 strategy: [:atomic, :stream],
                 return_errors?: true,
                 stop_on_error?: true
               )
    end

    test "emits the error into a returned stream", %{query: query} do
      assert [{:error, %Ash.Error.Forbidden{}}] =
               query
               |> Ash.bulk_destroy(:delete, %{},
                 strategy: [:atomic, :stream],
                 return_stream?: true,
                 return_records?: true
               )
               |> Enum.to_list()
    end

    test "destroys when the read is allowed", %{order: order} do
      assert :ok = Order.delete_by_id(order.id, actor: %{id: 1})
    end
  end
end
