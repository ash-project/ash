# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SignalsGraphTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.SignalsGraphTest.{Account, Cart, Order, Receipt, Shipment, Signals}

  defmodule Signals do
    @moduledoc false
    use Ash.Signals

    signals do
      signal :order_placed do
        description "An order was placed."
        argument :order_id, :uuid, allow_nil?: false
        argument :customer_id, :uuid
      end

      signal :order_shipped do
        phase :after_transaction
        argument :order_id, :uuid, allow_nil?: false
      end

      signal :points_awarded do
        argument :customer_id, :uuid
      end

      signal :unused
    end
  end

  defmodule JobDelivery do
    @moduledoc false
    @behaviour Ash.Signals.Delivery

    @impl true
    def deliver(_signals, _listeners, _opts, _context), do: :ok
  end

  defmodule Order do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :customer_id, :uuid, public?: true
      attribute :status, :atom, public?: true
    end

    actions do
      defaults [:read]

      create :place do
        accept [:customer_id]
        emit Ash.Test.SignalsGraphTest.Signals, :order_placed, fields: [order_id: :id]
      end

      create :import do
        accept [:customer_id]

        emit Ash.Test.SignalsGraphTest.Signals, :order_placed,
          dynamic?: true,
          description: """
          When the imported order is already paid.
          Unpaid orders are placed when they are paid.
          """
      end

      update :ship do
        accept [:status]
        require_atomic? false

        emit Ash.Test.SignalsGraphTest.Signals, :order_shipped,
          phase: :after_transaction,
          fields: [order_id: :id],
          where: [changing(:status)]
      end
    end
  end

  # emits like `Order.place`, from more than one action
  defmodule Cart do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:read]

      create :check_out do
        emit Ash.Test.SignalsGraphTest.Signals, :order_placed, fields: [order_id: :id]
      end

      create :reorder do
        emit Ash.Test.SignalsGraphTest.Signals, :order_placed, fields: [order_id: :id]
      end
    end
  end

  # listens, and emits in turn
  defmodule Account do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :customer_id, :uuid, public?: true
    end

    signals_in do
      from Ash.Test.SignalsGraphTest.Signals do
        on :order_placed, :award_points, get_by: [customer_id: :customer_id]
      end
    end

    actions do
      defaults [:read]

      update :award_points do
        emit Ash.Test.SignalsGraphTest.Signals, :points_awarded
      end
    end
  end

  defmodule Receipt do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :order_id, :uuid, public?: true
    end

    signals_in do
      from Ash.Test.SignalsGraphTest.Signals do
        on :order_placed, :send_receipt,
          action: :create,
          delivery: {Ash.Test.SignalsGraphTest.JobDelivery, queue: :mailers}
      end
    end

    actions do
      defaults [:read, create: [:order_id]]
    end
  end

  defmodule Shipment do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    signals_in do
      from Ash.Test.SignalsGraphTest.Signals do
        on :order_shipped, :track, batch?: true, description: "Tracks shipments in bulk."
        on :points_awarded, :notify_customer, action: :track
      end
    end

    actions do
      defaults [:read]

      action :track do
        argument :signals, {:array, :term}
        argument :signal, :term
        run fn _input, _ -> :ok end
      end
    end
  end

  defp build(opts \\ []) do
    Ash.Signals.Graph.build(Keyword.put_new(opts, :signal_modules, [Signals]))
  end

  describe "build/1" do
    test "returns each signal with its emitters and listeners" do
      assert [placed, shipped, awarded, unused] = build(resources: [Order])

      assert placed.signal.name == :order_placed

      assert [
               %{resource: Order, action: :place, action_type: :create},
               %{resource: Order, action: :import, emit: %{dynamic?: true}}
             ] = placed.emitters

      assert [
               %{resource: Account, name: :award_points},
               %{resource: Receipt, name: :send_receipt}
             ] =
               Enum.sort_by(placed.listeners, & &1.resource)

      assert [%{resource: Order, action: :ship}] = shipped.emitters
      assert [%{resource: Shipment, name: :track}] = shipped.listeners

      assert [%{resource: Account, action: :award_points}] = awarded.emitters
      assert [%{resource: Shipment, name: :notify_customer}] = awarded.listeners

      assert %{signal: %{name: :unused}, emitters: [], listeners: []} = unused
    end

    test "finds emitters in resources that listen to signals, without being given them" do
      assert [placed, _shipped, awarded, _unused] = build()

      assert placed.emitters == []
      assert [%{resource: Account}] = awarded.emitters
    end
  end

  describe "text/1" do
    test "lists the emitters and listeners of each signal" do
      assert Ash.Signals.Graph.text(build(resources: [Order])) == """
             Ash.Test.SignalsGraphTest.Signals

               :order_placed (after_action)
                 An order was placed.
                 emitted by:
                   Ash.Test.SignalsGraphTest.Order
                     :place            create
                     :import           create  dynamic
                       When the imported order is already paid.
                       Unpaid orders are placed when they are paid.
                 listened to by:
                   Ash.Test.SignalsGraphTest.Account
                     :award_points     update
                   Ash.Test.SignalsGraphTest.Receipt
                     :send_receipt     create  action: :create, delivery: Ash.Test.SignalsGraphTest.JobDelivery

               :order_shipped (after_transaction)
                 emitted by:
                   Ash.Test.SignalsGraphTest.Order
                     :ship             update  conditional
                 listened to by:
                   Ash.Test.SignalsGraphTest.Shipment
                     :track            action  batch
                       Tracks shipments in bulk.

               :points_awarded (after_action)
                 emitted by:
                   Ash.Test.SignalsGraphTest.Account
                     :award_points     update
                 listened to by:
                   Ash.Test.SignalsGraphTest.Shipment
                     :notify_customer  action  action: :track

               :unused (after_action)
                 emitted by:
                   (none)
                 listened to by:
                   (none)
             """
    end
  end

  describe "mermaid/1" do
    test "renders a flowchart of resources, their actions, and signals" do
      assert Ash.Signals.Graph.mermaid(build(resources: [Order])) == """
             flowchart LR
               subgraph r0["Ash.Test.SignalsGraphTest.Account"]
                 a0["award_points"]
               end
               subgraph r1["Ash.Test.SignalsGraphTest.Order"]
                 a1["import"]
                 a2["place"]
                 a3["ship"]
               end
               subgraph r2["Ash.Test.SignalsGraphTest.Receipt"]
                 a4["create"]
               end
               subgraph r3["Ash.Test.SignalsGraphTest.Shipment"]
                 a5["track"]
               end
               subgraph m0["Ash.Test.SignalsGraphTest.Signals"]
                 s0{{"order_placed"}}
                 s1{{"order_shipped<br/>(after_transaction)"}}
                 s2{{"points_awarded"}}
                 s3{{"unused"}}
               end
               a2 --> s0
               a1 -->|"dynamic"| s0
               s0 --> a0
               s0 -.->|"send_receipt via Ash.Test.SignalsGraphTest.JobDelivery"| a4
               a3 -->|"conditional"| s1
               s1 --> a5
               a0 --> s2
               s2 -->|"notify_customer"| a5
             """
    end

    test "renders the actions of a resource with the same edges as one node" do
      assert Ash.Signals.Graph.mermaid(build(resources: [Cart, Order])) == """
             flowchart LR
               subgraph r0["Ash.Test.SignalsGraphTest.Account"]
                 a0["award_points"]
               end
               subgraph r1["Ash.Test.SignalsGraphTest.Cart"]
                 a1["check_out<br/>reorder"]
               end
               subgraph r2["Ash.Test.SignalsGraphTest.Order"]
                 a2["import"]
                 a3["place"]
                 a4["ship"]
               end
               subgraph r3["Ash.Test.SignalsGraphTest.Receipt"]
                 a5["create"]
               end
               subgraph r4["Ash.Test.SignalsGraphTest.Shipment"]
                 a6["track"]
               end
               subgraph m0["Ash.Test.SignalsGraphTest.Signals"]
                 s0{{"order_placed"}}
                 s1{{"order_shipped<br/>(after_transaction)"}}
                 s2{{"points_awarded"}}
                 s3{{"unused"}}
               end
               a1 --> s0
               a3 --> s0
               a2 -->|"dynamic"| s0
               s0 --> a0
               s0 -.->|"send_receipt via Ash.Test.SignalsGraphTest.JobDelivery"| a5
               a4 -->|"conditional"| s1
               s1 --> a6
               a0 --> s2
               s2 -->|"notify_customer"| a6
             """
    end
  end

  describe "mix ash.signals" do
    import ExUnit.CaptureIO

    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "ash_signals_#{System.unique_integer([:positive])}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      %{tmp_dir: tmp_dir}
    end

    test "writes the signals of the given signal modules", %{tmp_dir: tmp_dir} do
      output = Path.join(tmp_dir, "signals.txt")

      capture_io(fn ->
        Mix.Tasks.Ash.Signals.run(["Ash.Test.SignalsGraphTest.Signals", "--output", output])
      end)

      text = File.read!(output)
      assert text =~ "Ash.Test.SignalsGraphTest.Signals"
      assert text =~ ":send_receipt"
      refute text =~ "Ash.Test.SignalsTest"
    end

    test "writes a mermaid flowchart", %{tmp_dir: tmp_dir} do
      output = Path.join(tmp_dir, "signals.mmd")

      capture_io(fn ->
        Mix.Tasks.Ash.Signals.run(
          ~w(Ash.Test.SignalsGraphTest.Signals --format mermaid --output #{output})
        )
      end)

      assert File.read!(output) =~ ~r/\Aflowchart LR\n/

      output = Path.join(tmp_dir, "signals.md")

      capture_io(fn ->
        Mix.Tasks.Ash.Signals.run(
          ~w(Ash.Test.SignalsGraphTest.Signals --format md --output #{output})
        )
      end)

      assert File.read!(output) =~ ~r/\A```mermaid\nflowchart LR\n.*```\n\z/s
    end

    test "rejects modules that aren't signal modules" do
      assert_raise Mix.Error, ~r/is not an `Ash.Signals` module/, fn ->
        Mix.Tasks.Ash.Signals.run(["Ash.Test.SignalsGraphTest.Order"])
      end
    end

    test "rejects unknown formats, and images without an output" do
      assert_raise Mix.Error, ~r/Invalid format `nope`/, fn ->
        Mix.Tasks.Ash.Signals.run(~w(Ash.Test.SignalsGraphTest.Signals --format nope))
      end

      assert_raise Mix.Error, ~r/requires the `--output` option/, fn ->
        Mix.Tasks.Ash.Signals.run(~w(Ash.Test.SignalsGraphTest.Signals --format svg))
      end
    end
  end
end
