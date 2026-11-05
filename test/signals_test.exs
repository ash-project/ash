# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SignalsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Signals.Test

  alias Ash.Test.SignalsTest.{Comment, FeedItem, Post, Signals}

  defmodule Signals do
    @moduledoc false
    use Ash.Signals

    signals do
      signal :post_created do
        phase(:after_action)
        argument :post_id, :uuid, allow_nil?: false
        argument :title, :string
      end

      signal :post_archiving do
        phase(:before_action)
        argument :post_id, :uuid, allow_nil?: false
      end
    end
  end

  defmodule OrderSignals do
    @moduledoc false
    use Ash.Signals

    signals do
      signal :order_placed do
        phase(:after_action)
        argument :order_id, :uuid, allow_nil?: false
        argument :customer_id, :uuid, allow_nil?: false
        argument :note, :string
      end
    end
  end

  # only for listeners that fail verification, which would otherwise run when OrderSignals is
  # emitted, since tests don't consolidate signals
  defmodule VerifySignals do
    @moduledoc false
    use Ash.Signals

    signals do
      signal :order_placed do
        phase(:after_action)
        argument :order_id, :uuid, allow_nil?: false
      end
    end
  end

  # records what it is asked to deliver, instead of running the listeners
  defmodule RecordingDelivery do
    @moduledoc false
    @behaviour Ash.Signals.Delivery

    @impl true
    def deliver(signals, listeners, opts, context) do
      send(self(), {:delivered, signals, listeners, opts, context})
      if opts[:fail?], do: {:error, "delivery failed"}, else: :ok
    end

    @impl true
    def verify_listener(%{action: :rejected}, _signal, _opts, _dsl_state),
      do: {:error, "the delivery rejects this listener"}

    def verify_listener(_listener, _signal, _opts, _dsl_state), do: :ok
  end

  defmodule DeliverySignals do
    @moduledoc false
    use Ash.Signals

    signals do
      signal :queued do
        phase(:after_action)
        argument :order_id, :uuid, allow_nil?: false
      end

      signal :failing do
        phase(:after_action)
      end

      # only for listeners that fail verification
      signal :verified do
        phase(:after_action)
        argument :order_id, :uuid
      end
    end
  end

  defmodule QueuedReceipt do
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
      from Ash.Test.SignalsTest.DeliverySignals do
        on(:queued, :record, delivery: {Ash.Test.SignalsTest.RecordingDelivery, queue: :orders})
        on(:queued, :record_now, action: :record)

        on(:failing, :record_failing,
          action: :record,
          delivery: {Ash.Test.SignalsTest.RecordingDelivery, fail?: true}
        )
      end
    end

    actions do
      defaults [:read]

      create :record do
        accept [:order_id]
      end
    end
  end

  # listens to the same signal twice with the same action, told apart by `key`
  defmodule LedgerEntry do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :account_id, :uuid, public?: true
    end

    signals_in do
      from Ash.Test.SignalsTest.OrderSignals do
        on(:order_placed, :record_for_order,
          action: :record,
          inputs: [account_id: :order_id]
        )

        on(:order_placed, :record_for_customer,
          action: :record,
          inputs: [account_id: :customer_id]
        )
      end
    end

    actions do
      defaults [:read]

      create :record do
        accept [:account_id]
      end
    end
  end

  # `signal_id` is only set from the signal when it's mapped explicitly
  defmodule SignalLog do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :order_id, :uuid, public?: true
      attribute :signal_id, :uuid, public?: true
      attribute :source_signal_id, :uuid, public?: true
    end

    signals_in do
      from Ash.Test.SignalsTest.OrderSignals do
        on(:order_placed, :log_unmapped)
        on(:order_placed, :log_mapped, inputs: [:order_id, source_signal_id: :signal_id])
      end
    end

    actions do
      defaults [:read]

      create :log_unmapped do
        accept [:order_id, :signal_id]
      end

      create :log_mapped do
        accept [:order_id, :source_signal_id]
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
      attribute :note, :string, public?: true
    end

    # creates a receipt per signal, from the signal fields named like its inputs
    signals_in do
      from Ash.Test.SignalsTest.OrderSignals do
        on(:order_placed, :record)
      end
    end

    validations do
      validate attribute_does_not_equal(:note, "fail")
    end

    actions do
      defaults [:read]

      create :record do
        accept [:order_id, :note]
      end
    end
  end

  defmodule Reference do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :reference, :uuid, public?: true
    end

    signals_in do
      from Ash.Test.SignalsTest.OrderSignals do
        on(:order_placed, :record, inputs: [reference: :order_id])
      end
    end

    actions do
      defaults [:read]

      create :record do
        accept [:reference]
      end
    end
  end

  defmodule Customer do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :status, :string, public?: true, default: "new"
    end

    # banned customers are not marked, because the read action filters them out
    signals_in do
      from Ash.Test.SignalsTest.OrderSignals do
        on(:order_placed, :mark_ordered, read_action: :active, get_by: [id: :customer_id])
      end
    end

    actions do
      defaults [:read, create: [:status]]

      read :active do
        filter expr(status != "banned")
      end

      update :mark_ordered do
        change set_attribute(:status, "ordered")
      end
    end
  end

  defmodule CartItem do
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
      from Ash.Test.SignalsTest.OrderSignals do
        on(:order_placed, :clear, read_action: :for_customer, args: [:customer_id])
      end
    end

    actions do
      defaults [:read, create: [:customer_id]]

      read :for_customer do
        argument :customer_id, :uuid, allow_nil?: false
        filter expr(customer_id == ^arg(:customer_id))
      end

      destroy :clear
    end
  end

  defmodule OrderAudit do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain

    signals_in do
      from Ash.Test.SignalsTest.OrderSignals do
        on(:order_placed, :log, batch?: true)
      end
    end

    actions do
      action :log do
        argument :signals, {:array, Ash.Test.SignalsTest.OrderSignals.OrderPlaced},
          allow_nil?: false

        run fn input, _ ->
          send(self(), {:audited, Enum.map(input.arguments.signals, & &1.order_id)})
          :ok
        end
      end
    end
  end

  defmodule ArticleSignals do
    @moduledoc false
    use Ash.Signals

    signals do
      # emitted `:after_action`, the default
      signal :article_published do
        argument :article_id, :uuid, allow_nil?: false
        argument :title, :string
        argument :previous_title, :string
        argument :by, :string
      end

      signal :article_drafting do
        phase(:before_action)
        argument :title, :string, allow_nil?: false
      end

      signal :article_committed do
        phase(:after_transaction)
        argument :article_id, :uuid, allow_nil?: false
      end

      signal :digest_requested do
        phase(:after_action)
        argument :topic, :string, allow_nil?: false
      end
    end
  end

  defmodule Article do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    actions do
      defaults [:read]

      create :publish do
        primary? true
        accept [:title]

        emit Ash.Test.SignalsTest.ArticleSignals, :article_published,
          fields: [article_id: :id],
          values: [by: actor(:name)],
          description: "Whenever an article is published."
      end

      update :rename do
        accept [:title]

        emit Ash.Test.SignalsTest.ArticleSignals, :article_published,
          fields: [:title, article_id: :id]
      end

      update :retitle do
        accept [:title]
        require_atomic? false

        emit Ash.Test.SignalsTest.ArticleSignals, :article_published,
          fields: [:title, article_id: :id],
          values: [previous_title: previous(:title)]
      end

      create :draft do
        accept [:title]

        emit Ash.Test.SignalsTest.ArticleSignals, :article_drafting, phase: :before_action
      end

      create :commit do
        accept [:title]

        emit Ash.Test.SignalsTest.ArticleSignals, :article_committed,
          phase: :after_transaction,
          fields: [article_id: :id]
      end

      update :commit_update do
        accept [:title]
        require_atomic? false

        emit Ash.Test.SignalsTest.ArticleSignals, :article_committed,
          phase: :after_transaction,
          fields: [article_id: :id]
      end

      destroy :retract do
        require_atomic? false

        emit Ash.Test.SignalsTest.ArticleSignals, :article_committed,
          phase: :after_transaction,
          fields: [article_id: :id]
      end

      action :request_digest do
        argument :topic, :string, allow_nil?: false
        emit Ash.Test.SignalsTest.ArticleSignals, :digest_requested
        run fn _, _ -> :ok end
      end
    end
  end

  defmodule EmitPostsCreated do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def batch_change(changesets, _opts, _context), do: changesets

    @impl true
    def after_batch([{changeset, _} | _] = changesets_and_posts, _opts, _context) do
      payloads =
        Enum.map(changesets_and_posts, fn {_, post} -> %{post_id: post.id, title: post.title} end)

      case Ash.Signals.emit_many(changeset, Ash.Test.SignalsTest.Signals, :post_created, payloads) do
        :ok -> Enum.map(changesets_and_posts, fn {_, post} -> {:ok, post} end)
        {:error, error} -> [{:error, error}]
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    actions do
      defaults [:read]

      create :create do
        accept [:title]
        argument :emit_opts, :term, default: []
        emit Ash.Test.SignalsTest.Signals, :post_created, dynamic?: true

        # emitting from a hook that isn't the first one
        change after_action(fn _changeset, post, _context -> {:ok, post} end)

        change fn changeset, _ ->
          Ash.Changeset.after_action(changeset, fn changeset, post ->
            opts = Ash.Changeset.get_argument(changeset, :emit_opts)

            with :ok <-
                   Ash.Signals.emit(
                     changeset,
                     Ash.Test.SignalsTest.Signals,
                     :post_created,
                     %{post_id: post.id, title: post.title},
                     opts
                   ) do
              {:ok, post}
            end
          end)
        end
      end

      create :create_in_batches do
        accept [:title]
        emit Ash.Test.SignalsTest.Signals, :post_created, dynamic?: true
        change EmitPostsCreated
      end

      create :emit_in_wrong_phase do
        accept [:title]
        emit Ash.Test.SignalsTest.Signals, :post_created, dynamic?: true

        change fn changeset, _ ->
          Ash.Changeset.before_action(changeset, fn changeset ->
            Ash.Signals.emit(changeset, Ash.Test.SignalsTest.Signals, :post_created, %{
              post_id: Ash.UUID.generate()
            })

            changeset
          end)
        end
      end

      action :announce, :string do
        emit Ash.Test.SignalsTest.Signals, :post_created, dynamic?: true
        emit Ash.Test.SignalsTest.OrderSignals, :order_placed, dynamic?: true
        emit Ash.Test.SignalsTest.DeliverySignals, :queued, dynamic?: true
        emit Ash.Test.SignalsTest.DeliverySignals, :failing, dynamic?: true
        run fn _input, _ -> {:ok, "announced"} end
      end

      update :archive do
        require_atomic? false
        emit Ash.Test.SignalsTest.Signals, :post_archiving, phase: :before_action, dynamic?: true

        change fn changeset, _ ->
          Ash.Changeset.before_action(changeset, fn changeset ->
            :ok =
              Ash.Signals.emit(changeset, Ash.Test.SignalsTest.Signals, :post_archiving, %{
                post_id: changeset.data.id
              })

            changeset
          end)
        end
      end
    end
  end

  defmodule NotEmitter do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      create :create do
        change fn changeset, _ ->
          Ash.Changeset.after_action(changeset, fn changeset, record ->
            Ash.Signals.emit(changeset, Ash.Test.SignalsTest.Signals, :post_created, %{
              post_id: record.id
            })

            {:ok, record}
          end)
        end
      end
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :post_id, :uuid, public?: true
      attribute :body, :string, public?: true
    end

    signals_in do
      from Ash.Test.SignalsTest.Signals do
        on(:post_created, :welcome)
      end
    end

    actions do
      defaults [:read, create: [:post_id, :body]]

      action :welcome do
        argument :signal, Ash.Test.SignalsTest.Signals.PostCreated, allow_nil?: false

        run fn input, context ->
          signal = input.arguments.signal
          send(self(), {:listened, :welcome, signal.title})
          actor_name = if context.actor, do: context.actor.name, else: "nobody"
          body = "Welcome to #{signal.title}, from #{actor_name}"

          with {:ok, _comment} <-
                 Comment
                 |> Ash.Changeset.for_create(:create, %{post_id: signal.post_id, body: body})
                 |> Ash.create() do
            :ok
          end
        end
      end
    end
  end

  defmodule FeedItem do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :post_id, :uuid, public?: true
    end

    signals_in do
      from Ash.Test.SignalsTest.Signals do
        on(:post_created, :add)
        on(:post_archiving, :remove)
      end
    end

    actions do
      defaults [:read, create: [:post_id]]

      action :add do
        argument :signal, Ash.Test.SignalsTest.Signals.PostCreated, allow_nil?: false

        run fn input, _ ->
          signal = input.arguments.signal
          send(self(), {:listened, :add, signal.title})

          if signal.title == "fail" do
            {:error, "feed is closed"}
          else
            FeedItem
            |> Ash.Changeset.for_create(:create, %{post_id: signal.post_id})
            |> Ash.create!()

            :ok
          end
        end
      end

      action :remove do
        argument :signal, Ash.Test.SignalsTest.Signals.PostArchiving, allow_nil?: false

        run fn input, _ ->
          send(self(), {:archiving, input.arguments.signal.post_id})
          :ok
        end
      end
    end
  end

  defmodule PathSignals do
    @moduledoc false
    use Ash.Signals

    signals do
      signal :started do
        argument :run_id, :uuid, allow_nil?: false
        argument :target_id, :uuid
      end

      signal :relayed do
        argument :run_id, :uuid, allow_nil?: false
      end
    end
  end

  defmodule PathRun do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :target_id, :uuid, public?: true
    end

    actions do
      create :start do
        accept [:target_id]
        emit Ash.Test.SignalsTest.PathSignals, :started, fields: [run_id: :id]
      end
    end
  end

  defmodule StoreSignalPath do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context) do
      Ash.Changeset.force_change_attribute(
        changeset,
        :signal_path,
        changeset.context[:signal_path]
      )
    end
  end

  # stores the `signal_path` of each listener, whose actions only signals may run
  defmodule PathRecord do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :run_id, :uuid, public?: true
      attribute :signal_path, :term, public?: true
    end

    signals_in do
      from Ash.Test.SignalsTest.PathSignals do
        on(:started, :record_started, action: :record)
        on(:started, :observe)
        on(:started, :relay)
        on(:relayed, :record_relayed)
      end
    end

    policies do
      policy action([:record, :observe, :relay]) do
        authorize_if signal_in(Ash.Test.SignalsTest.PathSignals, :started)
      end

      policy action(:record_relayed) do
        authorize_if signal_in(Ash.Test.SignalsTest.PathSignals, :relayed)
      end
    end

    actions do
      defaults [:read]

      create :record do
        accept [:run_id]
        change Ash.Test.SignalsTest.StoreSignalPath
      end

      # a listener that emits a signal of its own
      create :relay do
        accept [:run_id]
        change Ash.Test.SignalsTest.StoreSignalPath
        emit Ash.Test.SignalsTest.PathSignals, :relayed
      end

      create :record_relayed do
        accept [:run_id]
        change Ash.Test.SignalsTest.StoreSignalPath
      end

      action :observe do
        argument :signal, Ash.Test.SignalsTest.PathSignals.Started, allow_nil?: false

        run fn input, _ ->
          send(self(), {:observed, input.context[:signal_path]})
          :ok
        end
      end
    end
  end

  defmodule PathTarget do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :signal_path, :term, public?: true
    end

    signals_in do
      from Ash.Test.SignalsTest.PathSignals do
        on(:started, :touch, get_by: [id: :target_id])
      end
    end

    # the read that selects the records to update is run from the signal too
    policies do
      policy action([:read, :touch]) do
        authorize_if signal_in(Ash.Test.SignalsTest.PathSignals, :started)
      end
    end

    actions do
      defaults [:read, :create]

      update :touch do
        require_atomic? false
        change Ash.Test.SignalsTest.StoreSignalPath
      end
    end
  end

  defp create_post(title, opts \\ []) do
    Post
    |> Ash.Changeset.for_create(:create, %{title: title}, opts)
    |> Ash.create()
  end

  # the messages `Ash.Signals.Test` sends, as opposed to the ones the listeners here send
  defp signal_message?({:ash_signal_emitted, _, _, _, _}), do: true
  defp signal_message?({ref, _, _, _}) when is_reference(ref), do: true
  defp signal_message?(_), do: false

  defp comment_bodies, do: Comment |> Ash.read!() |> Enum.map(& &1.body)

  describe "signal modules" do
    test "define a struct for their arguments, and a signal_id" do
      signal_id = Ash.UUIDv7.generate()

      assert {:ok, %Signals.PostCreated{title: "hi", signal_id: ^signal_id}} =
               Signals.PostCreated.new(%{
                 signal_id: signal_id,
                 post_id: Ash.UUID.generate(),
                 title: "hi"
               })

      assert {:error, _} = Signals.PostCreated.new(%{signal_id: signal_id, title: "no id"})
      assert {:error, _} = Signals.PostCreated.new(%{post_id: Ash.UUID.generate()})
    end

    test "listeners are found without either side depending on the other" do
      assert [
               %{resource: Comment, action: :welcome},
               %{resource: FeedItem, action: :add}
             ] = Ash.Signals.Registry.listeners(Signals, :post_created)

      assert [%{resource: FeedItem, action: :remove}] =
               Ash.Signals.Registry.listeners(Signals, :post_archiving)
    end
  end

  describe "Ash.Signals.emit" do
    test "runs every listener" do
      assert {:ok, post} = create_post("Signals")

      assert [%{post_id: post_id}] = Ash.read!(FeedItem)
      assert post_id == post.id
      assert comment_bodies() == ["Welcome to Signals, from nobody"]
    end

    test "listeners run as the emitter's actor by default" do
      assert {:ok, _post} = create_post("Signals", actor: %{name: "zach"})
      assert comment_bodies() == ["Welcome to Signals, from zach"]
    end

    test "the emitter can choose the actor" do
      assert {:ok, _post} =
               Post
               |> Ash.Changeset.for_create(
                 :create,
                 %{title: "Signals", emit_opts: [actor: %{name: "system"}]},
                 actor: %{name: "zach"}
               )
               |> Ash.create()

      assert comment_bodies() == ["Welcome to Signals, from system"]
    end

    test "the emitter can pass a scope" do
      assert {:ok, _post} =
               Post
               |> Ash.Changeset.for_create(
                 :create,
                 %{title: "Signals", emit_opts: [scope: %{actor: %{name: "scoped"}}]},
                 actor: %{name: "zach"}
               )
               |> Ash.create()

      assert comment_bodies() == ["Welcome to Signals, from scoped"]
    end

    test "explicit options take precedence over the scope" do
      assert {:ok, _post} =
               Post
               |> Ash.Changeset.for_create(
                 :create,
                 %{
                   title: "Signals",
                   emit_opts: [scope: %{actor: %{name: "scoped"}}, actor: %{name: "explicit"}]
                 },
                 actor: %{name: "zach"}
               )
               |> Ash.create()

      assert comment_bodies() == ["Welcome to Signals, from explicit"]
    end

    test "a listener error fails the emitting action" do
      assert {:error, error} = create_post("fail")
      assert Exception.message(error) =~ "feed is closed"
    end

    test "signals can be emitted in before_action" do
      {:ok, post} = create_post("Signals")
      post |> Ash.Changeset.for_update(:archive) |> Ash.update!()
      post_id = post.id
      assert_received {:archiving, ^post_id}
    end

    test "generic actions can emit signals" do
      post_id = Ash.UUID.generate()

      assert "announced" =
               Post
               |> Ash.ActionInput.for_action(:announce, %{})
               |> Ash.ActionInput.after_action(fn input, result ->
                 with :ok <-
                        Ash.Signals.emit(input, Signals, :post_created, %{
                          post_id: post_id,
                          title: "Generic"
                        }) do
                   {:ok, result}
                 end
               end)
               |> Ash.run_action!()

      assert [%{post_id: ^post_id}] = Ash.read!(FeedItem)
      assert comment_bodies() == ["Welcome to Generic, from nobody"]
    end

    test "generic actions can only emit signals during the signal's phase" do
      assert_raise Ash.Error.Unknown, ~r/the action input is in `:before_action`/, fn ->
        Post
        |> Ash.ActionInput.for_action(:announce, %{})
        |> Ash.ActionInput.before_action(fn input ->
          Ash.Signals.emit(input, Signals, :post_created, %{post_id: Ash.UUID.generate()})
          input
        end)
        |> Ash.run_action!()
      end
    end

    test "raises when emitted outside of the signal's phase" do
      assert_raise Ash.Error.Unknown, ~r/can only be emitted during `:after_action`/, fn ->
        Post
        |> Ash.Changeset.for_create(:emit_in_wrong_phase, %{title: "x"})
        |> Ash.create()
      end
    end

    test "raises when the action does not declare the signal" do
      assert_raise Ash.Error.Unknown,
                   ~r/Action `:create` of .*NotEmitter does not emit `:post_created`/,
                   fn ->
                     NotEmitter |> Ash.Changeset.for_create(:create, %{}) |> Ash.create()
                   end
    end
  end

  describe "react_to_signal" do
    test "delivers a signal to its listeners as if it had been emitted, without emitting it" do
      order_id = Ash.UUID.generate()
      customer_id = Ash.UUID.generate()

      assert :ok =
               refute_emits_signal(OrderSignals, :order_placed, fn ->
                 react_to_signal(
                   OrderSignals,
                   :order_placed,
                   %{order_id: order_id, customer_id: customer_id},
                   only: LedgerEntry
                 )
               end)

      assert LedgerEntry |> Ash.read!() |> Enum.map(& &1.account_id) |> Enum.sort() ==
               Enum.sort([order_id, customer_id])
    end

    test "delivers only to the given listeners, and several signals together" do
      orders =
        for _ <- 1..2, do: %{order_id: Ash.UUID.generate(), customer_id: Ash.UUID.generate()}

      assert :ok =
               react_to_signal(OrderSignals, :order_placed, orders,
                 only: {LedgerEntry, :record_for_customer}
               )

      assert LedgerEntry |> Ash.read!() |> Enum.map(& &1.account_id) |> Enum.sort() ==
               orders |> Enum.map(& &1.customer_id) |> Enum.sort()
    end

    test "raises when no listener matches only" do
      assert_raise ArgumentError, ~r/No listener of `:order_placed`/, fn ->
        react_to_signal(
          OrderSignals,
          :order_placed,
          %{order_id: Ash.UUID.generate(), customer_id: Ash.UUID.generate()},
          only: Post
        )
      end
    end

    test "delivers through each listener's delivery, unless inline?" do
      order_id = Ash.UUID.generate()

      assert :ok =
               react_to_signal(DeliverySignals, :queued, %{order_id: order_id},
                 actor: %{name: "zach"}
               )

      assert_received {:delivered, [%DeliverySignals.Queued{order_id: ^order_id}],
                       [%{name: :record}], [queue: :orders], %{actor: %{name: "zach"}}}

      # only the inline listener ran
      assert [%{order_id: ^order_id}] = Ash.read!(QueuedReceipt)

      assert :ok = react_to_signal(DeliverySignals, :queued, %{order_id: order_id}, inline?: true)

      refute_received {:delivered, _, _, _, _}
      assert [_, _, _] = Ash.read!(QueuedReceipt)
    end
  end

  describe "assert_emits_signal and refute_emits_signal" do
    test "assert the signals emitted by a function, and return its result" do
      assert {:ok, %Post{title: "Signals"}} =
               assert_emits_signal(Signals, :post_created, fn -> create_post("Signals") end)

      assert {:ok, %Post{}} =
               assert_emits_signal(Signals, :post_created, %{title: "Signals"}, fn ->
                 create_post("Signals")
               end)
    end

    test "patterns can bind variables" do
      {:ok, post} =
        assert_emits_signal(Signals, :post_created, %{post_id: post_id}, fn ->
          create_post("Bound")
        end)

      assert post_id == post.id
    end

    test "patterns can use pins" do
      title = "Pinned"

      assert_emits_signal(Signals, :post_created, %{title: ^title}, fn ->
        create_post("Pinned")
      end)

      assert_raise ExUnit.AssertionError, fn ->
        assert_emits_signal(Signals, :post_created, %{title: ^title}, fn ->
          create_post("Other")
        end)
      end
    end

    test "fail with the signals that were emitted" do
      error =
        assert_raise ExUnit.AssertionError, fn ->
          assert_emits_signal(Signals, :post_created, %{title: "Other"}, fn ->
            create_post("Signals")
          end)
        end

      assert error.message =~ "none of the emitted :post_created signals matched"
      assert error.message =~ ~s(title: "Signals")

      assert_raise ExUnit.AssertionError, ~r/no :post_created signals were emitted/, fn ->
        assert_emits_signal(Signals, :post_created, fn -> :ok end)
      end
    end

    test "refute_emits_signal" do
      assert :ok = refute_emits_signal(Signals, :post_created, fn -> :ok end)

      refute_emits_signal(Signals, :post_created, %{title: "Other"}, fn ->
        create_post("Signals")
      end)

      assert_raise ExUnit.AssertionError, ~r/not to be emitted, but got/, fn ->
        refute_emits_signal(Signals, :post_created, fn -> create_post("Signals") end)
      end
    end

    test "do not leave messages behind" do
      assert_emits_signal(Signals, :post_created, fn -> create_post("Signals") end)

      {:messages, messages} = Process.info(self(), :messages)
      assert Enum.filter(messages, &signal_message?/1) == []
    end

    test "the retroactive helpers fail without capture_signals" do
      assert_raise ExUnit.AssertionError, ~r/capture_signals/, fn ->
        refute_signal_emitted(Signals, :post_created)
      end

      assert_raise ExUnit.AssertionError, ~r/capture_signals/, fn ->
        assert_signal_emitted(Signals, :post_created)
      end
    end
  end

  describe "Ash.Signals.Test" do
    setup :capture_signals

    test "assert_signal_emitted matches emitted signals and returns them" do
      assert {:ok, %{id: post_id}} = create_post("Signals", actor: %{name: "zach"})

      assert %Signals.PostCreated{title: "Signals"} =
               assert_signal_emitted(Signals, :post_created, %{post_id: ^post_id})

      refute_signal_emitted(Signals, :post_created)
    end

    test "patterns can bind variables" do
      assert {:ok, post} = create_post("Bound")

      assert_signal_emitted(Signals, :post_created, %{post_id: post_id, title: title})
      assert post_id == post.id
      assert title == "Bound"
    end

    test "messages include who emitted the signal" do
      assert {:ok, _post} = create_post("Signals", actor: %{name: "zach"})

      assert_received {:ash_signal_emitted, Signals, :post_created, %Signals.PostCreated{},
                       %{resource: Post, action: :create, actor: %{name: "zach"}}}
    end

    test "refute_signal_emitted fails when a matching signal was emitted" do
      assert {:ok, _post} = create_post("Signals")

      refute_signal_emitted(Signals, :post_created, %{title: "Other"})

      assert_raise ExUnit.AssertionError, ~r/not to be emitted/, fn ->
        refute_signal_emitted(Signals, :post_created, %{title: "Signals"})
      end
    end

    test "assert_signal_emitted fails when no matching signal was emitted" do
      refute_signal_emitted(Signals, :post_created)

      assert_raise ExUnit.AssertionError,
                   ~r/Expected signal :post_created .* to be emitted/,
                   fn ->
                     assert_signal_emitted(Signals, :post_created)
                   end
    end

    test "signals emitted from processes started by the test are received" do
      Task.async(fn -> create_post("From a task") end) |> Task.await()

      assert_signal_emitted(Signals, :post_created, %{title: "From a task"})
    end

    test "unrelated processes are not sent anything" do
      test_pid = self()

      spawn(fn ->
        {:ok, _} = create_post("Elsewhere")
        {:messages, messages} = Process.info(self(), :messages)
        send(test_pid, {:emitter_messages, Enum.filter(messages, &signal_message?/1)})
      end)

      assert_receive {:emitter_messages, []}
      refute_signal_emitted(Signals, :post_created)
    end
  end

  describe "emit_many" do
    test "a bulk action emits a signal for each record" do
      result =
        Ash.bulk_create!(
          [%{title: "one"}, %{title: "two"}, %{title: "three"}],
          Post,
          :create_in_batches,
          return_records?: true,
          actor: %{name: "zach"}
        )

      assert length(Ash.read!(FeedItem)) == 3

      assert Enum.sort(comment_bodies()) == [
               "Welcome to one, from zach",
               "Welcome to three, from zach",
               "Welcome to two, from zach"
             ]

      assert Enum.map(result.records, & &1.id) |> Enum.sort() ==
               Ash.read!(FeedItem) |> Enum.map(& &1.post_id) |> Enum.sort()
    end

    test "a single action can emit from batch callbacks too" do
      assert_emits_signal(Signals, :post_created, %{title: "single"}, fn ->
        Post |> Ash.Changeset.for_create(:create_in_batches, %{title: "single"}) |> Ash.create!()
      end)
    end

    test "each listener handles every signal before the next listener runs" do
      Ash.bulk_create!([%{title: "one"}, %{title: "two"}], Post, :create_in_batches,
        sorted?: true
      )

      {:messages, messages} = Process.info(self(), :messages)

      assert for({:listened, listener, title} <- messages, do: {listener, title}) == [
               welcome: "one",
               welcome: "two",
               add: "one",
               add: "two"
             ]
    end

    test "an invalid payload emits nothing" do
      post_id = Ash.UUID.generate()

      assert {:error, _} =
               refute_emits_signal(Signals, :post_created, fn ->
                 Post
                 |> Ash.ActionInput.for_action(:announce, %{})
                 |> Ash.ActionInput.after_action(fn input, _result ->
                   Ash.Signals.emit_many(input, Signals, :post_created, [
                     %{post_id: post_id},
                     %{title: "missing post_id"}
                   ])
                 end)
                 |> Ash.run_action()
               end)

      assert Ash.read!(FeedItem) == []
    end
  end

  describe "emit in actions" do
    test "adds the signal module to signals_out" do
      assert ArticleSignals in Ash.Resource.Info.signals_out(Article)
    end

    test "keeps the description, on the emit and on the change that emits the signal" do
      action = Ash.Resource.Info.action(Article, :publish)

      assert [%{description: "Whenever an article is published."}] = action.emits

      assert %{description: "Whenever an article is published."} =
               Enum.find(action.changes, &match?({Ash.Resource.Change.EmitSignal, _}, &1.change))
    end

    test "fills signal fields from the record, fields and values" do
      article =
        assert_emits_signal(ArticleSignals, :article_published, %{title: "Hi", by: "zach"}, fn ->
          Ash.create!(Article, %{title: "Hi"}, actor: %{name: "zach"})
        end)

      assert_emits_signal(ArticleSignals, :article_published, %{article_id: article_id}, fn ->
        Ash.create!(Article, %{title: "Again"})
      end)

      assert is_binary(article_id)
      assert article.id != article_id
    end

    test "previous/1 takes fields from the record before an update" do
      article = Ash.create!(Article, %{title: "Old"})
      article_id = article.id

      assert_emits_signal(
        ArticleSignals,
        :article_published,
        %{article_id: ^article_id, title: "New", previous_title: "Old"},
        fn -> article |> Ash.Changeset.for_update(:retitle, %{title: "New"}) |> Ash.update!() end
      )
    end

    test "bulk actions emit once per record" do
      capture_signals()

      Ash.bulk_create!([%{title: "a"}, %{title: "b"}], Article, :publish)

      assert_signal_emitted(ArticleSignals, :article_published, %{title: "a"})
      assert_signal_emitted(ArticleSignals, :article_published, %{title: "b"})
      refute_signal_emitted(ArticleSignals, :article_published)
    end

    test "atomic bulk updates emit once per record" do
      Ash.bulk_create!([%{title: "a"}, %{title: "b"}], Article, :publish)
      capture_signals()

      Ash.bulk_update!(Article, :rename, %{title: "c"}, strategy: :atomic)

      assert_signal_emitted(ArticleSignals, :article_published, %{title: "c"})
      assert_signal_emitted(ArticleSignals, :article_published, %{title: "c"})
      refute_signal_emitted(ArticleSignals, :article_published)
    end

    test "previous/1 makes the change non-atomic" do
      Ash.bulk_create!([%{title: "a"}], Article, :publish)

      assert %Ash.BulkResult{status: :error} =
               Ash.bulk_update(Article, :retitle, %{title: "c"}, strategy: :atomic)

      assert_emits_signal(
        ArticleSignals,
        :article_published,
        %{title: "c", previous_title: "a"},
        fn -> Ash.bulk_update!(Article, :retitle, %{title: "c"}, strategy: :stream) end
      )
    end

    test "before_action signals read the changeset" do
      assert_emits_signal(ArticleSignals, :article_drafting, %{title: "Draft"}, fn ->
        Article |> Ash.Changeset.for_create(:draft, %{title: "Draft"}) |> Ash.create!()
      end)
    end

    test "after_transaction signals are emitted after the transaction" do
      article =
        assert_emits_signal(ArticleSignals, :article_committed, %{article_id: article_id}, fn ->
          Article |> Ash.Changeset.for_create(:commit, %{title: "Done"}) |> Ash.create!()
        end)

      assert article.id == article_id
    end

    test "bulk actions emit after_transaction signals for every record" do
      capture_signals()

      Ash.bulk_create!([%{title: "a"}, %{title: "b"}], Article, :commit)

      assert_signal_emitted(ArticleSignals, :article_committed)
      assert_signal_emitted(ArticleSignals, :article_committed)
      refute_signal_emitted(ArticleSignals, :article_committed)
    end

    test "bulk destroys emit after_transaction signals for every record" do
      Ash.bulk_create!([%{title: "a"}, %{title: "b"}], Article, :publish)
      capture_signals()

      Ash.bulk_destroy!(Article, :retract, %{}, strategy: :stream)

      assert_signal_emitted(ArticleSignals, :article_committed)
      assert_signal_emitted(ArticleSignals, :article_committed)
      refute_signal_emitted(ArticleSignals, :article_committed)
    end

    # atomic actions don't run the hooks that emit after the transaction
    test "after_transaction signals make the change non-atomic" do
      Ash.bulk_create!([%{title: "a"}, %{title: "b"}], Article, :publish)

      assert %Ash.BulkResult{status: :error} =
               Ash.bulk_update(Article, :commit_update, %{title: "c"}, strategy: :atomic)

      capture_signals()
      Ash.bulk_update!(Article, :commit_update, %{title: "c"}, strategy: [:atomic, :stream])

      assert_signal_emitted(ArticleSignals, :article_committed)
      assert_signal_emitted(ArticleSignals, :article_committed)
      refute_signal_emitted(ArticleSignals, :article_committed)
    end

    test "generic actions emit with prepare, from their arguments" do
      assert_emits_signal(ArticleSignals, :digest_requested, %{topic: "elixir"}, fn ->
        Article
        |> Ash.ActionInput.for_action(:request_digest, %{topic: "elixir"})
        |> Ash.run_action!()
      end)
    end
  end

  describe "consolidation" do
    test "compiles a dispatch module that emit uses instead of searching" do
      tmp_dir = Path.join(System.tmp_dir!(), "ash_signals_#{System.unique_integer([:positive])}")

      Code.compile_string("""
      defmodule Ash.Test.SignalsTest.ConsolidatedSignals do
        use Ash.Signals

        signals do
          signal :happened do
            phase :after_action
          end
        end
      end

      for name <- [ConsolidatedA, ConsolidatedB] do
        defmodule Module.concat(Ash.Test.SignalsTest, name) do
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          signals_in do
            from Ash.Test.SignalsTest.ConsolidatedSignals do
              on :happened, :heard
            end
          end

          actions do
            action :heard do
              argument :signal, Ash.Test.SignalsTest.ConsolidatedSignals.Happened
              run fn _, _ -> :ok end
            end
          end
        end
      end
      """)

      signal_module = Ash.Test.SignalsTest.ConsolidatedSignals
      dispatch = Ash.Signals.Registry.dispatch_module(signal_module)

      on_exit(fn ->
        :code.purge(dispatch)
        :code.delete(dispatch)
        File.rm_rf!(tmp_dir)
      end)

      assert [^dispatch] = Mix.Tasks.Compile.AshSignals.consolidate([signal_module], tmp_dir)
      assert File.exists?(Path.join(tmp_dir, "#{dispatch}.beam"))

      assert [
               %{resource: Ash.Test.SignalsTest.ConsolidatedA, action: :heard},
               %{resource: Ash.Test.SignalsTest.ConsolidatedB, action: :heard}
             ] = dispatch.listeners(:happened)

      assert dispatch.listeners(:unknown) == []

      # listeners defined after consolidation are not found
      Code.compile_string("""
      defmodule Ash.Test.SignalsTest.ConsolidatedC do
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        signals_in do
          from Ash.Test.SignalsTest.ConsolidatedSignals do
            on :happened, :heard
          end
        end

        actions do
          action :heard do
            argument :signal, Ash.Test.SignalsTest.ConsolidatedSignals.Happened
            run fn _, _ -> :ok end
          end
        end
      end
      """)

      assert length(Ash.Signals.Registry.listeners(signal_module, :happened)) == 2
      assert length(Ash.Signals.Registry.discover(signal_module)) == 3
    end
  end

  describe "verification" do
    # Verification happens outside the test process, so verifier errors are emitted as warnings.
    # The warnings are read from this process's diagnostics, since stderr also carries whatever
    # concurrently running tests write to it. Verifier warnings are still printed, so stderr is
    # captured only to keep them out of the test output.
    defp compile_warnings(code) do
      {warnings, _stderr} =
        ExUnit.CaptureIO.with_io(:stderr, fn ->
          {_, diagnostics} = Code.with_diagnostics(fn -> Code.compile_string(code) end)
          Enum.map_join(diagnostics, "\n", & &1.message)
        end)

      warnings
    end

    defp compile_listener(signals_in, actions) do
      compile_warnings("""
      defmodule Ash.Test.SignalsTest.Bad#{System.unique_integer([:positive])} do
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :order_id, :uuid, public?: true
        end

        signals_in do
          #{signals_in}
        end

        actions do
          defaults [:read]
          #{actions}
        end
      end
      """)
    end

    defp compile_emitter(actions) do
      compile_warnings("""
      defmodule Ash.Test.SignalsTest.BadEmitter#{System.unique_integer([:positive])} do
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :title, :string, public?: true
        end

        actions do
          defaults [:read]
          #{actions}
        end
      end
      """)
    end

    test "emit checks the signal" do
      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :nope
             end
             """) =~ "has no signal named `:nope`"

      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.Nope, :article_published
             end
             """) =~ "is not an `Ash.Signals` module"
    end

    test "signals are emitted after the action by default" do
      assert Ash.Signals.Info.signal(ArticleSignals, :article_published).phase == :after_action
    end

    test "emit requires the signal's phase" do
      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :article_published, phase: :before_action, fields: [article_id: :id]
             end
             """) =~
               "so `emit` must be given `phase: :after_action`, got: :before_action"

      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :article_drafting
             end
             """) =~
               "so `emit` must be given `phase: :before_action`, got: :after_action"
    end

    test "emit checks fields" do
      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :article_published, fields: [article_id: :id, nope: :title]
             end
             """) =~ "`fields` sets `:nope`, but signal `:article_published` has no field"

      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :article_published, fields: [article_id: :nope]
             end
             """) =~ "`:nope` is not an attribute of the resource"
    end

    test "a dynamic emit is built by your own code, so it can't set fields" do
      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :article_published, dynamic?: true
             end
             """) == ""

      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :article_published,
                 dynamic?: true,
                 fields: [article_id: :id]
             end
             """) =~ "`fields` can't be used with `dynamic?: true`"
    end

    test "emit requires every required signal field" do
      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :article_published
             end
             """) =~ "requires `:article_id`, which is not an attribute"

      assert compile_emitter("""
             action :request do
               emit Ash.Test.SignalsTest.ArticleSignals, :digest_requested
               run fn _, _ -> :ok end
             end
             """) =~ "requires `:topic`, which is not an argument of the action"
    end

    test "previous is only for update and destroy" do
      assert compile_emitter("""
             create :publish do
               emit Ash.Test.SignalsTest.ArticleSignals, :article_published, fields: [article_id: :id], values: [previous_title: previous(:title)]
             end
             """) =~ "`previous/1` is only for update and destroy actions"
    end

    test "batch? is only for generic actions" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :record, batch?: true end",
               "create :record, accept: [:order_id]"
             ) =~ "`batch?` is only for generic actions"
    end

    test "a batch listener's argument must be an array" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :log, batch?: true end",
               """
               action :log do
                 argument :signals, Ash.Test.SignalsTest.VerifySignals.OrderPlaced
                 run fn _, _ -> :ok end
               end
               """
             ) =~ "must be an array"
    end

    test "inputs must be inputs of the action, and fields of the signal" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :record, inputs: [nope: :order_id] end",
               "create :record, accept: [:order_id]"
             ) =~ "`:nope` is not an input of `:record`"

      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :record, inputs: [order_id: :nope] end",
               "create :record, accept: [:order_id]"
             ) =~ "`inputs` uses `:nope`, but signal `:order_placed` has no field with that name"
    end

    test "update and destroy options are checked" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :touch, get_by: [nope: :order_id] end",
               "update :touch"
             ) =~ "`get_by` field `:nope` is not an attribute"

      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :touch, read_action: :touch end",
               "update :touch"
             ) =~ "`:touch` is not a read action"

      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :touch, args: [:nope] end",
               "update :touch"
             ) =~ "`:nope` is not an argument of the `:read` read action"
    end

    test "a listener's delivery must implement Ash.Signals.Delivery" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.DeliverySignals do on :verified, :record, delivery: Ash.Test.SignalsTest.Nope end",
               "create :record, accept: [:order_id]"
             ) =~ "`Ash.Test.SignalsTest.Nope` does not implement `Ash.Signals.Delivery`"
    end

    test "a listener's delivery can verify it" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.DeliverySignals do on :verified, :rejected, delivery: Ash.Test.SignalsTest.RecordingDelivery end",
               "create :rejected, accept: [:order_id]"
             ) =~ "the delivery rejects this listener"
    end

    test "listener names are unique within a resource" do
      assert compile_listener(
               """
               from Ash.Test.SignalsTest.VerifySignals do
                 on :order_placed, :record
               end

               from Ash.Test.SignalsTest.DeliverySignals do
                 on :verified, :record
               end
               """,
               "create :record, accept: [:order_id]"
             ) =~ "There is already a listener named `:record`"
    end

    test "the action defaults to the listener's name" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :nope end",
               "create :record, accept: [:order_id]"
             ) =~
               "No action named `:nope`. The action defaults to the listener's name: set `action` to run a different one."
    end

    test "options for other action types are rejected" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :record, get_by: [:order_id] end",
               "create :record, accept: [:order_id]"
             ) =~ "`get_by` is only for update and destroy actions"

      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :order_placed, :log, inputs: [:order_id] end",
               """
               action :log do
                 argument :signal, Ash.Test.SignalsTest.VerifySignals.OrderPlaced
                 run fn _, _ -> :ok end
               end
               """
             ) =~ "`inputs` is only for create, update and destroy actions"
    end
  end

  describe "create, update and destroy listeners" do
    defp place_orders(orders) do
      Post
      |> Ash.ActionInput.for_action(:announce, %{})
      |> Ash.ActionInput.after_action(fn input, result ->
        with :ok <- Ash.Signals.emit_many(input, OrderSignals, :order_placed, orders) do
          {:ok, result}
        end
      end)
      |> Ash.run_action()
    end

    setup do
      [active, other, banned, untouched] =
        for status <- ["new", "new", "banned", "new"] do
          Ash.create!(Customer, %{status: status})
        end

      for customer <- [active, other, untouched] do
        Ash.create!(CartItem, %{customer_id: customer.id})
      end

      %{active: active, other: other, banned: banned, untouched: untouched}
    end

    test "run in bulk for every signal", customers do
      orders =
        for customer <- [customers.active, customers.other, customers.banned] do
          %{order_id: Ash.UUID.generate(), customer_id: customer.id, note: "thanks"}
        end

      order_ids = Enum.map(orders, & &1.order_id)

      assert {:ok, "announced"} = place_orders(orders)

      # create, from the signal fields named like its inputs
      assert Receipt |> Ash.read!() |> Enum.map(&{&1.order_id, &1.note}) |> Enum.sort() ==
               Enum.sort(Enum.map(order_ids, &{&1, "thanks"}))

      # create, with explicit inputs
      assert Reference |> Ash.read!() |> Enum.map(& &1.reference) |> Enum.sort() ==
               Enum.sort(order_ids)

      # update, with a read action and get_by: the banned customer is filtered out
      statuses = Customer |> Ash.read!() |> Map.new(&{&1.id, &1.status})
      assert statuses[customers.active.id] == "ordered"
      assert statuses[customers.other.id] == "ordered"
      assert statuses[customers.banned.id] == "banned"
      assert statuses[customers.untouched.id] == "new"

      # destroy, with a read action and its arguments
      assert [%{customer_id: untouched_id}] = Ash.read!(CartItem)
      assert untouched_id == customers.untouched.id

      # the same action, run by two listeners with different inputs
      customer_ids = Enum.map(orders, & &1.customer_id)

      assert LedgerEntry |> Ash.read!() |> Enum.map(& &1.account_id) |> Enum.sort() ==
               Enum.sort(order_ids ++ customer_ids)

      # a batched generic action, called once with every signal
      assert_received {:audited, ^order_ids}
      refute_received {:audited, _}
    end

    test "signals that match no records are handled", customers do
      assert {:ok, _} =
               place_orders([
                 %{order_id: Ash.UUID.generate(), customer_id: customers.banned.id}
               ])

      assert length(Ash.read!(CartItem)) == 3
    end

    test "a failing bulk listener fails the emit", customers do
      assert {:error, error} =
               place_orders([
                 %{order_id: Ash.UUID.generate(), customer_id: customers.active.id, note: "fail"}
               ])

      assert Exception.message(error) =~ "must not equal"
    end
  end

  defp announce(signal_module, name, payloads, opts \\ []) do
    Post
    |> Ash.ActionInput.for_action(:announce, %{}, opts)
    |> Ash.ActionInput.after_action(fn input, result ->
      with :ok <- Ash.Signals.emit_many(input, signal_module, name, payloads) do
        {:ok, result}
      end
    end)
    |> Ash.run_action()
  end

  describe "signal_id" do
    test "is generated for each signal when it is emitted" do
      orders =
        for _ <- 1..2 do
          %{order_id: Ash.UUID.generate(), customer_id: Ash.UUID.generate()}
        end

      {:ok, _} = announce(OrderSignals, :order_placed, orders)

      ids =
        SignalLog
        |> Ash.read!()
        |> Enum.map(& &1.source_signal_id)
        |> Enum.reject(&is_nil/1)
        |> Enum.uniq()

      assert length(ids) == 2
      assert Enum.all?(ids, &match?(<<_::binary-size(14), "7", _::binary>>, &1))
    end

    test "can be given in the payload" do
      signal_id = Ash.UUIDv7.generate()

      assert_emits_signal(OrderSignals, :order_placed, %{signal_id: ^signal_id}, fn ->
        {:ok, _} =
          announce(OrderSignals, :order_placed, [
            %{
              signal_id: signal_id,
              order_id: Ash.UUID.generate(),
              customer_id: Ash.UUID.generate()
            }
          ])
      end)
    end

    test "is only passed to listeners when mapped explicitly" do
      order_id = Ash.UUID.generate()

      {:ok, _} =
        announce(OrderSignals, :order_placed, [
          %{order_id: order_id, customer_id: Ash.UUID.generate()}
        ])

      assert [unmapped] = Enum.filter(Ash.read!(SignalLog), &is_nil(&1.source_signal_id))
      assert [mapped] = Enum.reject(Ash.read!(SignalLog), &is_nil(&1.source_signal_id))
      assert unmapped.order_id == order_id
      assert unmapped.signal_id == nil
      assert mapped.order_id == order_id
    end

    test "is reserved" do
      assert_raise Spark.Error.DslError, ~r/`signal_id` is reserved/, fn ->
        Code.compile_string("""
        defmodule Ash.Test.SignalsTest.ReservedSignals do
          use Ash.Signals

          signals do
            signal :happened do
              phase :after_action
              argument :signal_id, :uuid
            end
          end
        end
        """)
      end
    end
  end

  describe "signal_path" do
    @started %{
      signal_module: PathSignals,
      signal: :started,
      resource: PathRun,
      action: :start
    }

    @relayed %{
      signal_module: PathSignals,
      signal: :relayed,
      resource: PathRecord,
      action: :relay
    }

    defp start_run(input \\ %{}) do
      PathRun
      |> Ash.Changeset.for_create(:start, input, authorize?: true)
      |> Ash.create()
    end

    test "is in the context of listeners, starting with the signal they received" do
      assert {:ok, _run} = start_run()

      assert_received {:observed, [@started]}

      assert [[@started], [@started], [@relayed, @started]] =
               PathRecord
               |> Ash.read!(authorize?: false)
               |> Enum.map(& &1.signal_path)
               |> Enum.sort_by(&length/1)
    end

    test "is in the context of update and destroy listeners, and the reads that select their records" do
      target = Ash.create!(PathTarget, %{}, authorize?: false)
      assert {:ok, _run} = start_run(%{target_id: target.id})

      assert %{signal_path: [@started]} = Ash.get!(PathTarget, target.id, authorize?: false)
    end

    test "starts without an emitting action when reacting to a signal in a test" do
      assert :ok =
               react_to_signal(PathSignals, :started, %{run_id: Ash.UUID.generate()},
                 only: {PathRecord, :observe},
                 authorize?: true
               )

      assert_received {:observed,
                       [
                         %{
                           signal_module: PathSignals,
                           signal: :started,
                           resource: nil,
                           action: nil
                         }
                       ]}
    end

    test "can be stored and loaded again" do
      signal_path = [
        @started,
        @relayed,
        %{signal_module: PathSignals, signal: :started, resource: nil, action: nil}
      ]

      dumped = signal_path |> Ash.Signals.dump_signal_path() |> Jason.encode!() |> Jason.decode!()

      assert Ash.Signals.load_signal_path(dumped) == signal_path
    end

    test "signal_in only authorizes actions run by a listener of the signal" do
      assert {:error, %Ash.Error.Forbidden{}} =
               PathRecord
               |> Ash.Changeset.for_create(:record, %{run_id: Ash.UUID.generate()},
                 authorize?: true
               )
               |> Ash.create()
    end

    test "signal_in matches the signal the listener received, not the signals before it" do
      changeset =
        PathRecord
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_context(%{signal_path: [@relayed, @started]})

      authorizer = %{subject: changeset, changeset: changeset}

      assert Ash.Policy.Check.SignalIn.match?(nil, authorizer,
               signal_module: PathSignals,
               signal: :relayed
             )

      refute Ash.Policy.Check.SignalIn.match?(nil, authorizer,
               signal_module: PathSignals,
               signal: :started
             )
    end
  end

  describe "delivery" do
    test "receives the signals, the listeners that use it and the emitting context" do
      order_id = Ash.UUID.generate()

      assert {:ok, "announced"} =
               announce(DeliverySignals, :queued, [%{order_id: order_id}],
                 actor: %{name: "zach"},
                 context: %{shared: %{source: "test"}}
               )

      assert_received {:delivered, [%DeliverySignals.Queued{order_id: ^order_id}], listeners,
                       [queue: :orders], context}

      assert [%{resource: QueuedReceipt, name: :record}] = listeners

      assert %Ash.Signals.Delivery.Context{
               signal_module: DeliverySignals,
               signal: %{name: :queued},
               resource: Post,
               action: :announce,
               actor: %{name: "zach"},
               source_context: %{shared: %{source: "test"}},
               signal_path: [
                 %{
                   signal_module: DeliverySignals,
                   signal: :queued,
                   resource: Post,
                   action: :announce
                 }
               ]
             } = context

      assert %{shared: %{source: "test"}, signal_path: [%{signal: :queued}]} =
               Ash.Scope.to_opts(context)[:context]

      # only the inline listener ran
      assert [%{order_id: ^order_id}] = Ash.read!(QueuedReceipt)
    end

    test "can run listeners later, the way the inline delivery does" do
      order_id = Ash.UUID.generate()
      {:ok, _} = announce(DeliverySignals, :queued, [%{order_id: order_id}])
      assert_received {:delivered, signals, [listener], _opts, context}

      {:ok, dumped} = Ash.Signals.dump(DeliverySignals, :queued, signals)
      assert [%{"order_id" => ^order_id, "signal_id" => _}] = Jason.decode!(Jason.encode!(dumped))
      assert {:ok, ^signals} = Ash.Signals.load(DeliverySignals, :queued, dumped)

      assert %{resource: QueuedReceipt, action: :record, action_type: :create} =
               Ash.Resource.Info.signal_listener(QueuedReceipt, :record)

      assert :ok = Ash.Signals.run_listener(listener, signals, Ash.Scope.to_opts(context))
      assert [%{order_id: ^order_id}, %{order_id: ^order_id}] = Ash.read!(QueuedReceipt)
    end

    test "loading ignores unknown fields and refuses missing required ones" do
      signal_id = Ash.UUIDv7.generate()
      order_id = Ash.UUID.generate()

      assert {:ok, [%DeliverySignals.Queued{order_id: ^order_id}]} =
               Ash.Signals.load(DeliverySignals, :queued, [
                 %{"signal_id" => signal_id, "order_id" => order_id, "removed" => "value"}
               ])

      assert {:error, _} =
               Ash.Signals.load(DeliverySignals, :queued, [
                 %{"signal_id" => signal_id, "renamed_order_id" => order_id}
               ])
    end

    test "can fail the emit" do
      assert {:error, error} = announce(DeliverySignals, :failing, [%{}])
      assert Exception.message(error) =~ "delivery failed"

      assert_received {:delivered, [%DeliverySignals.Failing{}], [%{name: :record_failing}],
                       [fail?: true], _}
    end

    test "listeners are found by name" do
      assert %{action: :record, inputs: [account_id: :customer_id]} =
               Ash.Resource.Info.signal_listener(LedgerEntry, :record_for_customer)

      assert Ash.Resource.Info.signal_listener(LedgerEntry, :record) == nil
    end

    test "update and destroy listeners expose the queries they run" do
      [customer, banned] =
        for status <- ["new", "banned"], do: Ash.create!(Customer, %{status: status})

      listener =
        Ash.Resource.Info.signal_listener(Customer, :mark_ordered)

      signals =
        for customer <- [customer, banned] do
          OrderSignals.OrderPlaced.new!(%{
            signal_id: Ash.UUIDv7.generate(),
            order_id: Ash.UUID.generate(),
            customer_id: customer.id
          })
        end

      assert [{query, %{}, ^signals}] = Ash.Signals.listener_queries(listener, signals, [])
      assert [%{id: id}] = Ash.read!(query)
      assert id == customer.id
    end
  end
end
