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
    def verify_signal(%{name: :rejected}, _opts, _dsl_state),
      do: {:error, "the delivery rejects this signal"}

    def verify_signal(_signal, _opts, _dsl_state), do: :ok

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
        delivery({Ash.Test.SignalsTest.RecordingDelivery, queue: :orders})
        argument :order_id, :uuid, allow_nil?: false
      end

      signal :failing do
        phase(:after_action)
        delivery({Ash.Test.SignalsTest.RecordingDelivery, fail?: true})
      end

      # only for listeners that fail verification
      signal :verified do
        phase(:after_action)
        delivery(Ash.Test.SignalsTest.RecordingDelivery)
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
        on(:record, :queued)
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
        on(:record_for_order, :order_placed,
          action: :record,
          inputs: [account_id: :order_id]
        )

        on(:record_for_customer, :order_placed,
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
        on(:log_unmapped, :order_placed)
        on(:log_mapped, :order_placed, inputs: [:order_id, source_signal_id: :signal_id])
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
        on(:record, :order_placed)
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
        on(:record, :order_placed, inputs: [reference: :order_id])
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
        on(:mark_ordered, :order_placed, read_action: :active, get_by: [id: :customer_id])
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
        on(:clear, :order_placed, read_action: :for_customer, args: [:customer_id])
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
        on(:log, :order_placed, batch?: true)
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
      signal :article_published do
        phase(:after_action)
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

  # emits only with `emit_signal`, so it doesn't list `signals_out`
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

        change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_published, :after_action,
                 fields: [article_id: :id],
                 values: [by: actor(:name)]
               )
      end

      update :rename do
        accept [:title]

        change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_published, :after_action,
                 fields: [:title, article_id: :id]
               )
      end

      update :retitle do
        accept [:title]
        require_atomic? false

        change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_published, :after_action,
                 fields: [:title, article_id: :id],
                 values: [previous_title: previous(:title)]
               )
      end

      create :draft do
        accept [:title]
        change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_drafting, :before_action)
      end

      create :commit do
        accept [:title]

        change emit_signal(
                 Ash.Test.SignalsTest.ArticleSignals,
                 :article_committed,
                 :after_transaction,
                 fields: [article_id: :id]
               )
      end

      update :commit_update do
        accept [:title]
        require_atomic? false

        change emit_signal(
                 Ash.Test.SignalsTest.ArticleSignals,
                 :article_committed,
                 :after_transaction,
                 fields: [article_id: :id]
               )
      end

      destroy :retract do
        require_atomic? false

        change emit_signal(
                 Ash.Test.SignalsTest.ArticleSignals,
                 :article_committed,
                 :after_transaction,
                 fields: [article_id: :id]
               )
      end

      action :request_digest do
        argument :topic, :string, allow_nil?: false
        prepare emit_signal(Ash.Test.SignalsTest.ArticleSignals, :digest_requested, :after_action)
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
      data_layer: Ash.DataLayer.Ets,
      signals_out: [
        Ash.Test.SignalsTest.Signals,
        Ash.Test.SignalsTest.OrderSignals,
        Ash.Test.SignalsTest.DeliverySignals
      ]

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
        change EmitPostsCreated
      end

      create :emit_in_wrong_phase do
        accept [:title]

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
        run fn _input, _ -> {:ok, "announced"} end
      end

      update :archive do
        require_atomic? false

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
        on(:welcome, :post_created)
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
        on(:add, :post_created)
        on(:remove, :post_archiving)
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

  describe "emit" do
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

    test "raises when the resource does not list the signal module in signals_out" do
      assert_raise Ash.Error.Unknown, ~r/cannot emit signals from/, fn ->
        NotEmitter |> Ash.Changeset.for_create(:create, %{}) |> Ash.create()
      end
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

  describe "emit_signal" do
    test "adds the signal module to signals_out" do
      assert ArticleSignals in Ash.Resource.Info.signals_out(Article)
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
              on :heard, :happened
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
            on :heard, :happened
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
    defp compile_warnings(code) do
      ExUnit.CaptureIO.capture_io(:stderr, fn -> Code.compile_string(code) end)
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

    test "emit_signal checks the signal" do
      assert compile_emitter("""
             create :publish do
               change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :nope, :after_action)
             end
             """) =~ "has no signal named `:nope`"

      assert compile_emitter("""
             create :publish do
               change emit_signal(Ash.Test.SignalsTest.Nope, :article_published, :after_action)
             end
             """) =~ "is not an `Ash.Signals` module"
    end

    test "emit_signal requires the signal's phase" do
      assert compile_emitter("""
             create :publish do
               change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_published,
                        :before_action, fields: [article_id: :id])
             end
             """) =~
               "the phase given to `emit_signal` must be `:after_action`, got: :before_action"
    end

    test "emit_signal checks fields" do
      assert compile_emitter("""
             create :publish do
               change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_published, :after_action,
                        fields: [article_id: :id, nope: :title])
             end
             """) =~ "`fields` sets `:nope`, but signal `:article_published` has no field"

      assert compile_emitter("""
             create :publish do
               change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_published, :after_action,
                        fields: [article_id: :nope])
             end
             """) =~ "`:nope` is not an attribute, calculation or aggregate"
    end

    test "emit_signal requires every required signal field" do
      assert compile_emitter("""
             create :publish do
               change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_published, :after_action)
             end
             """) =~ "requires `:article_id`, which is not an attribute"

      assert compile_emitter("""
             action :request do
               prepare emit_signal(Ash.Test.SignalsTest.ArticleSignals, :digest_requested, :after_action)
               run fn _, _ -> :ok end
             end
             """) =~ "requires `:topic`, which is not an argument of the action"
    end

    test "previous is only for update and destroy" do
      assert compile_emitter("""
             create :publish do
               change emit_signal(Ash.Test.SignalsTest.ArticleSignals, :article_published, :after_action,
                        fields: [article_id: :id], values: [previous_title: previous(:title)])
             end
             """) =~ "`previous/1` is only for update and destroy actions"
    end

    test "batch? is only for generic actions" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :record, :order_placed, batch?: true end",
               "create :record, accept: [:order_id]"
             ) =~ "`batch?` is only for generic actions"
    end

    test "a batch listener's argument must be an array" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :log, :order_placed, batch?: true end",
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
               "from Ash.Test.SignalsTest.VerifySignals do on :record, :order_placed, inputs: [nope: :order_id] end",
               "create :record, accept: [:order_id]"
             ) =~ "`:nope` is not an input of `:record`"

      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :record, :order_placed, inputs: [order_id: :nope] end",
               "create :record, accept: [:order_id]"
             ) =~ "`inputs` uses `:nope`, but signal `:order_placed` has no field with that name"
    end

    test "update and destroy options are checked" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :touch, :order_placed, get_by: [nope: :order_id] end",
               "update :touch"
             ) =~ "`get_by` field `:nope` is not an attribute"

      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :touch, :order_placed, read_action: :touch end",
               "update :touch"
             ) =~ "`:touch` is not a read action"

      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :touch, :order_placed, args: [:nope] end",
               "update :touch"
             ) =~ "`:nope` is not an argument of the `:read` read action"
    end

    test "a signal's delivery must implement Ash.Signals.Delivery, and can verify it" do
      assert compile_warnings("""
             defmodule Ash.Test.SignalsTest.BadDelivery#{System.unique_integer([:positive])} do
               use Ash.Signals

               signals do
                 signal :happened do
                   phase :after_action
                   delivery Ash.Test.SignalsTest.Nope
                 end
               end
             end
             """) =~ "`Ash.Test.SignalsTest.Nope` does not implement `Ash.Signals.Delivery`"

      assert compile_warnings("""
             defmodule Ash.Test.SignalsTest.BadDelivery#{System.unique_integer([:positive])} do
               use Ash.Signals

               signals do
                 signal :rejected do
                   phase :after_action
                   delivery Ash.Test.SignalsTest.RecordingDelivery
                 end
               end
             end
             """) =~ "the delivery rejects this signal"
    end

    test "a signal's delivery can verify its listeners" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.DeliverySignals do on :rejected, :verified end",
               "create :rejected, accept: [:order_id]"
             ) =~ "the delivery rejects this listener"
    end

    test "listener names are unique within a resource" do
      assert compile_listener(
               """
               from Ash.Test.SignalsTest.VerifySignals do
                 on :record, :order_placed
               end

               from Ash.Test.SignalsTest.DeliverySignals do
                 on :record, :verified
               end
               """,
               "create :record, accept: [:order_id]"
             ) =~ "There is already a listener named `:record`"
    end

    test "the action defaults to the listener's name" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :nope, :order_placed end",
               "create :record, accept: [:order_id]"
             ) =~
               "No action named `:nope`. The action defaults to the listener's name: set `action` to run a different one."
    end

    test "options for other action types are rejected" do
      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :record, :order_placed, get_by: [:order_id] end",
               "create :record, accept: [:order_id]"
             ) =~ "`get_by` is only for update and destroy actions"

      assert compile_listener(
               "from Ash.Test.SignalsTest.VerifySignals do on :log, :order_placed, inputs: [:order_id] end",
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

  describe "delivery" do
    test "receives the signals, their listeners and the emitting context" do
      order_id = Ash.UUID.generate()

      assert {:ok, "announced"} =
               announce(DeliverySignals, :queued, [%{order_id: order_id}],
                 actor: %{name: "zach"},
                 context: %{shared: %{source: "test"}}
               )

      assert_received {:delivered, [%DeliverySignals.Queued{order_id: ^order_id}], listeners,
                       [queue: :orders], context}

      assert [%{resource: QueuedReceipt, action: :record}] = listeners

      assert %Ash.Signals.Delivery.Context{
               signal_module: DeliverySignals,
               signal: %{name: :queued},
               resource: Post,
               action: :announce,
               actor: %{name: "zach"},
               source_context: %{shared: %{source: "test"}}
             } = context

      # the listener did not run
      assert Ash.read!(QueuedReceipt) == []
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
      assert [%{order_id: ^order_id}] = Ash.read!(QueuedReceipt)
    end

    test "is called even when the signal has no listeners, and can fail the emit" do
      assert {:error, error} = announce(DeliverySignals, :failing, [%{}])
      assert Exception.message(error) =~ "delivery failed"
      assert_received {:delivered, [%DeliverySignals.Failing{}], [], [fail?: true], _}
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
