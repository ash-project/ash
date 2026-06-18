# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.CreateFilterTest do
  @moduledoc false
  use ExUnit.Case, async: false

  defmodule Domain do
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Organization do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :create_filter_test_orgs
    end

    attributes do
      uuid_primary_key :id
      attribute :owner_id, :uuid, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end
  end

  defmodule Post do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [Ash.Policy.Authorizer]

    mnesia do
      table :create_filter_test_posts
    end

    attributes do
      uuid_primary_key :id
      attribute :text, :string, public?: true, allow_nil?: false
    end

    relationships do
      belongs_to :organization, Organization, public?: true, attribute_writable?: true
    end

    policies do
      policy action_type(:create) do
        authorize_if expr(organization.owner_id == ^actor(:id))
      end

      policy action_type(:read) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, update: :*, create: :*]
    end
  end

  defmodule CustomCheck do
    @moduledoc """
    Returns `:unknown` from `auto_filter/3`, forcing the framework to defer
    authorization to `check/4` against the inserted record post-insert.
    """
    use Ash.Policy.Check

    @impl true
    def describe(_), do: "custom check returning :unknown from auto_filter"

    @impl true
    def type, do: :filter

    @impl true
    def strict_check(_actor, _authorizer, _opts), do: {:ok, :unknown}

    @impl true
    def auto_filter(_actor, _authorizer, _opts), do: :unknown

    @impl true
    def check(actor, records, _authorizer, _opts) do
      send(self(), {:custom_check_invoked, length(records)})
      Enum.filter(records, fn record -> record.text == "ok-#{actor.id}" end)
    end
  end

  defmodule UnknownPost do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [Ash.Policy.Authorizer]

    mnesia do
      table :create_filter_test_unknown_posts
    end

    attributes do
      uuid_primary_key :id
      attribute :text, :string, public?: true, allow_nil?: false
    end

    policies do
      policy action_type(:create) do
        authorize_if {CustomCheck, []}
      end

      policy action_type(:read) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end
  end

  defmodule MixedPost do
    @moduledoc """
    Combines two `forbid_unless`-style policies on create. One is a filter
    referencing a relationship (the framework's auto-derived `check/4`
    queries with the filter), the other is a custom check returning
    `:unknown` from `auto_filter/3` (forces the framework to call its
    `check/4`). Both must authorize the inserted record for the create to
    succeed.
    """
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [Ash.Policy.Authorizer]

    mnesia do
      table :create_filter_test_mixed_posts
    end

    attributes do
      uuid_primary_key :id
      attribute :text, :string, public?: true, allow_nil?: false
    end

    relationships do
      belongs_to :organization, Organization,
        public?: true,
        attribute_writable?: true
    end

    policies do
      policy action_type(:create) do
        forbid_unless expr(organization.owner_id == ^actor(:id))
        authorize_if {CustomCheck, []}
      end

      policy action_type(:read) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end
  end

  defmodule BeforeTxnPost do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [Ash.Policy.Authorizer]

    mnesia do
      table :create_filter_test_before_txn_posts
    end

    attributes do
      uuid_primary_key :id
      attribute :text, :string, public?: true, allow_nil?: false
    end

    relationships do
      belongs_to :organization, Organization,
        public?: true,
        attribute_writable?: true,
        allow_nil?: false
    end

    policies do
      policy action_type(:create) do
        authorize_if expr(organization.owner_id == ^actor(:id))
      end

      policy action_type(:read) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read]

      create :create_without_opt_in do
        accept [:text, :organization_id]

        change fn changeset, _ ->
          Ash.Changeset.before_transaction(changeset, fn cs ->
            send(self(), :before_txn_fired)
            cs
          end)
        end
      end

      create :create_with_opt_in do
        accept [:text, :organization_id]
        allow_post_action_authorization? true

        change fn changeset, _ ->
          Ash.Changeset.before_transaction(changeset, fn cs ->
            send(self(), :before_txn_fired)
            cs
          end)
        end
      end
    end
  end

  defmodule EtsOrganization do
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :owner_id, :uuid, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end
  end

  defmodule EtsPost do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :text, :string, public?: true, allow_nil?: false
    end

    relationships do
      belongs_to :organization, EtsOrganization,
        public?: true,
        attribute_writable?: true
    end

    policies do
      policy action_type(:create) do
        authorize_if expr(organization.owner_id == ^actor(:id))
      end

      policy action_type(:read) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end
  end

  setup do
    import ExUnit.CaptureLog

    capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Domain, [
        Organization,
        Post,
        UnknownPost,
        MixedPost,
        BeforeTxnPost
      ])
    end)

    on_exit(fn ->
      capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)

    :ok
  end

  describe "relationship-referencing filter policy on a create action" do
    test "authorizes when the inserted record matches the filter (post-insert auth)" do
      owner_id = Ash.UUID.generate()

      org =
        Organization
        |> Ash.Changeset.for_create(:create, %{owner_id: owner_id})
        |> Ash.create!()

      assert {:ok, %Post{}} =
               Post
               |> Ash.Changeset.for_create(:create, %{text: "hi", organization_id: org.id})
               |> Ash.create(actor: %{id: owner_id})
    end

    test "forbids and rolls back when the inserted record does not match the filter" do
      owner_id = Ash.UUID.generate()
      other_actor_id = Ash.UUID.generate()

      org =
        Organization
        |> Ash.Changeset.for_create(:create, %{owner_id: owner_id})
        |> Ash.create!()

      assert {:error, %Ash.Error.Forbidden{}} =
               Post
               |> Ash.Changeset.for_create(:create, %{text: "hi", organization_id: org.id})
               |> Ash.create(actor: %{id: other_actor_id})

      assert {:ok, []} = Ash.read(Post, authorize?: false)
    end
  end

  describe "auto_filter returning :unknown defers to check/4 post-insert" do
    test "authorizes when check/4 returns the inserted record" do
      actor_id = Ash.UUID.generate()

      assert {:ok, %UnknownPost{}} =
               UnknownPost
               |> Ash.Changeset.for_create(:create, %{text: "ok-#{actor_id}"})
               |> Ash.create(actor: %{id: actor_id})

      assert_received {:custom_check_invoked, 1}
    end

    test "forbids and rolls back when check/4 returns no records" do
      actor_id = Ash.UUID.generate()

      assert {:error, %Ash.Error.Forbidden{}} =
               UnknownPost
               |> Ash.Changeset.for_create(:create, %{text: "nope"})
               |> Ash.create(actor: %{id: actor_id})

      assert_received {:custom_check_invoked, 1}
      assert {:ok, []} = Ash.read(UnknownPost, authorize?: false)
    end
  end

  describe "policy combining a relationship filter and a check/4-deferred check" do
    test "authorizes when both the filter matches and check/4 returns the record" do
      actor_id = Ash.UUID.generate()

      org =
        Organization
        |> Ash.Changeset.for_create(:create, %{owner_id: actor_id})
        |> Ash.create!()

      assert {:ok, %MixedPost{}} =
               MixedPost
               |> Ash.Changeset.for_create(:create, %{
                 text: "ok-#{actor_id}",
                 organization_id: org.id
               })
               |> Ash.create(actor: %{id: actor_id})

      assert_received {:custom_check_invoked, 1}
    end

    test "forbids when the filter matches but check/4 rejects the record" do
      actor_id = Ash.UUID.generate()

      org =
        Organization
        |> Ash.Changeset.for_create(:create, %{owner_id: actor_id})
        |> Ash.create!()

      assert {:error, %Ash.Error.Forbidden{}} =
               MixedPost
               |> Ash.Changeset.for_create(:create, %{
                 text: "wrong-text",
                 organization_id: org.id
               })
               |> Ash.create(actor: %{id: actor_id})

      assert {:ok, []} = Ash.read(MixedPost, authorize?: false)
    end

    test "forbids when check/4 accepts but the filter rejects the record" do
      actor_id = Ash.UUID.generate()
      other_owner_id = Ash.UUID.generate()

      org =
        Organization
        |> Ash.Changeset.for_create(:create, %{owner_id: other_owner_id})
        |> Ash.create!()

      assert {:error, %Ash.Error.Forbidden{}} =
               MixedPost
               |> Ash.Changeset.for_create(:create, %{
                 text: "ok-#{actor_id}",
                 organization_id: org.id
               })
               |> Ash.create(actor: %{id: actor_id})

      assert {:ok, []} = Ash.read(MixedPost, authorize?: false)
    end
  end

  describe "before_transaction hooks require explicit opt-in" do
    test "without `allow_post_action_authorization?`, raises CannotFilterCreates" do
      owner_id = Ash.UUID.generate()

      org =
        Organization
        |> Ash.Changeset.for_create(:create, %{owner_id: owner_id})
        |> Ash.create!()

      raised =
        try do
          BeforeTxnPost
          |> Ash.Changeset.for_create(:create_without_opt_in, %{
            text: "hi",
            organization_id: org.id
          })
          |> Ash.create!(actor: %{id: owner_id})

          nil
        rescue
          e -> e
        end

      assert %Ash.Error.Forbidden{errors: errors} = raised

      assert Enum.any?(errors, &match?(%Ash.Error.Forbidden.CannotFilterCreates{}, &1))
      assert Exception.message(raised) =~ "before_transaction"

      # The before_transaction hook did NOT fire, because we caught the
      # config issue at pre-flight install time.
      refute_received :before_txn_fired
    end

    test "with `allow_post_action_authorization?`, the create proceeds and rolls back" do
      owner_id = Ash.UUID.generate()
      other_actor = Ash.UUID.generate()

      org =
        Organization
        |> Ash.Changeset.for_create(:create, %{owner_id: owner_id})
        |> Ash.create!()

      # Happy path: matching actor — before_transaction fires, create succeeds.
      assert {:ok, %BeforeTxnPost{}} =
               BeforeTxnPost
               |> Ash.Changeset.for_create(:create_with_opt_in, %{
                 text: "hi",
                 organization_id: org.id
               })
               |> Ash.create(actor: %{id: owner_id})

      assert_received :before_txn_fired

      # Rejected path: non-matching actor — before_transaction STILL fires (user opted
      # in to this trade-off), the insert is rolled back, the action returns forbidden.
      assert {:error, %Ash.Error.Forbidden{}} =
               BeforeTxnPost
               |> Ash.Changeset.for_create(:create_with_opt_in, %{
                 text: "rejected",
                 organization_id: org.id
               })
               |> Ash.create(actor: %{id: other_actor})

      assert_received :before_txn_fired

      assert [%BeforeTxnPost{text: "hi"}] = Ash.read!(BeforeTxnPost, authorize?: false)
    end
  end

  describe "bulk_create with transaction: false is rejected at runtime" do
    test "raises rather than allowing un-rollback-able creates" do
      owner_id = Ash.UUID.generate()
      other_actor = Ash.UUID.generate()

      org =
        Organization
        |> Ash.Changeset.for_create(:create, %{owner_id: owner_id})
        |> Ash.create!()

      # The runtime in_transaction? guard inside the post-insert hook fires
      # when bulk_create opts out of its transaction wrapping. The hook
      # cannot safely roll back without a transaction, so it raises rather
      # than silently authorizing.
      raised =
        try do
          Ash.bulk_create(
            [%{text: "rejected", organization_id: org.id}],
            Post,
            :create,
            actor: %{id: other_actor},
            transaction: false,
            return_errors?: true,
            stop_on_error?: true
          )

          nil
        rescue
          e -> e
        end

      assert %Ash.Error.Forbidden{errors: errors} = raised

      assert Enum.any?(errors, &match?(%Ash.Error.Forbidden.CannotFilterCreates{}, &1))
    end
  end

  describe "non-transactional data layer" do
    test "raises CannotFilterCreates with explanation rather than silently skipping rollback" do
      raised =
        try do
          EtsPost
          |> Ash.Changeset.for_create(:create, %{text: "hi"})
          |> Ash.create!(actor: %{id: Ash.UUID.generate()})

          nil
        rescue
          e -> e
        end

      assert %Ash.Error.Forbidden{errors: errors} = raised

      assert Enum.any?(errors, fn
               %Ash.Error.Forbidden.CannotFilterCreates{} -> true
               _ -> false
             end)

      assert Exception.message(raised) =~ "does not\nsupport transactions"
    end
  end
end
