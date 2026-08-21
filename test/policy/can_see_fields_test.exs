# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.CanSeeFieldsTest do
  @doc false
  use ExUnit.Case

  alias Ash.Test.Support.PolicyField.{Ticket, User}

  defmodule PlainResource do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      defaults [:read]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public? true
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end
  end

  defmodule NoAuthorizerResource do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public? true
      end
    end
  end

  setup do
    rep =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :representative}),
        authorize?: false
      )

    user =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :user}),
        authorize?: false
      )

    admin =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :admin}),
        authorize?: false
      )

    [representative: rep, user: user, admin: admin]
  end

  describe "fields with simple/strict-checkable field policies" do
    test "returns true when the actor can see the field", %{representative: rep} do
      assert Ash.can_see_fields?(Ticket, rep, [:internal_status])
    end

    test "returns false when the actor cannot see the field", %{user: user} do
      refute Ash.can_see_fields?(Ticket, user, [:internal_status])
    end

    test "bypasses apply", %{admin: admin} do
      assert Ash.can_see_fields?(Ticket, admin, [:internal_status])
    end

    test "returns false if any requested field is not visible", %{user: user} do
      refute Ash.can_see_fields?(Ticket, user, [:name, :internal_status])
    end

    test "can_see_fields/4 returns per-field results", %{user: user} do
      assert {:ok, %{name: true, internal_status: false}} =
               Ash.can_see_fields(Ticket, user, [:name, :internal_status])
    end
  end

  describe "fields with filter check field policies" do
    test "count as visible by default", %{user: user} do
      # :status is visible via `relates_to_actor_via`, which depends on the record
      assert Ash.can_see_fields?(Ticket, user, [:status])
      assert {:ok, %{status: true}} = Ash.can_see_fields(Ticket, user, [:status])
    end

    test "count as not visible with filter_is: false", %{user: user} do
      refute Ash.can_see_fields?(Ticket, user, [:status], filter_is: false)

      assert {:ok, %{status: false}} =
               Ash.can_see_fields(Ticket, user, [:status], filter_is: false)
    end

    test "filter_is does not affect fields that strict check to true", %{admin: admin} do
      # admins bypass field policies entirely, no filter necessary
      assert Ash.can_see_fields?(Ticket, admin, [:status], filter_is: false)
    end

    test "expression policies referencing the actor produce filters", %{user: user} do
      # only you can see your own points: expr(id == ^actor(:id))
      assert Ash.can_see_fields?(User, user, [:points])
      refute Ash.can_see_fields?(User, user, [:points], filter_is: false)
    end

    test "the raw filter expression is available from Ash.Can", %{user: user} do
      assert {:ok, %{status: {:filter, _expr}, name: true}} =
               Ash.Can.evaluate_field_policies(Ticket, Ash.Resource.Info.domain(Ticket), user, [
                 :status,
                 :name
               ])
    end
  end

  describe "primary keys" do
    test "are always visible", %{user: user} do
      assert Ash.can_see_fields?(Ticket, user, [:id])
      assert Ash.can_see_fields?(Ticket, user, [:id], filter_is: false)
    end
  end

  describe "private fields" do
    test "are not visible when private_fields is :hide, even with a bypass", %{
      user: user,
      admin: admin
    } do
      refute Ash.can_see_fields?(Ticket, user, [:top_secret])
      refute Ash.can_see_fields?(Ticket, admin, [:top_secret])
    end

    test "are visible when private_fields is :show", %{user: user} do
      assert Ash.can_see_fields?(User, user, [:top_secret])
    end
  end

  describe "subject shapes" do
    test "accepts a resource, a resource/action tuple, and a query", %{
      representative: rep
    } do
      assert Ash.can_see_fields?(Ticket, rep, [:internal_status])
      assert Ash.can_see_fields?({Ticket, :read}, rep, [:internal_status])

      assert Ash.can_see_fields?(
               Ash.Query.for_read(Ticket, :read, %{}, actor: rep),
               rep,
               [:internal_status]
             )
    end

    test "accepts non-read actions", %{representative: rep, user: user} do
      assert Ash.can_see_fields?({Ticket, :create}, rep, [:internal_status])
      refute Ash.can_see_fields?({Ticket, :create}, user, [:internal_status])
    end

    test "accepts a single field instead of a list", %{user: user} do
      refute Ash.can_see_fields?(Ticket, user, :internal_status)
    end
  end

  describe "resources without field policies" do
    test "all fields are visible" do
      assert Ash.can_see_fields?(PlainResource, nil, [:name])
      assert {:ok, %{name: true}} = Ash.can_see_fields(PlainResource, nil, [:name])
    end

    test "all fields are visible without any authorizers" do
      assert Ash.can_see_fields?(NoAuthorizerResource, nil, [:name])
    end
  end

  describe "invalid input" do
    test "unknown fields raise an ArgumentError", %{user: user} do
      assert_raise ArgumentError, ~r/Invalid field\(s\)/, fn ->
        Ash.can_see_fields?(Ticket, user, [:not_a_field])
      end
    end

    test "relationships raise an ArgumentError", %{user: user} do
      assert_raise ArgumentError, ~r/do not apply to relationships/, fn ->
        Ash.can_see_fields?(Ticket, user, [:reporter])
      end
    end
  end
end
