defmodule Ash.Test.Policy.RelatesToActorViaTest do
  @doc false
  use ExUnit.Case

  alias Ash.Test.Domain, as: Domain

  defmodule Actor do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    calculations do
      calculate :type,
                :atom,
                expr(
                  if is_nil(role_id) do
                    :user
                  else
                    :role
                  end
                ) do
        public?(true)
      end
    end

    relationships do
      belongs_to :user, Ash.Test.Policy.RelatesToActorViaTest.User, public?: true
      belongs_to :role, Ash.Test.Policy.RelatesToActorViaTest.Role, public?: true
    end
  end

  defmodule User do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule Account do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    policies do
      policy actor_attribute_equals(:type, :user) do
        authorize_if relates_to_actor_via(:user, field: :user)
      end

      policy actor_attribute_equals(:type, :role) do
        authorize_if relates_to_actor_via(:roles, field: :role)
      end
    end

    relationships do
      belongs_to :user, Ash.Test.Policy.RelatesToActorViaTest.User do
        public?(true)
      end

      has_many :roles, Ash.Test.Policy.RelatesToActorViaTest.Role do
        public?(true)
      end
    end
  end

  defmodule Role do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :account, Ash.Test.Policy.RelatesToActorViaTest.Account do
        public?(true)
      end
    end
  end

  defmodule BadPolicyRelName do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    policies do
      policy always() do
        authorize_if relates_to_actor_via(:does_not_exist)
      end
    end
  end

  defmodule BadPolicyRelPathName do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    policies do
      policy always() do
        authorize_if relates_to_actor_via([:does_not_exist, :neither_does_this])
      end
    end
  end

  describe "actor_at option" do
    test "relates_to_actor_via with belongs_to" do
      user =
        User
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!()

      account =
        Account
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:user, user, type: :append)
        |> Ash.create!(authorize?: false)

      actor =
        Actor
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:user, user, type: :append)
        |> Ash.create!(authorize?: false)
        |> Ash.load!(:type)

      assert {:ok, _} = Ash.get(Account, account.id, actor: actor)
    end

    test "relates_to_actor_via with has_many" do
      user =
        User
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!()

      account =
        Account
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:user, user, type: :append)
        |> Ash.create!(authorize?: false)

      role =
        Role
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:account, account, type: :append)
        |> Ash.create!(authorize?: false)

      actor =
        Actor
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:role, role, type: :append)
        |> Ash.create!(authorize?: false)
        |> Ash.load!(:type)

      assert {:ok, _} = Ash.get(Account, account.id, actor: actor)
    end

    test "relates_to_actor_via raises if relationship does not exist" do
      assert_raise Ash.Error.Unknown, ~r/does_not_exist/, fn ->
        BadPolicyRelName
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!(authorize?: true)
      end

      assert_raise Ash.Error.Unknown, ~r/:does_not_exist/, fn ->
        BadPolicyRelPathName
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!(authorize?: true)
      end
    end
  end
end
