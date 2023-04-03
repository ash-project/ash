defmodule Ash.Test.Policy.RelatesToActorViaTest do
  @doc false
  use ExUnit.Case

  defmodule Actor do
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    actions do
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
                )
    end

    relationships do
      belongs_to :user, Ash.Test.Policy.RelatesToActorViaTest.User
      belongs_to :role, Ash.Test.Policy.RelatesToActorViaTest.Role
    end
  end

  defmodule User do
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    actions do
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
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
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
      belongs_to :user, Ash.Test.Policy.RelatesToActorViaTest.User
      has_many :roles, Ash.Test.Policy.RelatesToActorViaTest.Role
    end
  end

  defmodule Role do
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :account, Ash.Test.Policy.RelatesToActorViaTest.Account
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Account
      entry Actor
      entry User
      entry Role
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  describe "actor_at option" do
    test "relates_to_actor_via with belongs_to" do
      user =
        User
        |> Ash.Changeset.for_create(:create)
        |> Api.create!()

      account =
        Account
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:user, user, type: :append)
        |> Api.create!(authorize?: false)

      actor =
        Actor
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:user, user, type: :append)
        |> Api.create!(authorize?: false)
        |> Api.load!(:type)

      assert {:ok, _} = Api.get(Account, account.id, actor: actor)
    end

    test "relates_to_actor_via with has_many" do
      user =
        User
        |> Ash.Changeset.for_create(:create)
        |> Api.create!()

      account =
        Account
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:user, user, type: :append)
        |> Api.create!(authorize?: false)

      role =
        Role
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:account, account, type: :append)
        |> Api.create!(authorize?: false)

      actor =
        Actor
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.manage_relationship(:role, role, type: :append)
        |> Api.create!(authorize?: false)
        |> Api.load!(:type)

      assert {:ok, _} = Api.get(Account, account.id, actor: actor)
    end
  end
end
