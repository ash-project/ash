defmodule Ash.FlowTest.SimpleFlowTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Org do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    identities do
      identity :unique_name, [:name]
    end

    actions do
      read :read do
        primary? true
      end

      read :by_name do
        argument :name, :string, allow_nil?: false
        get? true

        filter expr(name == ^arg(:name))
      end

      create :create
      update :update
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :users, Ash.FlowTest.User
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :read do
        primary? true
      end

      read :for_org do
        argument :org, :uuid, allow_nil?: false

        filter(expr(org_id == ^arg(:org)))
      end

      create :create do
        argument :org, :uuid, allow_nil?: false
        change manage_relationship(:org, type: :replace)
      end

      update :approve do
        accept []
        change set_attribute(:approved, true)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :first_name, :string
      attribute :last_name, :string

      attribute :approved, :boolean do
        private? true
      end
    end

    relationships do
      belongs_to :org, Org
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(User)
      entry(Org)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  defmodule GetOrgByName do
    use Ash.Flow

    flow do
      api Api

      argument :org_name, :string
      returns :get_org
    end

    steps do
      read :get_org, Org, :by_name do
        input(%{
          name: arg(:org_name)
        })
      end
    end
  end

  defmodule GetOrgAndUsers do
    use Ash.Flow

    flow do
      api Api
      argument :org_name, :string
      returns get_org: :org, list_users: :users
    end

    steps do
      read :get_org, Org, :by_name do
        input(%{
          name: arg(:org_name)
        })
      end

      read :list_users, User, :for_org do
        input(%{
          org: path(result(:get_org), :id)
        })
      end
    end
  end

  defmodule SignUpUser do
    use Ash.Flow

    flow do
      api Api
      argument :org_name, :string
      argument :first_name, :string
      argument :last_name, :string
      returns get_org: :org, create_user: :user
    end

    steps do
      read :get_org, Org, :by_name do
        input %{
          name: arg(:org_name)
        }
      end

      create :create_user, User, :create do
        input %{
          first_name: arg(:first_name),
          last_name: arg(:last_name),
          org: path(result(:get_org), :id)
        }
      end

      update :approve_user, User, :approve do
        record result(:create_user)
      end
    end
  end

  defmodule SignUpAndApproveUser do
    use Ash.Flow

    flow do
      api Api
      argument :org_name, :string
      argument :first_name, :string
      argument :last_name, :string
      returns get_org: :org, approve_user: :user
    end

    steps do
      read :get_org, Org, :by_name do
        input %{
          name: arg(:org_name)
        }
      end

      create :create_user, User, :create do
        input %{
          first_name: arg(:first_name),
          last_name: arg(:last_name),
          org: path(result(:get_org), :id)
        }
      end

      update :approve_user, User, :approve do
        record result(:create_user)
      end
    end
  end

  defmodule SignUpApproveUserThenDeleteIt do
    use Ash.Flow

    flow do
      api Api
      argument :org_name, :string
      argument :first_name, :string
      argument :last_name, :string
      returns get_org: :org, approve_user: :user
    end

    steps do
      read :get_org, Org, :by_name do
        input %{
          name: arg(:org_name)
        }
      end

      create :create_user, User, :create do
        input %{
          first_name: arg(:first_name),
          last_name: arg(:last_name),
          org: path(result(:get_org), :id)
        }
      end

      update :approve_user, User, :approve do
        record result(:create_user)
      end

      destroy :destroy_user, User, :destroy do
        record result(:approve_user)
      end
    end
  end

  test "a simple flow can be run" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    org_id = org.id

    assert %{id: ^org_id} = GetOrgByName.run!("Org 1")
  end

  test "a flow with multiple steps and dependencies can be run" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Ash.Changeset.replace_relationship(:org, org.id)
    |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()

    org_id = org.id

    assert %{org: %{id: ^org_id}, users: users} = GetOrgAndUsers.run!("Org 1")

    assert users |> Enum.map(& &1.first_name) |> Enum.sort() == ["abc", "def"]
  end

  test "a flow with a create step works" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    assert %{org: %{name: "Org 1"}, user: %{first_name: "Bruce", last_name: "Wayne"}} =
             SignUpUser.run!(org.name, "Bruce", "Wayne")
  end

  test "a flow with a create and an update step works" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    assert %{
             org: %{name: "Org 1"},
             user: %{first_name: "Bruce", last_name: "Wayne", approved: true}
           } = SignUpAndApproveUser.run!(org.name, "Bruce", "Wayne")
  end

  test "a flow with a create and an update and a destroy step works" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    assert %{
             org: %{name: "Org 1"},
             user: %{first_name: "Bruce", last_name: "Wayne", approved: true}
           } = SignUpApproveUserThenDeleteIt.run!(org.name, "Bruce", "Wayne")

    assert Api.read!(User) == []
  end
end
