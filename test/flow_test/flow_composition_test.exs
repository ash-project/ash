defmodule Ash.FlowTest.FlowCompositionTest do
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

      update :update do
        primary? true
      end

      update :approve do
        accept []
        change set_attribute(:approved, true)
      end

      update :unapprove do
        accept []
        change set_attribute(:approved, false)
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
      run_flow :get_org, GetOrgByName do
        input %{
          org_name: arg(:org_name)
        }
      end

      read :list_users, User, :for_org do
        input %{
          org: path(result(:get_org), :id)
        }
      end
    end
  end

  defmodule GetOrgAndUsersAndUnapproveThem do
    use Ash.Flow

    flow do
      api Api
      argument :org_name, :string
      returns :unapprove_users
    end

    steps do
      run_flow :get_org_and_users, GetOrgAndUsers do
        input %{
          org_name: arg(:org_name)
        }
      end

      map :unapprove_users, path(result(:get_org_and_users), :users) do
        update :unapprove_user, User, :unapprove do
          record element(:unapprove_users)
        end
      end
    end
  end

  defmodule CountValue do
    use Ash.Flow.Step

    def run(input, opts, _context) do
      field = opts[:field] || :value

      {:ok, input |> Map.get(field) |> List.wrap() |> Enum.count()}
    end
  end

  defmodule GetOrgAndUsersAndUnapproveThemReturningCount do
    use Ash.Flow

    flow do
      api Api
      argument :org_name, :string
      returns :count_unapproved_users
    end

    steps do
      run_flow :get_org_and_users_and_unapprove_them, GetOrgAndUsersAndUnapproveThem do
        input %{
          org_name: arg(:org_name)
        }
      end

      custom :count_unapproved_users, {CountValue, field: :users} do
        input %{
          users: result(:get_org_and_users_and_unapprove_them)
        }
      end
    end
  end

  test "a flow can reference other flows" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Ash.Changeset.force_change_attribute(:approved, true)
    |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Ash.Changeset.force_change_attribute(:approved, true)
    |> Api.create!()

    org_id = org.id

    assert %{org: %{id: ^org_id}, users: users} = GetOrgAndUsers.run!("Org 1")

    assert users |> Enum.map(& &1.first_name) |> Enum.sort() == ["abc", "def"]
  end

  test "a map will run nested steps over all elements" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()

    GetOrgAndUsersAndUnapproveThem.run!("Org 1")

    assert Enum.all?(User |> Api.read!(), &(&1.approved == false))
  end

  test "a custom step can be used to introduce custom logic" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()

    assert GetOrgAndUsersAndUnapproveThemReturningCount.run!("Org 1") == 2
  end
end
