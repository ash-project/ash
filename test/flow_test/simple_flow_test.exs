defmodule Ash.FlowTest.SimpleFlowTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Support.Flow.{Api, Org, User}

  setup do
    ExUnit.CaptureLog.capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Api)
    end)

    on_exit(fn ->
      ExUnit.CaptureLog.capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)
  end

  defmodule GetOrgByName do
    use Ash.Flow

    flow do
      api Api

      argument :org_name, :string do
        allow_nil? false
      end

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

      argument :org_name, :string do
        allow_nil? false
      end

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

      argument :org_name, :string do
        allow_nil? false
      end

      argument :first_name, :string do
        allow_nil? false
      end

      argument :last_name, :string do
        allow_nil? false
      end

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

      argument :org_name, :string do
        allow_nil? false
      end

      argument :first_name, :string do
        allow_nil? false
      end

      argument :last_name, :string do
        allow_nil? false
      end

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

      argument :org_name, :string do
        allow_nil? false
      end

      argument :first_name, :string do
        allow_nil? false
      end

      argument :last_name, :string do
        allow_nil? false
      end

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
