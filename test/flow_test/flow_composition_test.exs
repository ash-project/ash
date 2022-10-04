defmodule Ash.FlowTest.FlowCompositionTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Flow.Result
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

      argument :org_name, :string do
        allow_nil? false
      end

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

      argument :org_name, :string do
        allow_nil? false
      end

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

    assert %Ash.Flow.Result{result: %{org: %{id: ^org_id}, users: users}} =
             GetOrgAndUsers.run!("Org 1")

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

    assert %Result{result: 2} = GetOrgAndUsersAndUnapproveThemReturningCount.run!("Org 1")
  end
end
