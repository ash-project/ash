defmodule Ash.FlowTest.TransactionTest do
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

  defmodule CountValue do
    use Ash.Flow.Step

    def run(input, opts, _context) do
      field = opts[:field] || :value

      {:ok, input |> Map.get(field) |> List.wrap() |> Enum.count()}
    end
  end

  defmodule Error do
    use Ash.Flow.Step

    def run(%{error: :raise}, _opts, _context) do
      raise "uh oh!"
    end

    def run(%{error: :return}, _opts, _context) do
      {:error, "uh oh!"}
    end

    def run(_input, _opts, _context) do
      {:ok, nil}
    end
  end

  defmodule UnapproveAllUsers do
    @moduledoc "Foo"
    use Ash.Flow

    flow do
      api Api

      argument :org_name, :string do
        allow_nil? false
      end

      argument :error, :atom do
        constraints one_of: [:raise, :return]
      end

      returns :count_unapproved_users
    end

    steps do
      transaction :get_org_and_unapprove_users, Org do
        read :get_org, Org, :by_name do
          input(%{
            name: arg(:org_name)
          })
        end

        read :list_users, User, :for_org do
          input %{
            org: path(result(:get_org), :id)
          }
        end

        map :unapprove_users, result(:list_users) do
          update :unapprove_user, User, :unapprove do
            record element(:unapprove_users)
          end
        end

        custom :count_unapproved_users, {CountValue, field: :users} do
          input %{
            users: result(:list_users)
          }
        end

        custom :error, Error do
          input %{
            error: arg(:error)
          }
        end
      end
    end
  end

  test "a flow in a transaction can be run" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Api.create!()
    |> Ash.Changeset.for_update(:approve, %{})
    |> Api.update!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()
    |> Ash.Changeset.for_update(:approve, %{})
    |> Api.update!()

    UnapproveAllUsers.run!("Org 1")
  end

  test "a flow in a transaction will be rolled back if an error is raised" do
    org =
      Org
      |> Ash.Changeset.for_create(:create, %{name: "Org 1"})
      |> Api.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "abc", org: org.id})
    |> Api.create!()
    |> Ash.Changeset.for_update(:approve, %{})
    |> Api.update!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "def", org: org.id})
    |> Api.create!()
    |> Ash.Changeset.for_update(:approve, %{})
    |> Api.update!()

    assert_raise(Ash.Error.Unknown, ~r/uh oh!/, fn ->
      UnapproveAllUsers.run!("Org 1", %{error: :raise})
    end)

    assert User |> Api.read!() |> Enum.all?(& &1.approved)
  end
end
