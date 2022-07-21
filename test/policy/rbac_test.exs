defmodule Ash.Test.Policy.RbacTest do
  @doc false
  use ExUnit.Case

  alias Ash.Test.Support.PolicyRbac.{Api, File, Membership, Organization, User}

  setup do
    [
      user: Api.create!(Ash.Changeset.new(User)),
      org: Api.create!(Ash.Changeset.new(Organization))
    ]
  end

  test "if the actor has no permissions, they can't see anything", %{
    user: user,
    org: org
  } do
    create_file(org, "foo")
    create_file(org, "bar")
    create_file(org, "baz")

    assert Api.read!(File, actor: user) == []
  end

  test "if the actor has permission to read a file, they can only read that file", %{
    user: user,
    org: org
  } do
    file_with_access = create_file(org, "foo")
    give_role(user, org, :viewer, :file, file_with_access.id)
    create_file(org, "bar")
    create_file(org, "baz")

    assert [%{name: "foo"}] = Api.read!(File, actor: user)
  end

  test "unauthorized if no policy is defined", %{user: user} do
    assert_raise Ash.Error.Forbidden, fn ->
      Api.read!(User, actor: user) == []
    end
  end

  test "if the action can be performed, the can utility should return true", %{
    user: user,
    org: org
  } do
    file_with_access = create_file(org, "foo")
    give_role(user, org, :viewer, :file, file_with_access.id)
    create_file(org, "bar")
    create_file(org, "baz")

    assert Ash.Policy.Info.can(File, :read, user, api: Api)
  end

  defp give_role(user, org, role, resource, resource_id) do
    Membership
    |> Ash.Changeset.new(%{role: role, resource: resource, resource_id: resource_id})
    |> Ash.Changeset.replace_relationship(:user, user)
    |> Ash.Changeset.replace_relationship(:organization, org)
    |> Api.create!()
  end

  defp create_file(org, name) do
    File
    |> Ash.Changeset.new(%{name: name})
    |> Ash.Changeset.replace_relationship(:organization, org)
    |> Api.create!()
  end
end
