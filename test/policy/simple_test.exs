defmodule Ash.Test.Policy.SimpleTest do
  @doc false
  use ExUnit.Case
  require Ash.Query

  alias Ash.Test.Support.PolicySimple.{Api, Car, Organization, Post, Trip, Tweet, User}

  setup do
    [
      user: Api.create!(Ash.Changeset.new(User), authorize?: false),
      admin: Api.create!(Ash.Changeset.new(User, %{admin: true}), authorize?: false)
    ]
  end

  test "bypass with condition does not apply subsequent filters", %{admin: admin, user: user} do
    Api.create!(Ash.Changeset.new(Tweet), authorize?: false)

    assert [_] = Api.read!(Tweet, actor: admin)
    assert [] = Api.read!(Tweet, actor: user)
  end

  test "arguments can be referenced in expression policies", %{admin: admin, user: user} do
    Tweet
    |> Ash.Changeset.for_create(:create_foo, %{foo: "foo", user_id: admin.id}, actor: user)
    |> Api.create!()

    assert_raise Ash.Error.Forbidden, fn ->
      Tweet
      |> Ash.Changeset.for_create(:create_foo, %{foo: "bar", user_id: admin.id}, actor: user)
      |> Api.create!()
    end
  end

  test "filter checks work on create/update/destroy actions", %{user: user} do
    user2 = Api.create!(Ash.Changeset.new(User), authorize?: false)

    assert_raise Ash.Error.Forbidden, fn ->
      Api.update!(Ash.Changeset.new(user), actor: user2)
    end
  end

  test "filter checks work on update/destroy actions", %{user: user} do
    tweet =
      Tweet
      |> Ash.Changeset.for_create(:create)
      |> Ash.Changeset.manage_relationship(:user, user, type: :append_and_remove)
      |> Api.create!(authorize?: false)

    changeset = Ash.Changeset.for_update(tweet, :update)

    assert Ash.Policy.Info.strict_check(user, changeset, Api) == true

    tweet =
      Tweet
      |> Ash.Changeset.for_create(:create)
      |> Api.create!(authorize?: false)

    changeset = Ash.Changeset.for_update(tweet, :update)

    assert Ash.Policy.Info.strict_check(%{user | id: nil}, changeset, Api) == false
  end

  test "non-filter checks work on create/update/destroy actions" do
    user = Api.create!(Ash.Changeset.new(User), authorize?: false)

    assert_raise Ash.Error.Forbidden, fn ->
      Api.create!(Ash.Changeset.new(Post, %{text: "foo"}), actor: user)
    end
  end

  test "filter checks work with related data", %{user: user} do
    organization =
      Organization
      |> Ash.Changeset.for_create(:create, %{owner: user.id})
      |> Api.create!(authorize?: false)

    post1 =
      Post
      |> Ash.Changeset.for_create(:create, %{author: user.id, text: "aaa"})
      |> Api.create!(authorize?: false)

    post2 =
      Post
      |> Ash.Changeset.for_create(:create, %{organization: organization.id, text: "bbb"})
      |> Api.create!(authorize?: false)

    Post
    |> Ash.Changeset.for_create(:create, %{text: "invalid"})
    |> Api.create!(authorize?: false)

    ids =
      Post
      |> Api.read!(actor: user)
      |> Enum.map(& &1.id)
      |> Enum.sort()

    assert ids == Enum.sort([post1.id, post2.id])
  end

  test "authorize_unless properly combines", %{user: user} do
    Car
    |> Ash.Changeset.for_create(:authorize_unless, %{})
    |> Api.create!(actor: user)
  end

  test "filter checks work with many to many related data and a filter", %{user: user} do
    car1 =
      Car
      |> Ash.Changeset.for_create(:create, %{users: [user.id]})
      |> Api.create!(authorize?: false)

    car2 =
      Car
      |> Ash.Changeset.for_create(:create, %{})
      |> Api.create!(authorize?: false)

    results =
      Car
      |> Ash.Query.filter(id == ^car2.id)
      |> Api.read!(actor: user)

    assert results == []

    results =
      Car
      |> Ash.Query.filter(id == ^car1.id)
      |> Api.read!(actor: user)
      |> Enum.map(& &1.id)

    assert results == [car1.id]
  end

  test "calculations that reference aggregates are properly authorized", %{user: user} do
    Car
    |> Ash.Changeset.for_create(:create, %{users: [user.id], active: false})
    |> Api.create!()

    :timer.sleep(2)

    assert %{restricted_from_driving: false, has_car: true} =
             user
             |> Api.load!([:restricted_from_driving, :has_car], authorize?: false)
             |> Map.take([:restricted_from_driving, :has_car])

    assert %{restricted_from_driving: true, has_car: false} =
             user
             |> Api.load!([:restricted_from_driving, :has_car], authorize?: true)
             |> Map.take([:restricted_from_driving, :has_car])
  end

  test "filter checks work via deeply related data", %{user: user} do
    assert Api.read!(Trip, actor: user) == []
  end
end
