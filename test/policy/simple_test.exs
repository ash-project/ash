defmodule Ash.Test.Policy.SimpleTest do
  @doc false
  use ExUnit.Case
  require Ash.Query

  alias Ash.Test.Support.PolicySimple.{
    Car,
    Context,
    Domain,
    Foo,
    Organization,
    Post,
    Trip,
    Tweet,
    User
  }

  setup do
    [
      user: Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false),
      admin:
        Ash.create!(Ash.Changeset.for_create(User, :create, %{admin: true}), authorize?: false)
    ]
  end

  test "bypass with condition does not apply subsequent filters", %{admin: admin, user: user} do
    Ash.create!(Ash.Changeset.for_create(Tweet, :create), authorize?: false)

    assert [_] = Ash.read!(Tweet, actor: admin)
    assert [] = Ash.read!(Tweet, actor: user)
  end

  test "arguments can be referenced in expression policies", %{admin: admin, user: user} do
    Tweet
    |> Ash.Changeset.for_create(:create_foo, %{foo: "foo", user_id: admin.id}, actor: user)
    |> Ash.create!()

    assert_raise Ash.Error.Forbidden, fn ->
      Tweet
      |> Ash.Changeset.for_create(:create_foo, %{foo: "bar", user_id: admin.id}, actor: user)
      |> Ash.create!()
    end
  end

  test "functions can be used as checks through `matches`", %{user: user} do
    Tweet
    |> Ash.Changeset.for_create(:create_bar, %{bar: 2}, actor: user)
    |> Ash.create!()

    Tweet
    |> Ash.Changeset.for_create(:create_bar, %{bar: 9}, actor: user)
    |> Ash.create!()

    assert_raise Ash.Error.Forbidden, fn ->
      Tweet
      |> Ash.Changeset.for_create(:create_bar, %{bar: 1}, actor: user)
      |> Ash.create!()
    end
  end

  test "filter checks work on create/update/destroy actions", %{user: user} do
    user2 = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.update!(Ash.Changeset.for_update(user, :update), actor: user2)
    end
  end

  test "filter checks work on update/destroy actions", %{user: user} do
    tweet =
      Tweet
      |> Ash.Changeset.for_create(:create)
      |> Ash.Changeset.manage_relationship(:user, user, type: :append_and_remove)
      |> Ash.create!(authorize?: false)

    changeset = Ash.Changeset.for_update(tweet, :update)

    assert Ash.Policy.Info.strict_check(user, changeset, Domain) == true

    tweet =
      Tweet
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!(authorize?: false)

    changeset = Ash.Changeset.for_update(tweet, :update)

    assert Ash.Policy.Info.strict_check(%{user | id: nil}, changeset, Domain) == false
  end

  test "non-filter checks work on create/update/destroy actions" do
    user = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.create!(Ash.Changeset.for_create(Post, :create, %{text: "foo"}), actor: user)
    end
  end

  test "filter checks work with related data", %{user: user} do
    organization =
      Organization
      |> Ash.Changeset.for_create(:create, %{owner: user.id})
      |> Ash.create!(authorize?: false)

    post1 =
      Post
      |> Ash.Changeset.for_create(:create, %{author: user.id, text: "aaa"})
      |> Ash.create!(authorize?: false)

    post2 =
      Post
      |> Ash.Changeset.for_create(:create, %{organization: organization.id, text: "bbb"})
      |> Ash.create!(authorize?: false)

    Post
    |> Ash.Changeset.for_create(:create, %{text: "invalid"})
    |> Ash.create!(authorize?: false)

    ids =
      Post
      |> Ash.read!(actor: user)
      |> Enum.map(& &1.id)
      |> Enum.sort()

    assert ids == Enum.sort([post1.id, post2.id])
  end

  test "authorize_unless properly combines", %{user: user} do
    Car
    |> Ash.Changeset.for_create(:authorize_unless, %{})
    |> Ash.create!(actor: user)
  end

  test "filter checks work on generic actions when they don't reference anything specifically", %{
    admin: admin,
    user: user
  } do
    assert_raise Ash.Error.Forbidden, fn ->
      Post
      |> Ash.ActionInput.for_action(:say_hello, %{from_an_admin?: true, to: "Fred"}, actor: user)
      |> Ash.run_action!()
    end

    assert "Hello Fred from an admin!" ==
             Post
             |> Ash.ActionInput.for_action(:say_hello, %{from_an_admin?: true, to: "Fred"},
               actor: admin
             )
             |> Ash.run_action!()
  end

  test "filter checks work with many to many related data and a filter", %{user: user} do
    car1 =
      Car
      |> Ash.Changeset.for_create(:create, %{users: [user.id]})
      |> Ash.create!(authorize?: false)

    car2 =
      Car
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!(authorize?: false)

    results =
      Car
      |> Ash.Query.filter(id == ^car2.id)
      |> Ash.read!(actor: user)

    assert results == []

    results =
      Car
      |> Ash.Query.filter(id == ^car1.id)
      |> Ash.read!(actor: user)
      |> Enum.map(& &1.id)

    assert results == [car1.id]
  end

  test "calculations that reference aggregates are properly authorized", %{user: user} do
    Car
    |> Ash.Changeset.for_create(:create, %{users: [user.id], active: false}, authorize?: false)
    |> Ash.create!()

    assert %{restricted_from_driving: false, has_car: true} =
             user
             |> Ash.load!([:restricted_from_driving, :has_car], authorize?: false)
             |> Map.take([:restricted_from_driving, :has_car])

    assert %{restricted_from_driving: true, has_car: false} =
             user
             |> Ash.load!([:restricted_from_driving, :has_car], authorize?: true)
             |> Map.take([:restricted_from_driving, :has_car])
  end

  test "filter checks work via deeply related data", %{user: user} do
    assert Ash.read!(Trip, actor: user) == []
  end

  test "changing_attributes with `:to` option works" do
    Foo
    |> Ash.Changeset.for_create(:create, %{name: "Foo"})
    |> Ash.create!()
  end

  test "checking context using expr works" do
    %{id: id} =
      context =
      Context
      |> Ash.Changeset.for_create(:create, %{name: "Foo"})
      |> Ash.create!()

    assert [%{id: ^id}] = Ash.read!(Context, context: %{name: "Foo"}, authorize?: true)

    assert %{name: "Bar"} =
             context
             |> Ash.Changeset.for_update(:update, %{name: "Bar"},
               context: %{name: "Foo"},
               authorize?: true
             )
             |> Ash.update!()

    assert {:ok, %{name: "Foo"}} =
             Domain.update_context(id, "Foo",
               context: %{name: "Bar"},
               actor: nil,
               authorize?: true
             )
  end

  test "a final always policy with a forbid if always is properly applied" do
    user = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    Ash.Test.Support.PolicySimple.Always
    |> Ash.Changeset.for_create(:create, %{user_id: user.id})
    |> Ash.create!(authorize?: false)

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.Test.Support.PolicySimple.Always
      |> Ash.read!(authorize?: true, actor: user)
    end
  end

  test "two filter condition checks combine properly" do
    user1 = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)
    user2 = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    user1_thing =
      Ash.Test.Support.PolicySimple.TwoFilters
      |> Ash.Changeset.for_create(:create, %{user_id: user1.id})
      |> Ash.create!(authorize?: false)

    user2_thing =
      Ash.Test.Support.PolicySimple.TwoFilters
      |> Ash.Changeset.for_create(:create, %{user_id: user2.id})
      |> Ash.create!(authorize?: false)

    assert [user1_got_thing] =
             Ash.Test.Support.PolicySimple.TwoFilters
             |> Ash.read!(authorize?: true, actor: user1)

    assert user1_got_thing.id == user1_thing.id

    assert [user2_got_thing] =
             Ash.Test.Support.PolicySimple.TwoFilters
             |> Ash.read!(authorize?: true, actor: user2)

    assert user2_got_thing.id == user2_thing.id
  end
end
