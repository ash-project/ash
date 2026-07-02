# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.ActorNotLoadedTest do
  @doc false
  use ExUnit.Case

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    actions do
      defaults [:read, create: [:title]]
    end

    policies do
      # Policy using authorize_if with actor field reference.
      # When actor field is NotLoaded, should behave like no actor (deny).
      policy action_type(:read) do
        authorize_if expr(^actor(:admin) == true)
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end
  end

  defmodule Comment do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :body, :string, public?: true
    end

    actions do
      defaults [:read, create: [:body]]
    end

    policies do
      # Policy using forbid_if with actor field reference.
      # When actor field is NotLoaded, should behave like no actor
      # (forbid_if doesn't trigger, authorize_if always() catches).
      policy action_type(:read) do
        forbid_if expr(^actor(:blocked) == true)
        authorize_if always()
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end
  end

  defmodule Article do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :content, :string, public?: true
    end

    actions do
      defaults [:read, create: [:content]]
    end

    policies do
      # Policy using a nested actor path.
      # Tests that get_path also handles NotLoaded correctly.
      policy action_type(:read) do
        authorize_if expr(^actor([:settings, :can_read]) == true)
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end
  end

  describe "authorize_if with actor field reference" do
    test "actor with loaded field (true) authorizes" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "hello"})
      |> Ash.create!(authorize?: false)

      assert [%Post{title: "hello"}] =
               Ash.read!(Post, actor: %{admin: true})
    end

    test "actor with loaded field (false) denies" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "hello"})
      |> Ash.create!(authorize?: false)

      assert [] = Ash.read!(Post, actor: %{admin: false})
    end

    test "nil actor denies" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "hello"})
      |> Ash.create!(authorize?: false)

      assert [] = Ash.read!(Post, actor: nil)
    end

    test "actor with NotLoaded field behaves the same as nil actor" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "hello"})
      |> Ash.create!(authorize?: false)

      actor = %{admin: %Ash.NotLoaded{field: :admin, type: :attribute}}

      assert [] = Ash.read!(Post, actor: actor)
    end
  end

  describe "forbid_if with actor field reference" do
    test "actor with loaded field (true) forbids" do
      Comment
      |> Ash.Changeset.for_create(:create, %{body: "hi"})
      |> Ash.create!(authorize?: false)

      assert [] = Ash.read!(Comment, actor: %{blocked: true})
    end

    test "actor with loaded field (false) allows" do
      Comment
      |> Ash.Changeset.for_create(:create, %{body: "hi"})
      |> Ash.create!(authorize?: false)

      assert [%Comment{body: "hi"}] =
               Ash.read!(Comment, actor: %{blocked: false})
    end

    test "nil actor allows (forbid_if doesn't trigger, authorize_if always catches)" do
      Comment
      |> Ash.Changeset.for_create(:create, %{body: "hi"})
      |> Ash.create!(authorize?: false)

      assert [%Comment{body: "hi"}] = Ash.read!(Comment, actor: nil)
    end

    test "actor with NotLoaded field behaves the same as nil actor" do
      Comment
      |> Ash.Changeset.for_create(:create, %{body: "hi"})
      |> Ash.create!(authorize?: false)

      actor = %{blocked: %Ash.NotLoaded{field: :blocked, type: :attribute}}

      assert [%Comment{body: "hi"}] = Ash.read!(Comment, actor: actor)
    end
  end

  describe "nested actor path with NotLoaded" do
    test "actor with loaded nested path authorizes" do
      Article
      |> Ash.Changeset.for_create(:create, %{content: "article"})
      |> Ash.create!(authorize?: false)

      actor = %{settings: %{can_read: true}}

      assert [%Article{content: "article"}] =
               Ash.read!(Article, actor: actor)
    end

    test "actor with NotLoaded on nested path behaves the same as nil actor" do
      Article
      |> Ash.Changeset.for_create(:create, %{content: "article"})
      |> Ash.create!(authorize?: false)

      actor = %{settings: %Ash.NotLoaded{field: :settings, type: :attribute}}

      assert [] = Ash.read!(Article, actor: actor)
    end
  end
end
