# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.ActorNotLoadedTest do
  @moduledoc """
  Covers ash-project/ash#2057.

  A policy that references a field on the actor must not silently succeed when
  that field is `%Ash.NotLoaded{}`. An unloaded field means the policy cannot
  actually be evaluated, so an error is raised rather than the reference
  quietly comparing as a `NotLoaded` struct.

  A `nil` actor is a distinct, supported case: anonymous access. Actor
  references simply resolve to `nil` and policy evaluation continues.
  """
  use ExUnit.Case

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
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
      # Reads are authorized only for an admin actor. `forbid_if` mirrors the
      # shape reported in the original issue.
      policy action_type(:read) do
        forbid_if expr(^actor(:banned) == true)
        authorize_if always()
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end
  end

  defmodule Article do
    @moduledoc false
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
      # Nested actor path, to cover `get_path/2` as well as the direct
      # field lookup exercised by `Post`.
      policy action_type(:read) do
        authorize_if expr(^actor([:settings, :can_read]) == true)
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end
  end

  defp create_post! do
    Post
    |> Ash.Changeset.for_create(:create, %{title: "hello"})
    |> Ash.create!(authorize?: false)
  end

  defp create_article! do
    Article
    |> Ash.Changeset.for_create(:create, %{content: "article"})
    |> Ash.create!(authorize?: false)
  end

  describe "actor field is loaded" do
    test "policy evaluates against the real value" do
      create_post!()

      assert [%Post{title: "hello"}] = Ash.read!(Post, actor: %{banned: false})
      assert [] = Ash.read!(Post, actor: %{banned: true})
    end

    test "nested actor path evaluates against the real value" do
      create_article!()

      assert [%Article{content: "article"}] =
               Ash.read!(Article, actor: %{settings: %{can_read: true}})

      assert [] = Ash.read!(Article, actor: %{settings: %{can_read: false}})
    end
  end

  describe "actor is nil" do
    test "actor references resolve to nil and policy evaluation continues" do
      create_post!()

      # `forbid_if ^actor(:banned) == true` does not trigger, so the
      # trailing `authorize_if always()` authorizes.
      assert [%Post{title: "hello"}] = Ash.read!(Post, actor: nil)
    end

    test "nested actor path resolves to nil rather than raising" do
      create_article!()

      # No actor means the check does not match, so nothing is authorized.
      assert [] = Ash.read!(Article, actor: nil)
    end
  end

  describe "actor is present but the referenced field is not loaded" do
    test "raises rather than silently evaluating the NotLoaded struct" do
      create_post!()

      actor = %{banned: %Ash.NotLoaded{field: :banned, type: :attribute}}

      assert_raise Ash.Error.Unknown, ~r/not loaded/, fn ->
        Ash.read!(Post, actor: actor)
      end
    end

    test "raises when a nested actor path is not loaded" do
      create_article!()

      actor = %{settings: %Ash.NotLoaded{field: :settings, type: :attribute}}

      assert_raise Ash.Error.Unknown, ~r/not loaded/, fn ->
        Ash.read!(Article, actor: actor)
      end
    end
  end
end
