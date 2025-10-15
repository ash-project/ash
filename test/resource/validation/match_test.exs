# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.MatchTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Expr

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create]

      update :atomic_update do
        argument :alternative_language, :string

        validate match(:language_code, ~r/^[a-z]{2,3}$/) do
          always_atomic? true
        end

        validate match(:alternative_language, ~r/^[a-z]{2,3}$/) do
          always_atomic? true
        end
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :language_code, :string do
        public?(true)
      end

      attribute :other, :string do
        public?(true)
      end
    end
  end

  describe "is atomic" do
    setup do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{language_code: "en"})
        |> Ash.create!()

      {:ok, post: post}
    end

    test "on arguments and fixed changes", %{post: post} do
      post
      |> Ash.Changeset.for_update(:atomic_update, %{
        language_code: "it",
        alternative_language: "it"
      })
      |> Ash.update!()
    end

    test "if arguments are omitted", %{post: post} do
      post
      |> Ash.Changeset.for_update(:atomic_update, %{language_code: "it"})
      |> Ash.update!()
    end

    test "on atomic_update with fixed value", %{post: post} do
      post
      |> Ash.Changeset.new()
      |> Ash.Changeset.atomic_update(:language_code, "it")
      |> Ash.Changeset.for_update(:atomic_update, %{language_code: "ab"})
      |> Ash.update!()
    end
  end

  describe "is not atomic" do
    setup do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{language_code: "en"})
        |> Ash.create!()

      {:ok, post: post}
    end

    test "if the attribute is not changing", %{post: post} do
      assert {:error, %Ash.Error.Invalid{errors: [error]}} =
               post
               |> Ash.Changeset.for_update(:atomic_update, %{other: "foo"})
               |> Ash.update()

      assert error.message =~ "not changing"
    end

    test "if the attribute is updated with atomic_update", %{post: post} do
      assert {:error, %Ash.Error.Invalid{errors: [error]}} =
               post
               |> Ash.Changeset.new()
               |> Ash.Changeset.atomic_update(
                 :language_code,
                 Ash.Expr.expr(language_code <> "new")
               )
               |> Ash.Changeset.for_update(:atomic_update, %{})
               |> Ash.update()

      assert error.message =~ "can't match on an atomic expression"
    end
  end
end
