# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.AnyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.Any

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :update_with_any_validation do
        validate any([
                   one_of(:status, [:valid]),
                   match(:title, "^[a-z]+$")
                 ])
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :status, :atom do
        public?(true)
      end

      attribute :priority, :integer do
        public?(true)
      end

      attribute :title, :string do
        public?(true)
      end
    end
  end

  defmodule PassingValidation do
    use Ash.Resource.Validation

    @impl true
    def validate(_, _, _), do: :ok

    @impl true
    def describe(_opts), do: [message: "Passing validation", vars: []]
  end

  defmodule FailingValidation do
    use Ash.Resource.Validation

    @impl true
    def validate(_, _, _), do: {:error, :some_error}

    @impl true
    def describe(_opts), do: [message: "Failing validation", vars: []]
  end

  defmodule CustomValidationNoDescribe do
    use Ash.Resource.Validation

    @impl true
    def validate(_, _, _), do: {:error, :some_error}
  end

  describe "Any validation" do
    test "passes when at least one inner validation passes" do
      {:ok, opts} =
        Any.init(
          validations: [
            FailingValidation,
            PassingValidation,
            FailingValidation
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert :ok = Any.validate(changeset, opts, %{})
    end

    test "passes when all inner validations pass" do
      {:ok, opts} =
        Any.init(
          validations: [
            PassingValidation,
            PassingValidation
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert :ok = Any.validate(changeset, opts, %{})
    end

    test "fails when all inner validations fail" do
      {:ok, opts} =
        Any.init(
          validations: [
            FailingValidation,
            FailingValidation
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert {:error, %Ash.Error.Changes.InvalidAttribute{}} =
               Any.validate(changeset, opts, %{})
    end

    test "works with builtin validations" do
      {:ok, opts} =
        Any.init(
          validations: [
            Ash.Resource.Validation.Builtins.one_of(:status, [:canceled]),
            Ash.Resource.Validation.Builtins.one_of(:status, [:valid])
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert :ok = Any.validate(changeset, opts, %{})
    end

    test "fails with builtin validations when none match" do
      {:ok, opts} =
        Any.init(
          validations: [
            Ash.Resource.Validation.Builtins.one_of(:status, [:canceled]),
            Ash.Resource.Validation.Builtins.one_of(:status, [:archived])
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert {:error, %Ash.Error.Changes.InvalidAttribute{}} =
               Any.validate(changeset, opts, %{})
    end

    test "returns error on init if validation does not export `describe/1`" do
      assert_raise ArgumentError, ~r/must implement `describe\/1`/, fn ->
        Any.init(validations: [CustomValidationNoDescribe])
      end
    end

    test "describe returns appropriate message" do
      {:ok, opts} =
        Any.init(
          validations: [
            PassingValidation,
            FailingValidation
          ]
        )

      description = Any.describe(opts)

      assert description[:message] =~ "must pass at least one of:"
      assert description[:message] =~ "Passing validation"
      assert description[:message] =~ "Failing validation"
      assert description[:vars] == []
    end

    test "works atomically with update actions when one validation passes" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :invalid, title: "UPPERCASE"})
        |> Ash.create!()

      updated_post =
        post
        |> Ash.Changeset.for_update(:update_with_any_validation, %{
          status: :valid,
          title: "STILLUPPERCASE"
        })
        |> Ash.update!()

      assert updated_post.status == :valid
      assert updated_post.title == "STILLUPPERCASE"
    end

    test "works atomically with update actions when other validation passes" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :invalid, title: "UPPERCASE"})
        |> Ash.create!()

      updated_post =
        post
        |> Ash.Changeset.for_update(:update_with_any_validation, %{
          status: :invalid,
          title: "lowercase"
        })
        |> Ash.update!()

      assert updated_post.status == :invalid
      assert updated_post.title == "lowercase"
    end

    test "fails atomically with update actions when no validations pass" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :valid, title: "lowercase"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, fn ->
        post
        |> Ash.Changeset.for_update(:update_with_any_validation, %{
          status: :invalid,
          title: "UPPERCASE"
        })
        |> Ash.update!()
      end
    end
  end
end
