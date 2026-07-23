# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.AllTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.All

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :update_with_all_validation do
        validate all([
                   one_of(:status, [:valid]),
                   match(:title, "^[a-z]+$")
                 ])
      end

      update :update_with_any_all_validation do
        validate any([
                   all([
                     one_of(:status, [:valid]),
                     match(:title, "^[a-z]+$")
                   ]),
                   attribute_equals(:priority, 1)
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

  describe "All validation" do
    test "passes when all inner validations pass" do
      {:ok, opts} =
        All.init(
          validations: [
            PassingValidation,
            PassingValidation
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert :ok = All.validate(changeset, opts, %{})
    end

    test "fails when one inner validation fails" do
      {:ok, opts} =
        All.init(
          validations: [
            PassingValidation,
            FailingValidation,
            PassingValidation
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert {:error, %Ash.Error.Changes.InvalidAttribute{}} =
               All.validate(changeset, opts, %{})
    end

    test "fails when all inner validations fail" do
      {:ok, opts} =
        All.init(
          validations: [
            FailingValidation,
            FailingValidation
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert {:error, %Ash.Error.Changes.InvalidAttribute{}} =
               All.validate(changeset, opts, %{})
    end

    test "works with builtin validations" do
      {:ok, opts} =
        All.init(
          validations: [
            Ash.Resource.Validation.Builtins.one_of(:status, [:valid]),
            Ash.Resource.Validation.Builtins.match(:title, "^[a-z]+$")
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid, title: "lowercase"})

      assert :ok = All.validate(changeset, opts, %{})
    end

    test "fails with builtin validations when one does not match" do
      {:ok, opts} =
        All.init(
          validations: [
            Ash.Resource.Validation.Builtins.one_of(:status, [:valid]),
            Ash.Resource.Validation.Builtins.match(:title, "^[a-z]+$")
          ]
        )

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid, title: "UPPERCASE"})

      assert {:error, %Ash.Error.Changes.InvalidAttribute{}} =
               All.validate(changeset, opts, %{})
    end

    test "returns error on init if validation does not export `describe/1`" do
      assert_raise ArgumentError, ~r/must implement `describe\/1`/, fn ->
        All.init(validations: [CustomValidationNoDescribe])
      end
    end

    test "describe returns appropriate message" do
      {:ok, opts} =
        All.init(
          validations: [
            PassingValidation,
            FailingValidation
          ]
        )

      description = All.describe(opts)

      assert description[:message] =~ "must pass all of:"
      assert description[:message] =~ "Passing validation"
      assert description[:message] =~ "Failing validation"
      assert description[:vars] == []
    end

    test "works atomically with update actions when all validations pass" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :invalid, title: "UPPERCASE"})
        |> Ash.create!()

      updated_post =
        post
        |> Ash.Changeset.for_update(:update_with_all_validation, %{
          status: :valid,
          title: "lowercase"
        })
        |> Ash.update!()

      assert updated_post.status == :valid
      assert updated_post.title == "lowercase"
    end

    test "fails atomically with update actions when one validation fails" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :valid, title: "lowercase"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, fn ->
        post
        |> Ash.Changeset.for_update(:update_with_all_validation, %{
          status: :valid,
          title: "UPPERCASE"
        })
        |> Ash.update!()
      end
    end

    test "fails atomically with update actions when the other validation fails" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :valid, title: "lowercase"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, fn ->
        post
        |> Ash.Changeset.for_update(:update_with_all_validation, %{
          status: :invalid,
          title: "lowercase"
        })
        |> Ash.update!()
      end
    end

    test "composes with any, passing when the all branch passes" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :invalid, title: "UPPERCASE"})
        |> Ash.create!()

      updated_post =
        post
        |> Ash.Changeset.for_update(:update_with_any_all_validation, %{
          status: :valid,
          title: "lowercase",
          priority: 2
        })
        |> Ash.update!()

      assert updated_post.status == :valid
    end

    test "composes with any, passing when the other branch passes" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :invalid, title: "UPPERCASE"})
        |> Ash.create!()

      updated_post =
        post
        |> Ash.Changeset.for_update(:update_with_any_all_validation, %{
          status: :invalid,
          title: "UPPERCASE",
          priority: 1
        })
        |> Ash.update!()

      assert updated_post.priority == 1
    end

    test "composes with any, failing when the all branch only partially passes" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :invalid, title: "UPPERCASE"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, fn ->
        post
        |> Ash.Changeset.for_update(:update_with_any_all_validation, %{
          status: :valid,
          title: "UPPERCASE",
          priority: 2
        })
        |> Ash.update!()
      end
    end
  end
end
