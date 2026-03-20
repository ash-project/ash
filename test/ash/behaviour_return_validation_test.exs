# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.BehaviourReturnValidationTest do
  @moduledoc """
  Verifies that behaviour wrappers raise Ash.Error.Framework.InvalidReturnType
  with a clear message when an implementation returns an invalid value.
  """
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  describe "Ash.Resource.Change" do
    defmodule ChangeReturnsWrong do
      use Ash.Resource.Change

      def change(_changeset, _opts, _context) do
        :wrong
      end
    end

    defmodule ChangeResource do
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
        attribute :name, :string, public?: true
      end

      actions do
        default_accept :*
        defaults [:read]

        create :other_action do
          # no change - we use this only to build a changeset
        end

        create :create_action do
          change ChangeReturnsWrong
        end
      end
    end

    test "raises InvalidReturnType when change returns wrong shape" do
      # Use other_action so building the changeset does not run ChangeReturnsWrong
      changeset = Ash.Changeset.for_create(ChangeResource, :other_action)
      context = struct(Ash.Resource.Change.Context, %{})

      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Resource.Change.change(ChangeReturnsWrong, changeset, [], context)
      end
    end

    test "error message includes callback name and expected return type" do
      changeset = Ash.Changeset.for_create(ChangeResource, :other_action)
      context = struct(Ash.Resource.Change.Context, %{})

      try do
        Ash.Resource.Change.change(ChangeReturnsWrong, changeset, [], context)
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)
          assert message =~ "change/3", "message should include callback name: #{message}"
          assert message =~ "ChangeReturnsWrong" or message =~ "change"

          assert message =~ "Ash.Changeset" or message =~ "changeset",
                 "message should describe expected return: #{message}"
      end
    end
  end

  describe "Ash.Resource.Validation" do
    defmodule ValidationReturnsWrong do
      use Ash.Resource.Validation

      def validate(_changeset, _opts, _context) do
        :wrong
      end
    end

    defmodule ValidationResource do
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
        attribute :name, :string, public?: true
      end

      actions do
        default_accept :*
        defaults [:read]

        create :other_action do
          # no validation - we use this only to build a changeset
        end

        create :create_action do
          validate ValidationReturnsWrong
        end
      end
    end

    test "raises InvalidReturnType when validate returns wrong shape" do
      # Use other_action so building the changeset does not run ValidationReturnsWrong
      changeset = Ash.Changeset.for_create(ValidationResource, :other_action)
      context = struct(Ash.Resource.Validation.Context, %{})

      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Resource.Validation.validate(
          ValidationReturnsWrong,
          changeset,
          [],
          context
        )
      end
    end

    test "error message includes callback name and expected return types" do
      changeset = Ash.Changeset.for_create(ValidationResource, :other_action)
      context = struct(Ash.Resource.Validation.Context, %{})

      try do
        Ash.Resource.Validation.validate(
          ValidationReturnsWrong,
          changeset,
          [],
          context
        )
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)
          assert message =~ "validate", "message should include callback name: #{message}"
          assert message =~ "ValidationReturnsWrong" or message =~ "validate"

          assert (message =~ ":ok" or message =~ "{:error") and
                   (message =~ "expected" or message =~ "expects"),
                 "message should describe expected return types: #{message}"
      end
    end
  end

  describe "Ash.Authorizer" do
    defmodule AuthorizerReturnsWrongInitialState do
      use Ash.Authorizer

      def initial_state(_actor, _resource, _action, _domain) do
        :wrong
      end

      def strict_check_context(_state), do: [:query]
      def strict_check(_state, _context), do: {:authorized, %{}}
      def check_context(_state), do: [:query]
      def check(_state, _context), do: :authorized
    end

    defmodule AuthorizerResource do
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
        attribute :name, :string, public?: true
      end

      actions do
        default_accept :*
        defaults [:read]
      end
    end

    test "raises InvalidReturnType when initial_state returns wrong shape" do
      action = %Ash.Resource.Actions.Action{name: :read, type: :read}

      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Authorizer.initial_state(
          AuthorizerReturnsWrongInitialState,
          nil,
          AuthorizerResource,
          action,
          Domain
        )
      end
    end

    test "error message includes callback name and expected return type" do
      action = %Ash.Resource.Actions.Action{name: :read, type: :read}

      try do
        Ash.Authorizer.initial_state(
          AuthorizerReturnsWrongInitialState,
          nil,
          AuthorizerResource,
          action,
          Domain
        )
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)

          assert message =~ "initial_state/4",
                 "message should include callback name: #{message}"

          assert message =~ "AuthorizerReturnsWrongInitialState" or message =~ "initial_state"

          assert message =~ "map" or message =~ "state",
                 "message should describe expected return: #{message}"
      end
    end

    defmodule AuthorizerReturnsWrongStrictCheck do
      use Ash.Authorizer

      def initial_state(_actor, _resource, _action, _domain), do: %{}
      def strict_check_context(_state), do: [:query]
      def strict_check(_state, _context), do: :wrong
      def check_context(_state), do: [:query]
      def check(_state, _context), do: :authorized
    end

    test "raises InvalidReturnType when strict_check returns wrong shape" do
      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Authorizer.strict_check(
          AuthorizerReturnsWrongStrictCheck,
          %{},
          %{}
        )
      end
    end

    test "strict_check error message includes callback name and expected shapes" do
      try do
        Ash.Authorizer.strict_check(
          AuthorizerReturnsWrongStrictCheck,
          %{},
          %{}
        )
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)
          assert message =~ "strict_check/2", "message should include callback name: #{message}"
          assert message =~ "AuthorizerReturnsWrongStrictCheck" or message =~ "strict_check"

          assert message =~ "authorized" or message =~ "continue" or message =~ "expects",
                 "message should describe expected return shapes: #{message}"
      end
    end
  end
end
