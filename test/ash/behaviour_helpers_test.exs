# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.BehaviourHelpersTest do
  use ExUnit.Case, async: true

  describe "call_and_validate_return/5" do
    defmodule ValidReturnModule do
      def callback_ok(_), do: :ok
      def callback_tuple(_, _), do: {:ok, :value}
      def callback_changeset(_), do: %Ash.Changeset{}
    end

    defmodule InvalidReturnModule do
      def returns_wrong_atom(_), do: :error
      def returns_wrong_type(_), do: "not allowed"
    end

    test "returns result when it matches an allowed pattern (atom)" do
      result =
        Ash.BehaviourHelpers.call_and_validate_return(
          ValidReturnModule,
          :callback_ok,
          [nil],
          [:ok],
          []
        )

      assert result == :ok
    end

    test "returns result when it matches an allowed pattern (tuple with :_)" do
      result =
        Ash.BehaviourHelpers.call_and_validate_return(
          ValidReturnModule,
          :callback_tuple,
          [nil, nil],
          [{:ok, :_}],
          []
        )

      assert result == {:ok, :value}
    end

    test "returns result when it matches an allowed pattern (struct)" do
      result =
        Ash.BehaviourHelpers.call_and_validate_return(
          ValidReturnModule,
          :callback_changeset,
          [nil],
          [%Ash.Changeset{}],
          []
        )

      assert %Ash.Changeset{} = result
    end

    test "returns result when it matches one of multiple allowed patterns" do
      result =
        Ash.BehaviourHelpers.call_and_validate_return(
          ValidReturnModule,
          :callback_tuple,
          [nil, nil],
          [:ok, {:ok, :_}, {:error, :_}],
          []
        )

      assert result == {:ok, :value}
    end

    test "raises InvalidReturnType when result matches no pattern" do
      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.BehaviourHelpers.call_and_validate_return(
          InvalidReturnModule,
          :returns_wrong_atom,
          [nil],
          [:ok],
          []
        )
      end
    end

    test "error message contains callback name and expected shapes" do
      try do
        Ash.BehaviourHelpers.call_and_validate_return(
          InvalidReturnModule,
          :returns_wrong_atom,
          [nil],
          [:ok, {:error, :_}],
          behaviour: Ash.Resource.Validation,
          callback_name: "validate/3"
        )
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)
          assert message =~ "validate/3"
          assert message =~ "returns_wrong_atom" or message =~ "validate"
          assert message =~ ":ok"
          assert message =~ "{:error, _}"
      end
    end

    test "supports function: option for callback name in error" do
      try do
        Ash.BehaviourHelpers.call_and_validate_return(
          InvalidReturnModule,
          :returns_wrong_type,
          [nil],
          [:ok],
          behaviour: Ash.Resource.Change,
          function: :change
        )
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)
          assert message =~ "change/1"
          assert message =~ "Ash.Resource.Change"
      end
    end
  end
end
