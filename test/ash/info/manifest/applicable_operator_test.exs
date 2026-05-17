# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.ApplicableOperatorTest do
  use ExUnit.Case, async: true

  alias Ash.Info.Manifest.{ApplicableFunction, ApplicableOperator}

  describe "%ApplicableOperator{}" do
    test "carries a name atom and an rhs descriptor" do
      op = %ApplicableOperator{name: :==, rhs: :same}
      assert op.name == :==
      assert op.rhs == :same
    end

    test "rhs can be :any" do
      assert %ApplicableOperator{rhs: :any} = %ApplicableOperator{name: :is_nil, rhs: :any}
    end

    test "rhs can be a concrete builtin atom" do
      assert %ApplicableOperator{rhs: {:concrete, :boolean}} =
               %ApplicableOperator{name: :is_nil, rhs: {:concrete, :boolean}}
    end

    test "rhs can be a concrete module" do
      assert %ApplicableOperator{rhs: {:concrete, Ash.Type.UUID}} =
               %ApplicableOperator{name: :==, rhs: {:concrete, Ash.Type.UUID}}
    end

    test "rhs can nest as {:array, rhs}" do
      assert %ApplicableOperator{rhs: {:array, :same}} =
               %ApplicableOperator{name: :in, rhs: {:array, :same}}
    end
  end

  describe "%ApplicableFunction{}" do
    test "shares the same shape as ApplicableOperator" do
      fun = %ApplicableFunction{name: :contains, rhs: {:concrete, :string}}
      assert fun.name == :contains
      assert fun.rhs == {:concrete, :string}
    end
  end
end
