# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.Generator.ResultNullabilityTest do
  use ExUnit.Case, async: true

  alias Ash.Info.Manifest.Generator.ResultNullability

  describe "for_aggregate/1" do
    test ":count always returns 0 minimum — not nullable" do
      assert ResultNullability.for_aggregate(%{kind: :count}) == false
    end

    test ":exists returns boolean — not nullable" do
      assert ResultNullability.for_aggregate(%{kind: :exists}) == false
    end

    test ":list returns [] on empty — not nullable" do
      assert ResultNullability.for_aggregate(%{kind: :list}) == false
    end

    test ":first is nil on empty relationship" do
      assert ResultNullability.for_aggregate(%{kind: :first}) == true
    end

    test ":max is nil on empty relationship" do
      assert ResultNullability.for_aggregate(%{kind: :max}) == true
    end

    test ":min is nil on empty relationship" do
      assert ResultNullability.for_aggregate(%{kind: :min}) == true
    end

    test ":sum is nil on empty relationship" do
      assert ResultNullability.for_aggregate(%{kind: :sum}) == true
    end

    test ":avg is nil on empty relationship" do
      assert ResultNullability.for_aggregate(%{kind: :avg}) == true
    end

    test ":custom preserves explicit allow_nil? when set" do
      assert ResultNullability.for_aggregate(%{kind: :custom, allow_nil?: false}) == false
      assert ResultNullability.for_aggregate(%{kind: :custom, allow_nil?: true}) == true
    end

    test ":custom defaults to true when allow_nil? not present" do
      assert ResultNullability.for_aggregate(%{kind: :custom}) == true
    end
  end
end
