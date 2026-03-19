# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Query.RequiredErrorTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.RequiredError

  test "required!/2 returns error when value is nil" do
    attribute = %{name: :title, resource: MyApp.Post}

    assert {:error, %Ash.Error.Changes.Required{} = err} =
             RequiredError.evaluate(%{arguments: [nil, attribute]})

    assert err.field == :title
    assert err.resource == MyApp.Post
    assert err.type == :attribute
  end

  test "required!/2 returns value when non-nil" do
    attribute = %{name: :title, resource: MyApp.Post}

    assert {:known, "hello"} =
             RequiredError.evaluate(%{arguments: ["hello", attribute]})
  end

  test "required!/2 treats false as present" do
    attribute = %{name: :title, resource: MyApp.Post}

    assert {:known, false} =
             RequiredError.evaluate(%{arguments: [false, attribute]})
  end

  test "required!/2 treats 0 as present" do
    attribute = %{name: :title, resource: MyApp.Post}

    assert {:known, 0} =
             RequiredError.evaluate(%{arguments: [0, attribute]})
  end

  test "required!/2 treats empty string as present" do
    attribute = %{name: :title, resource: MyApp.Post}

    assert {:known, ""} =
             RequiredError.evaluate(%{arguments: ["", attribute]})
  end

  test "required!/2 does not require attribute keys when value is non-nil" do
    # Contract: required! only reads the attribute metadata when value is nil.
    # If the value is non-nil, it should return the value without raising.
    assert {:known, "hello"} =
             RequiredError.evaluate(%{arguments: ["hello", %{resource: MyApp.Post}]})
  end

  test "required!/2 accepts attribute maps with string name key" do
    attribute = %{"name" => :title, resource: MyApp.Post}

    assert {:error, %Ash.Error.Changes.Required{} = err} =
             RequiredError.evaluate(%{arguments: [nil, attribute]})

    assert err.field == :title
    assert err.resource == MyApp.Post
  end

  test "required!/2 raises when attribute is missing :resource" do
    attribute = %{name: :title}

    assert_raise RuntimeError, ~r/attribute must have :resource for required!/, fn ->
      RequiredError.evaluate(%{arguments: [nil, attribute]})
    end
  end

  test "required!/2 raises when attribute is missing :name and \"name\"" do
    attribute = %{resource: MyApp.Post}

    assert_raise RuntimeError, ~r/attribute must have :name for required!/, fn ->
      RequiredError.evaluate(%{arguments: [nil, attribute]})
    end
  end

  test "required!/2 new/1 validates arguments" do
    assert {:error, "required! expects (value, attribute)"} = RequiredError.new([])
    assert {:error, "required! expects (value, attribute)"} = RequiredError.new([:only_one])
  end

  test "required!/2 metadata helpers are correct" do
    assert RequiredError.can_return_nil?(%RequiredError{}) == false
    assert RequiredError.evaluate_nil_inputs?() == true
  end
end
