defmodule Ash.Test.Query.RequiredErrorTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.RequiredError

  test "ash_required!/2 returns error when value is nil" do
    attribute = %{name: :title, resource: MyApp.Post}

    assert {:error, %Ash.Error.Changes.Required{} = err} =
             RequiredError.evaluate(%{arguments: [nil, attribute]})

    assert err.field == :title
    assert err.resource == MyApp.Post
    assert err.type == :attribute
  end

  test "ash_required!/2 returns value when non-nil" do
    attribute = %{name: :title, resource: MyApp.Post}

    assert {:known, "hello"} =
             RequiredError.evaluate(%{arguments: ["hello", attribute]})
  end

  test "ash_required!/2 accepts attribute maps with string name key" do
    attribute = %{"name" => :title, resource: MyApp.Post}

    assert {:error, %Ash.Error.Changes.Required{} = err} =
             RequiredError.evaluate(%{arguments: [nil, attribute]})

    assert err.field == :title
    assert err.resource == MyApp.Post
  end

  test "ash_required!/2 raises when attribute is missing :resource" do
    attribute = %{name: :title}

    assert_raise RuntimeError, ~r/attribute must have :resource for ash_required!/, fn ->
      RequiredError.evaluate(%{arguments: [nil, attribute]})
    end
  end

  test "ash_required!/2 raises when attribute is missing :name and \"name\"" do
    attribute = %{resource: MyApp.Post}

    assert_raise RuntimeError, ~r/attribute must have :name for ash_required!/, fn ->
      RequiredError.evaluate(%{arguments: [nil, attribute]})
    end
  end

  test "ash_required!/2 new/1 validates arguments" do
    assert {:error, "ash_required! expects (value, attribute)"} = RequiredError.new([])
    assert {:error, "ash_required! expects (value, attribute)"} = RequiredError.new([:only_one])
  end

  test "ash_required!/2 metadata helpers are correct" do
    assert RequiredError.can_return_nil?(%RequiredError{}) == false
    assert RequiredError.evaluate_nil_inputs?() == true
  end
end

