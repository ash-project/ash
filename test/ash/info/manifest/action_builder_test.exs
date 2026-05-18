# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.Generator.ActionBuilderTest do
  use ExUnit.Case, async: true

  alias Ash.Info.Manifest.{Action, Argument, Pagination}
  alias Ash.Info.Manifest.Generator.ActionBuilder

  defp get_action(resource, action_name) do
    Ash.Resource.Info.action(resource, action_name)
  end

  describe "build/2" do
    test "builds read action" do
      action = get_action(Ash.Test.Manifest.Todo, :read)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert %Action{} = result
      assert result.name == :read
      assert result.type == :read
      assert result.primary? == true
    end

    test "builds create action with accepted attribute inputs" do
      action = get_action(Ash.Test.Manifest.Todo, :create)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert result.type == :create
      assert is_list(result.inputs)
      assert Enum.any?(result.inputs, &(&1.name == :title))
    end

    test "builds action with public arguments only" do
      action = get_action(Ash.Test.Manifest.Todo, :read)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert is_list(result.inputs)
      assert Enum.all?(result.inputs, &is_atom(&1.name))
    end

    test "inputs merge accepted attributes and arguments" do
      action = get_action(Ash.Test.Manifest.Todo, :create)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      names = Enum.map(result.inputs, & &1.name)
      assert :title in names
      assert :user_id in names
      assert :auto_complete in names
    end

    test "builds argument with type resolution" do
      action = get_action(Ash.Test.Manifest.Todo, :create)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      user_id_arg = Enum.find(result.inputs, &(&1.name == :user_id))

      if user_id_arg do
        assert %Argument{} = user_id_arg
        assert user_id_arg.allow_nil? == false
        assert user_id_arg.type.kind == :uuid
      end
    end

    test "builds read action with pagination" do
      action = get_action(Ash.Test.Manifest.Todo, :read)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert %Pagination{} = result.pagination
      assert result.pagination.offset? == true
      assert result.pagination.keyset? == true
      assert result.pagination.countable? == true
      assert result.pagination.required? == false
      assert result.pagination.default_limit == 20
      assert result.pagination.max_page_size == 100
    end

    test "builds read action without pagination" do
      action = get_action(Ash.Test.Manifest.Todo, :list_high_priority)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert result.pagination == nil
    end

    test "builds generic action with returns type" do
      action = get_action(Ash.Test.Manifest.Todo, :bulk_complete)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert result.type == :action
      assert result.returns != nil
      assert result.returns.kind == :array
      assert result.returns.item_type.kind == :uuid
    end

    test "builds generic action with map return type" do
      action = get_action(Ash.Test.Manifest.Todo, :get_statistics)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert result.type == :action
      assert result.returns != nil
      assert result.returns.kind == :map
      assert is_list(result.returns.fields)
    end

    test "builds destroy action" do
      action = get_action(Ash.Test.Manifest.Todo, :destroy)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert result.type == :destroy
    end

    test "builds action with metadata" do
      action = get_action(Ash.Test.Manifest.Task, :read_with_metadata)

      if action do
        result = ActionBuilder.build(Ash.Test.Manifest.Task, action)
        assert is_list(result.metadata)
      end
    end

    test "get? action has get? set to true" do
      action = get_action(Ash.Test.Manifest.Todo, :get_by_id)
      result = ActionBuilder.build(Ash.Test.Manifest.Todo, action)

      assert result.get? == true
    end
  end
end
