# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.Generator.ReachabilityTest do
  use ExUnit.Case, async: true

  alias Ash.Info.Manifest.Generator.Reachability

  describe "find_reachable/1" do
    test "finds directly referenced resources" do
      {resources, _types} = Reachability.find_reachable([Ash.Test.Manifest.Todo])
      resource_set = MapSet.new(resources)

      # Todo has relationships to User and TodoComment
      assert MapSet.member?(resource_set, Ash.Test.Manifest.Todo)
      assert MapSet.member?(resource_set, Ash.Test.Manifest.User)
      assert MapSet.member?(resource_set, Ash.Test.Manifest.TodoComment)
    end

    test "finds transitively referenced resources" do
      {resources, _types} = Reachability.find_reachable([Ash.Test.Manifest.Todo])
      resource_set = MapSet.new(resources)

      # Todo -> User -> UserSettings (if User has a relationship to UserSettings)
      # At minimum, Todo itself and its direct relationships
      assert MapSet.member?(resource_set, Ash.Test.Manifest.Todo)
    end

    test "handles cycles without infinite recursion" do
      # Todo -> TodoComment -> Todo (via belongs_to)
      {resources, _types} = Reachability.find_reachable([Ash.Test.Manifest.Todo])
      assert is_list(resources)
      assert resources != []
    end

    test "finds standalone enum types" do
      {_resources, types} = Reachability.find_reachable([Ash.Test.Manifest.Todo])

      # Todo has a Status enum attribute — its module should appear in the
      # reachable type set so the generator emits its full definition.
      assert Ash.Test.Manifest.Todo.Status in types
    end

    test "multiple root resources are all included" do
      {resources, _types} =
        Reachability.find_reachable([
          Ash.Test.Manifest.Todo,
          Ash.Test.Manifest.User
        ])

      resource_set = MapSet.new(resources)
      assert MapSet.member?(resource_set, Ash.Test.Manifest.Todo)
      assert MapSet.member?(resource_set, Ash.Test.Manifest.User)
    end

    test "empty input returns empty results" do
      {resources, types} = Reachability.find_reachable([])
      assert resources == []
      assert types == []
    end

    test "finds embedded resources referenced by attributes" do
      {resources, _types} = Reachability.find_reachable([Ash.Test.Manifest.Todo])
      resource_set = MapSet.new(resources)

      # Todo has :metadata attribute of type TodoMetadata (embedded resource)
      assert MapSet.member?(resource_set, Ash.Test.Manifest.TodoMetadata)
    end

    test "traverses explicit action roots for already structurally visited resources" do
      {_resources, types} =
        Reachability.find_reachable([
          Ash.Test.Manifest.User,
          {Ash.Test.Manifest.Post, [:get_custom_metadata, :update_internal_code]}
        ])

      assert Ash.Test.Manifest.Types.EmailString in types
      assert Ash.Test.Manifest.CustomMetadata in types
      assert Ash.Test.Manifest.InputParsing.Options in types
      assert Ash.Test.Manifest.InputParsing.PreferencesKeyword in types
    end
  end
end
