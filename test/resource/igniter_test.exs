defmodule Ash.Resource.IgniterTest do
  use ExUnit.Case

  import Igniter.Test

  describe "add_new_attribute" do
    test "adds an attribute if it doesnt exists" do
      test_project()
      |> Igniter.Project.Module.create_module(MyApp.User, """
      use Ash.Resource
      """)
      |> apply_igniter!()
      |> Ash.Resource.Igniter.add_new_attribute(MyApp.User, :name, """
      attribute :name, :string, allow_nil?: true
      """)
      |> assert_has_patch("lib/my_app/user.ex", """
      + | attribute(:name, :string, allow_nil?: true)
      """)
    end

    test "doesnt an attribute if it exists" do
      test_project()
      |> Igniter.Project.Module.create_module(MyApp.User, """
      use Ash.Resource

      attributes do
        attribute :name, :string, allow_nil?: false
      end
      """)
      |> apply_igniter!()
      |> Ash.Resource.Igniter.add_new_attribute(MyApp.User, :name, """
      attribute :name, :string, allow_nil?: true
      """)
      |> assert_unchanged()
    end

    test "doesnt add an attribute if it exists in a fragment" do
      test_project()
      |> Igniter.Project.Module.create_module(MyApp.User, """
      use Ash.Resource, fragments: [MyApp.User.Attributes]
      """)
      |> Igniter.Project.Module.create_module(MyApp.User.Attributes, """
      use Spark.Dsl.Fragment, of: Ash.Resource

      attributes do
        attribute :name, :string, allow_nil?: false
      end
      """)
      |> apply_igniter!()
      |> Ash.Resource.Igniter.add_new_attribute(MyApp.User, :name, """
      attribute :name, :string, allow_nil?: true
      """)
      |> assert_unchanged("lib/my_app/user.ex")
    end
  end

  describe "add_new_action" do
    test "add_new_action adds an attribute if it doesnt exists" do
      test_project()
      |> Igniter.Project.Module.create_module(MyApp.User, """
      use Ash.Resource
      """)
      |> apply_igniter!()
      |> Ash.Resource.Igniter.add_new_action(MyApp.User, :create, """
        create :create do
        end
      """)
      |> assert_has_patch("lib/my_app/user.ex", """
      + | create :create
      """)
    end

    test "add_new_action doesnt an attribute if it exists" do
      test_project()
      |> Igniter.Project.Module.create_module(MyApp.User, """
      use Ash.Resource

      actions do
        create :create do
        end
      end
      """)
      |> apply_igniter!()
      |> Ash.Resource.Igniter.add_new_action(MyApp.User, :create, """
      create :create do
      end
      """)
      |> assert_unchanged()
    end

    test "add_new_action doesnt add an action if it exists in a fragment" do
      test_project()
      |> Igniter.Project.Module.create_module(MyApp.User, """
      use Ash.Resource, fragments: [MyApp.User.Actions]
      """)
      |> Igniter.Project.Module.create_module(MyApp.User.Actions, """
      use Spark.Dsl.Fragment, of: Ash.Resource

      actions do
        create :create do
        end
      end
      """)
      |> apply_igniter!()
      |> Ash.Resource.Igniter.add_new_action(MyApp.User, :create, """
      create :create do
      end
      """)
      |> assert_unchanged()
    end
  end
end
