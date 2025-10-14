# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.IgniterTest do
  use ExUnit.Case

  import Igniter.Test

  @moduletag :igniter

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

  describe "add_new_code_interface" do
    setup do
      igniter =
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Accounts.User",
          "--domain",
          "MyApp.Accounts",
          "--default-actions",
          "create,read,update,destroy",
          "--uuid-primary-key",
          "id",
          "--attribute",
          "name:string:required:public"
        ])
        |> apply_igniter!()

      # Register the domain in Application config for this test.
      # The list_domains/1 optimization uses compiled domains from config + changed sources.
      # Since apply_igniter! writes files without marking them as changed, the domain
      # won't be found unless we add it to the config here.
      app_name = Igniter.Project.Application.app_name(igniter)
      existing_domains = Application.get_env(app_name, :ash_domains, [])
      Application.put_env(app_name, :ash_domains, [MyApp.Accounts | existing_domains])

      on_exit(fn ->
        # Clean up - restore original config
        Application.put_env(app_name, :ash_domains, existing_domains)
      end)

      %{igniter: igniter}
    end

    test "add_new_code_interface adds a new code interface to existing resource", %{
      igniter: igniter
    } do
      igniter
      |> Ash.Domain.Igniter.add_new_code_interface(
        MyApp.Accounts,
        MyApp.Accounts.User,
        :create,
        "define :create, action: :create"
      )
      |> assert_has_patch("lib/my_app/accounts.ex", """
      - |    resource(MyApp.Accounts.User)
      + |    resource(MyApp.Accounts.User) do
      + |      define(:create, action: :create)
      + |    end
      """)
    end

    test "add_new_code_interface appends a new code interface to existing resource with code interface",
         %{
           igniter: igniter
         } do
      igniter =
        igniter
        |> Ash.Domain.Igniter.add_new_code_interface(
          MyApp.Accounts,
          MyApp.Accounts.User,
          :create,
          "define :create, action: :create"
        )
        |> apply_igniter!()

      igniter
      |> Ash.Domain.Igniter.add_new_code_interface(
        MyApp.Accounts,
        MyApp.Accounts.User,
        :update,
        "define :update, action: :update"
      )
      |> assert_has_patch("lib/my_app/accounts.ex", """
        |    resource(MyApp.Accounts.User) do
        |      define(:create, action: :create)
      + |      define(:update, action: :update)
        |    end
      """)
    end

    test "add_new_code_interface appends doesnt add code interface when one already exists",
         %{
           igniter: igniter
         } do
      igniter =
        igniter
        |> Ash.Domain.Igniter.add_new_code_interface(
          MyApp.Accounts,
          MyApp.Accounts.User,
          :create,
          "define :create, action: :create"
        )
        |> apply_igniter!()

      igniter
      |> Ash.Domain.Igniter.add_new_code_interface(
        MyApp.Accounts,
        MyApp.Accounts.User,
        :create,
        "define :create, action: :create"
      )
      |> assert_unchanged()
    end
  end
end
