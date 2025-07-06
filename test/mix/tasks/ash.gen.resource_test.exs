defmodule Mix.Tasks.Ash.Gen.ResourceTest do
  use ExUnit.Case
  import Igniter.Test

  @moduletag :igniter

  describe "basic resource generation" do
    test "generates a basic resource" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post"])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog
      end
      """)
    end

    test "generates a resource with custom domain" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post", "--domain", "MyApp.Content"])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Content
      end
      """)
    end
  end

  describe "attribute generation" do
    test "generates simple attributes" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--attribute",
        "title:string,body:text"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          attribute(:title, :string)
          attribute(:body, :text)
        end
      end
      """)
    end

    test "generates attributes with modifiers" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--attribute",
        "title:string:required:public,secret:string:sensitive"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          attribute :title, :string do
            allow_nil?(false)
            public?(true)
          end

          attribute :secret, :string do
            sensitive?(true)
          end
        end
      end
      """)
    end

    test "generates attributes with custom types" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--attribute",
        "metadata:map,tags:MyApp.Types.TagList"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          attribute(:metadata, :map)
          attribute(:tags, MyApp.Types.TagList)
        end
      end
      """)
    end
  end

  describe "primary key generation" do
    test "generates uuid primary key" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post", "--uuid-primary-key", "id"])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          uuid_primary_key(:id)
        end
      end
      """)
    end

    test "generates uuid primary key with modifiers" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--uuid-primary-key",
        "id:public"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          uuid_primary_key :id do
            public?(true)
          end
        end
      end
      """)
    end

    test "generates uuid v7 primary key" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--uuid-v7-primary-key",
        "id"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          uuid_v7_primary_key(:id)
        end
      end
      """)
    end

    test "generates integer primary key" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--integer-primary-key",
        "id"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          integer_primary_key(:id)
        end
      end
      """)
    end
  end

  describe "relationship generation" do
    test "generates belongs_to relationship" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--relationship",
        "belongs_to:author:MyApp.Accounts.User"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        relationships do
          belongs_to(:author, MyApp.Accounts.User)
        end
      end
      """)
    end

    test "generates belongs_to relationship with modifiers" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--relationship",
        "belongs_to:author:MyApp.Accounts.User:required:public"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        relationships do
          belongs_to :author, MyApp.Accounts.User do
            allow_nil?(false)
            public?(true)
          end
        end
      end
      """)
    end

    test "generates has_many relationship" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--relationship",
        "has_many:comments:MyApp.Blog.Comment:public"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        relationships do
          has_many :comments, MyApp.Blog.Comment do
            public?(true)
          end
        end
      end
      """)
    end

    test "generates multiple relationships" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--relationship",
        "belongs_to:author:MyApp.Accounts.User,has_many:comments:MyApp.Blog.Comment"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        relationships do
          belongs_to(:author, MyApp.Accounts.User)
          has_many(:comments, MyApp.Blog.Comment)
        end
      end
      """)
    end
  end

  describe "default actions" do
    test "generates default read action" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post", "--default-actions", "read"])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        actions do
          defaults([:read])
        end
      end
      """)
    end

    test "generates default create and update actions with public attributes" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--attribute",
        "title:string:public,body:text:public",
        "--default-actions",
        "create,update"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        actions do
          defaults(create: [:title, :body], update: [:title, :body])
        end

        attributes do
          attribute :title, :string do
            public?(true)
          end

          attribute :body, :text do
            public?(true)
          end
        end
      end
      """)
    end

    test "generates all default actions" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--default-actions",
        "read,create,update,destroy"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        actions do
          defaults([:read, :destroy, create: [], update: []])
        end
      end
      """)
    end
  end

  describe "timestamps" do
    test "generates timestamps" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post", "--timestamps"])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          timestamps()
        end
      end
      """)
    end

    test "generates timestamps with other attributes" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--attribute",
        "title:string",
        "--timestamps"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        attributes do
          attribute(:title, :string)
          timestamps()
        end
      end
      """)
    end
  end

  describe "base resources" do
    test "generates resource with custom base" do
      test_project(app_name: :my_app)
      |> Igniter.Project.Config.configure("config.exs", :my_app, [:base_resources], [
        MyApp.BaseResource
      ])
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--base",
        "MyApp.BaseResource"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use MyApp.BaseResource,
          otp_app: :my_app,
          domain: MyApp.Blog
      end
      """)
    end

    test "raises error when base resource not in config" do
      assert_raise RuntimeError,
                   ~r/The base module MyApp.BaseResource is not in the list of base resources/,
                   fn ->
                     test_project()
                     |> Igniter.compose_task("ash.gen.resource", [
                       "MyApp.Blog.Post",
                       "--base",
                       "MyApp.BaseResource"
                     ])
                     |> apply_igniter!()
                   end
    end
  end

  describe "domain handling" do
    test "creates domain if it doesn't exist" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post"])
      |> assert_creates("lib/my_app/blog.ex")
      |> assert_creates("lib/my_app/blog.ex", """
      defmodule MyApp.Blog do
        use Ash.Domain,
          otp_app: :test

        resources do
          resource(MyApp.Blog.Post)
        end
      end
      """)
    end

    test "adds resource to existing domain" do
      test_project()
      |> Igniter.compose_task("ash.gen.domain", ["MyApp.Blog"])
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.ExistingResource"])
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post"])
      |> assert_creates("lib/my_app/blog.ex", """
      defmodule MyApp.Blog do
        use Ash.Domain,
          otp_app: :test

        resources do
          resource(MyApp.Blog.ExistingResource)
          resource(MyApp.Blog.Post)
        end
      end
      """)
    end
  end

  describe "ignore-if-exists option" do
    test "does nothing if resource exists and ignore-if-exists is set" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post"])
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post", "--ignore-if-exists"])
      |> assert_creates("lib/my_app/blog/post.ex")
    end

    test "creates resource if it doesn't exist even with ignore-if-exists" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post", "--ignore-if-exists"])
      |> assert_creates("lib/my_app/blog/post.ex")
    end
  end

  describe "complex resource generation" do
    test "generates resource with all options combined" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "--uuid-primary-key",
        "id",
        "--attribute",
        "title:string:required:public,body:text:public,status:atom",
        "--relationship",
        "belongs_to:author:MyApp.Accounts.User:required,has_many:comments:MyApp.Blog.Comment",
        "--default-actions",
        "read,create,update",
        "--timestamps"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Blog

        actions do
          defaults([:read, create: [:title, :body], update: [:title, :body]])
        end

        attributes do
          uuid_primary_key(:id)

          attribute :title, :string do
            allow_nil?(false)
            public?(true)
          end

          attribute :body, :text do
            public?(true)
          end

          attribute(:status, :atom)
          timestamps()
        end

        relationships do
          belongs_to :author, MyApp.Accounts.User do
            allow_nil?(false)
          end

          has_many(:comments, MyApp.Blog.Comment)
        end
      end
      """)
    end
  end

  describe "error handling" do
    test "raises error for invalid attribute format" do
      assert_raise RuntimeError, ~r/Invalid attribute format/, fn ->
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Blog.Post",
          "--attribute",
          "invalid_format"
        ])
        |> apply_igniter!()
      end
    end

    test "raises error for invalid attribute name" do
      assert_raise RuntimeError, ~r/Invalid attribute name provided/, fn ->
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Blog.Post",
          "--attribute",
          "123invalid:string"
        ])
        |> apply_igniter!()
      end
    end

    test "raises error for invalid relationship format" do
      assert_raise RuntimeError, ~r/Invalid relationship format/, fn ->
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Blog.Post",
          "--relationship",
          "invalid"
        ])
        |> apply_igniter!()
      end
    end

    test "raises error for invalid relationship name" do
      assert_raise RuntimeError, ~r/Invalid relationship name provided/, fn ->
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Blog.Post",
          "--relationship",
          "belongs_to:123invalid:MyApp.User"
        ])
        |> apply_igniter!()
      end
    end

    test "raises error for invalid default action type" do
      assert_raise RuntimeError, ~r/Invalid default action type/, fn ->
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Blog.Post",
          "--default-actions",
          "invalid_action"
        ])
        |> apply_igniter!()
      end
    end

    test "raises error for unknown attribute modifier" do
      assert_raise ArgumentError, ~r/Unrecognizeable attribute modifier/, fn ->
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Blog.Post",
          "--attribute",
          "title:string:unknown_modifier"
        ])
        |> apply_igniter!()
      end
    end

    test "raises error for primary_key modifier on non-belongs_to relationship" do
      assert_raise ArgumentError, ~r/only valid for belongs_to relationships/, fn ->
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Blog.Post",
          "--relationship",
          "has_many:posts:MyApp.Blog.Post:primary_key"
        ])
        |> apply_igniter!()
      end
    end

    test "raises error for required modifier on non-belongs_to relationship" do
      assert_raise ArgumentError, ~r/only valid for belongs_to relationships/, fn ->
        test_project()
        |> Igniter.compose_task("ash.gen.resource", [
          "MyApp.Blog.Post",
          "--relationship",
          "has_many:posts:MyApp.Blog.Post:required"
        ])
        |> apply_igniter!()
      end
    end
  end

  describe "alias usage" do
    test "supports short aliases" do
      test_project()
      |> Igniter.compose_task("ash.gen.resource", [
        "MyApp.Blog.Post",
        "-u",
        "id",
        "-a",
        "title:string",
        "-r",
        "belongs_to:author:MyApp.Accounts.User",
        "-d",
        "MyApp.Content",
        "-t"
      ])
      |> assert_creates("lib/my_app/blog/post.ex", """
      defmodule MyApp.Blog.Post do
        use Ash.Resource,
          otp_app: :test,
          domain: MyApp.Content

        attributes do
          uuid_primary_key(:id)

          attribute(:title, :string)
          timestamps()
        end

        relationships do
          belongs_to(:author, MyApp.Accounts.User)
        end
      end
      """)
    end
  end
end

