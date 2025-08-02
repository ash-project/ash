defmodule Mix.Tasks.Ash.Gen.ResourcesTest do
  use ExUnit.Case
  import Igniter.Test

  @moduletag :igniter

  test "generates complete blog system with different resource configurations" do
    test_project()
    |> Igniter.compose_task("ash.gen.resources", [
      "MyApp.Blog.Post --uuid-primary-key id --attribute title:string:required:public,body:text:public,status:atom --relationship belongs_to:author:MyApp.Accounts.User:required --default-actions read,create,update --timestamps;MyApp.Blog.Comment --attribute content:text:required:public --relationship belongs_to:post:MyApp.Blog.Post:required,belongs_to:author:MyApp.Accounts.User:required --default-actions read,create;MyApp.Accounts.User --uuid-primary-key id --attribute name:string:required:public,email:string:required:public --default-actions read,create,update"
    ])
    |> assert_creates("lib/my_app/blog/post.ex", """
    defmodule MyApp.Blog.Post do
      use Ash.Resource,
        otp_app: :test,
        domain: MyApp.Blog

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
      end

      actions do
        defaults([:read, create: [:title, :body], update: [:title, :body]])
      end
    end
    """)
    |> assert_creates("lib/my_app/blog/comment.ex", """
    defmodule MyApp.Blog.Comment do
      use Ash.Resource,
        otp_app: :test,
        domain: MyApp.Blog

      attributes do
        attribute :content, :text do
          allow_nil?(false)
          public?(true)
        end
      end

      relationships do
        belongs_to :post, MyApp.Blog.Post do
          allow_nil?(false)
        end

        belongs_to :author, MyApp.Accounts.User do
          allow_nil?(false)
        end
      end

      actions do
        defaults([:read, create: [:content]])
      end
    end
    """)
    |> assert_creates("lib/my_app/accounts/user.ex", """
    defmodule MyApp.Accounts.User do
      use Ash.Resource,
        otp_app: :test,
        domain: MyApp.Accounts

      attributes do
        uuid_primary_key(:id)

        attribute :name, :string do
          allow_nil?(false)
          public?(true)
        end

        attribute :email, :string do
          allow_nil?(false)
          public?(true)
        end
      end

      actions do
        defaults([:read, create: [:name, :email], update: [:name, :email]])
      end
    end
    """)
    |> assert_creates("lib/my_app/blog.ex", """
    defmodule MyApp.Blog do
      use Ash.Domain,
        otp_app: :test

      resources do
        resource(MyApp.Blog.Post)
        resource(MyApp.Blog.Comment)
      end
    end
    """)
    |> assert_creates("lib/my_app/accounts.ex", """
    defmodule MyApp.Accounts do
      use Ash.Domain,
        otp_app: :test

      resources do
        resource(MyApp.Accounts.User)
      end
    end
    """)
  end
end
