defmodule Mix.Tasks.Ash.Gen.ResourceTest do
  use ExUnit.Case
  import Igniter.Test

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
end
