# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.ExtendTest do
  use ExUnit.Case
  import Igniter.Test
  @moduletag :igniter

  test "ets extension can be added" do
    test_project()
    |> Igniter.compose_task("ash.gen.resource", ["MyApp.Blog.Post"])
    |> apply_igniter!()
    |> Igniter.compose_task("ash.extend", ["MyApp.Blog.Post", "ets"])
    |> assert_has_patch("lib/my_app/blog/post.ex", """
    - |  use Ash.Resource,
    - |    otp_app: :test,
    - |    domain: MyApp.Blog
    + |  use Ash.Resource, otp_app: :test, domain: MyApp.Blog, data_layer: Ash.DataLayer.Ets
    """)
  end
end
