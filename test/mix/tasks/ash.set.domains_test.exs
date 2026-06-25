# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Set.DomainsTest do
  use ExUnit.Case, async: true
  import Igniter.Test

  test "branch 1: leaves file unchanged when no domains are discovered" do
    igniter = test_project()

    igniter = Mix.Tasks.Ash.Set.Domains.igniter(igniter, [])

    assert igniter.issues == []
    assert igniter.tasks == []
  end

  test "branch 2: successfully scans AST and patches config.exs with discovered domains" do
    igniter = test_project()

    igniter =
      igniter
      |> Igniter.Project.Module.create_module(
        Test.Team,
        """
        use Ash.Domain
        """,
        path: "lib/test/team.ex"
      )
      |> Igniter.Project.Module.create_module(
        Test.User,
        """
        use Ash.Domain
        """,
        path: "lib/test/user.ex"
      )
      |> Igniter.include_all_elixir_files()

    igniter = Mix.Tasks.Ash.Set.Domains.igniter(igniter, [])

    assert_has_patch(igniter, "config/config.exs", """
    |import Config
    |config :test, ash_domains: [Test.Team, Test.User]
    |
    """)
  end
end
