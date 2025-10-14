# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Gen.ChangeTest do
  use ExUnit.Case
  import Igniter.Test

  @moduletag :igniter

  test "generates a basic custom change module" do
    test_project()
    |> Igniter.compose_task("ash.gen.change", ["MyApp.Changes.MyChange"])
    |> assert_creates("lib/my_app/changes/my_change.ex", """
    defmodule MyApp.Changes.MyChange do
      use Ash.Resource.Change

      @impl true
      def init(opts) do
        {:ok, opts}
      end

      @impl true
      def change(changeset, _opts, _context) do
        changeset
      end
    end
    """)
  end
end
