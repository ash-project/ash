# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Gen.PreparationTest do
  use ExUnit.Case
  import Igniter.Test

  @moduletag :igniter

  test "generates a basic custom preparation module" do
    test_project()
    |> Igniter.compose_task("ash.gen.preparation", ["MyApp.Preparations.MyPreparation"])
    |> assert_creates("lib/my_app/preparations/my_preparation.ex", """
    defmodule MyApp.Preparations.MyPreparation do
      use Ash.Resource.Preparation

      @impl true
      def init(opts) do
        {:ok, opts}
      end

      @impl true
      def prepare(query, _opts, _context) do
        query
      end
    end
    """)
  end
end
