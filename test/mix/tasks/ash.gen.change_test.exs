defmodule Mix.Tasks.Ash.Gen.ChangeTest do
  use ExUnit.Case
  import Igniter.Test

  @moduletag :igniter

  test "generates a basic custom change module" do
    test_project()
    |> Igniter.compose_task("ash.gen.change", ["MyApp.Changes.Slugify"])
    |> assert_creates("lib/my_app/changes/slugify.ex", """
    defmodule MyApp.Changes.Slugify do
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
