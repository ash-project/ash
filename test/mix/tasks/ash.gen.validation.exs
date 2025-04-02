defmodule Mix.Tasks.Ash.Gen.ValidationTest do
  use ExUnit.Case
  import Igniter.Test

  @moduletag :igniter

  test "generates a basic custom validation module" do
    test_project()
    |> Igniter.compose_task("ash.gen.validation", ["MyApp.Validations.MyValidation"])
    |> assert_creates("lib/my_app/validations/my_validation.ex", """
    defmodule MyApp.Validations.MyValidation do
      use Ash.Resource.Validation

      @impl true
      def init(opts) do
        {:ok, opts}
      end

      @impl true
      def validation(_changeset, _opts, _context) do
        :ok
      end
    end
    """)
  end
end
