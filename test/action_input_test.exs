defmodule Ash.Test.ActionInput do
  @moduledoc false
  use ExUnit.Case, async: false
  alias Ash.Test.Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain

    actions do
      action :example, :boolean do
        argument :arg, :string, allow_nil?: true, public?: true
        argument :private_arg, :string, allow_nil?: true, public?: false

        run fn _, _ -> true end
      end
    end
  end

  doctest Ash.ActionInput

  describe "for_action/4" do
    test "when the action doesn't exist, it raises an appropriate error" do
      assert_raise(Ash.Error.Invalid, ~r/no such action/i, fn ->
        Post
        |> Ash.ActionInput.new()
        |> Ash.ActionInput.for_action(:invalid_action_name, %{})
      end)
    end
  end
end
