# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

      action :test_set_context, :string do
        argument :input, :string, allow_nil?: false

        prepare set_context(%{test_context: true})

        run fn input, _ ->
          if input.context[:test_context] do
            {:ok, "context_set"}
          else
            {:ok, "no_context"}
          end
        end
      end

      action :test_before_action, :string do
        argument :input, :string, allow_nil?: false

        prepare before_action(fn input, _context ->
                  Ash.ActionInput.set_argument(input, :input, "modified_#{input.arguments.input}")
                end)

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_after_action, :string do
        argument :input, :string, allow_nil?: false

        prepare after_action(fn _input, result, _context ->
                  {:ok, "after_#{result}"}
                end)

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_function_preparation, :string do
        argument :input, :string, allow_nil?: false

        prepare fn input, _context ->
          Ash.ActionInput.set_argument(input, :input, "function_#{input.arguments.input}")
        end

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_action_is_validation, :string do
        argument :input, :string, allow_nil?: false

        validate action_is(:test_action_is_validation)

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_argument_equals_validation, :string do
        argument :status, :string, allow_nil?: false
        argument :input, :string, allow_nil?: false

        validate argument_equals(:status, "active")

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_argument_in_validation, :string do
        argument :role, :string, allow_nil?: false
        argument :input, :string, allow_nil?: false

        validate argument_in(:role, ["admin", "user", "moderator"])

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_present_validation, :string do
        argument :required_field, :string, allow_nil?: true
        argument :input, :string, allow_nil?: false

        validate present(:required_field)

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_match_validation, :string do
        argument :email, :string, allow_nil?: false
        argument :input, :string, allow_nil?: false

        validate match(:email, ~r/@/)

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_confirm_validation, :string do
        argument :password, :string, allow_nil?: false
        argument :password_confirmation, :string, allow_nil?: false
        argument :input, :string, allow_nil?: false

        validate confirm(:password, :password_confirmation)

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end

      action :test_compare_validation, :string do
        argument :age, :integer, allow_nil?: false
        argument :input, :string, allow_nil?: false

        validate compare(:age, greater_than: 18)

        run fn input, _ ->
          {:ok, input.arguments.input}
        end
      end
    end
  end

  # doctest Ash.ActionInput

  describe "for_action/4" do
    test "when the action doesn't exist, it raises an appropriate error" do
      assert_raise(Ash.Error.Invalid, ~r/no such action/i, fn ->
        Post
        |> Ash.ActionInput.new()
        |> Ash.ActionInput.for_action(:invalid_action_name, %{})
      end)
    end
  end

  describe "preparations" do
    test "set_context preparation works with generic actions" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_set_context, %{input: "test"})
        |> Ash.run_action!()

      assert result == "context_set"
    end

    test "before_action preparation works with generic actions" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_before_action, %{input: "test"})
        |> Ash.run_action!()

      assert result == "modified_test"
    end

    test "after_action preparation works with generic actions" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_after_action, %{input: "test"})
        |> Ash.run_action!()

      assert result == "after_test"
    end

    test "function preparation works with generic actions" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_function_preparation, %{input: "test"})
        |> Ash.run_action!()

      assert result == "function_test"
    end
  end

  describe "validations" do
    test "action_is validation works with generic actions" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_action_is_validation, %{input: "test"})
        |> Ash.run_action!()

      assert result == "test"
    end

    test "argument_equals validation works with generic actions - success case" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_argument_equals_validation, %{
          status: "active",
          input: "test"
        })
        |> Ash.run_action!()

      assert result == "test"
    end

    test "argument_equals validation works with generic actions - failure case" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:test_argument_equals_validation, %{
          status: "inactive",
          input: "test"
        })
        |> Ash.run_action!()
      end
    end

    test "argument_in validation works with generic actions - success case" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_argument_in_validation, %{
          role: "admin",
          input: "test"
        })
        |> Ash.run_action!()

      assert result == "test"
    end

    test "argument_in validation works with generic actions - failure case" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:test_argument_in_validation, %{
          role: "invalid",
          input: "test"
        })
        |> Ash.run_action!()
      end
    end

    test "present validation works with generic actions - success case" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_present_validation, %{
          required_field: "present",
          input: "test"
        })
        |> Ash.run_action!()

      assert result == "test"
    end

    test "present validation works with generic actions - failure case" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:test_present_validation, %{input: "test"})
        |> Ash.run_action!()
      end
    end

    test "match validation works with generic actions - success case" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_match_validation, %{
          email: "test@example.com",
          input: "test"
        })
        |> Ash.run_action!()

      assert result == "test"
    end

    test "match validation works with generic actions - failure case" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:test_match_validation, %{
          email: "invalid-email",
          input: "test"
        })
        |> Ash.run_action!()
      end
    end

    test "confirm validation works with generic actions - success case" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_confirm_validation, %{
          password: "secret",
          password_confirmation: "secret",
          input: "test"
        })
        |> Ash.run_action!()

      assert result == "test"
    end

    test "confirm validation works with generic actions - failure case" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:test_confirm_validation, %{
          password: "secret",
          password_confirmation: "different",
          input: "test"
        })
        |> Ash.run_action!()
      end
    end

    test "compare validation works with generic actions - success case" do
      result =
        Post
        |> Ash.ActionInput.for_action(:test_compare_validation, %{age: 25, input: "test"})
        |> Ash.run_action!()

      assert result == "test"
    end

    test "compare validation works with generic actions - failure case" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:test_compare_validation, %{age: 15, input: "test"})
        |> Ash.run_action!()
      end
    end
  end
end
