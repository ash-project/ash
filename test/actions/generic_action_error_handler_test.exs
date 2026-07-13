# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.GenericActionErrorHandlerTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read]

      action :with_fun_handler, :string do
        argument :name, :string, allow_nil?: false

        error_handler fn _input, _error ->
          Ash.Error.Unknown.UnknownError.exception(error: "handled by fun")
        end

        run fn input, _ -> {:ok, input.arguments.name} end
      end

      action :with_mfa_handler, :string do
        argument :name, :string, allow_nil?: false

        error_handler {__MODULE__, :replace_error, ["handled by mfa"]}

        run fn input, _ -> {:ok, input.arguments.name} end
      end

      action :with_ignoring_handler, :string do
        argument :name, :string, allow_nil?: false

        error_handler fn _input, _error -> :ignore end

        run fn input, _ -> {:ok, input.arguments.name} end
      end

      action :run_errors_with_fun_handler, :string do
        error_handler fn _input, error ->
          Ash.Error.Unknown.UnknownError.exception(error: "handled #{Exception.message(error)}")
        end

        run fn _input, _ -> {:error, "from the run"} end
      end

      action :raises_with_handler, :string do
        error_handler fn _input, error ->
          Ash.Error.Unknown.UnknownError.exception(error: "handled #{Exception.message(error)}")
        end

        run fn _input, _ -> raise "boom" end
      end
    end

    attributes do
      uuid_primary_key :id
    end

    def replace_error(_input, _error, message) do
      Ash.Error.Unknown.UnknownError.exception(error: message)
    end
  end

  test "a function error handler transforms errors raised during input building" do
    assert {:error, %Ash.Error.Unknown{errors: [error]}} =
             Post
             |> Ash.ActionInput.for_action(:with_fun_handler, %{})
             |> Ash.run_action()

    assert Exception.message(error) =~ "handled by fun"
  end

  test "an mfa error handler transforms errors raised during input building" do
    assert {:error, %Ash.Error.Unknown{errors: [error]}} =
             Post
             |> Ash.ActionInput.for_action(:with_mfa_handler, %{})
             |> Ash.run_action()

    assert Exception.message(error) =~ "handled by mfa"
  end

  test "returning :ignore discards the error but keeps the input invalid" do
    input = Ash.ActionInput.for_action(Post, :with_ignoring_handler, %{})

    refute input.valid?
    assert input.errors == []
  end

  test "errors returned from the run implementation are piped through the handler" do
    assert {:error, %Ash.Error.Unknown{errors: [error]}} =
             Post
             |> Ash.ActionInput.for_action(:run_errors_with_fun_handler, %{})
             |> Ash.run_action()

    assert Exception.message(error) =~ "handled"
    assert Exception.message(error) =~ "from the run"
  end

  test "exceptions raised in the run implementation are piped through the handler" do
    error =
      assert_raise Ash.Error.Unknown, fn ->
        Post
        |> Ash.ActionInput.for_action(:raises_with_handler, %{})
        |> Ash.run_action()
      end

    assert Exception.message(error) =~ "handled"
    assert Exception.message(error) =~ "boom"
  end
end
