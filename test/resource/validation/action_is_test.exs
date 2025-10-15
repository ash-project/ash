# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.ActionIsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.ActionIs

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource, domain: Domain

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :status, :string, public?: true
    end
  end

  test "passes when action is equal to the one provided action" do
    changeset =
      Ash.Changeset.for_create(Post, :create)

    assert_validation_success(changeset, action: :create)
  end

  test "fails when action is equal to the one provided action" do
    changeset =
      Ash.Changeset.for_update(%Post{}, :update)

    assert_validation_error(changeset, [action: :create], "must be create")
  end

  test "passes when action is in list of provided actions" do
    changesets = [
      Ash.Changeset.for_create(Post, :create, %{status: "new"}),
      Ash.Changeset.for_update(%Post{}, :update, %{status: "new"})
    ]

    for changeset <- changesets do
      assert_validation_success(changeset, action: [:create, :update])
    end
  end

  test "fails when action is not in list of provided actions" do
    changeset = Ash.Changeset.for_destroy(%Post{}, :destroy)

    assert_validation_error(
      changeset,
      [action: [:create, :update]],
      "action must be one of create, update"
    )
  end

  defp assert_validation_success(changeset, opts) do
    assert :ok = ActionIs.validate(changeset, opts, %{})
  end

  defp assert_validation_error(changeset, opts, expected_message) do
    assert {:error, %Ash.Error.Unknown.UnknownError{error: message, vars: vars}} =
             ActionIs.validate(changeset, opts, %{})

    assert expected_message == translate_message(message, vars)
  end

  defp translate_message(message, vars) do
    Enum.reduce(vars, message, fn {key, value}, acc ->
      if String.contains?(acc, "%{#{key}}") do
        String.replace(acc, "%{#{key}}", to_string(value))
      else
        acc
      end
    end)
  end
end
