# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.ByteSizeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.ByteSize

  defmodule Post do
    use Ash.Resource, domain: Ash.Test.Domain

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :body, :string do
        public?(true)
      end
    end
  end

  describe "min byte size" do
    test "validate success" do
      {:ok, opts} = ByteSize.init(attribute: :body, min: 4)
      changeset = Post |> Ash.Changeset.for_create(:create, %{body: "🔥"})

      assert :ok = ByteSize.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = ByteSize.init(attribute: :body, min: 4)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: "yes"})

      assert_error(changeset, opts, "must have byte size of at least 4")
    end
  end

  describe "max byte size" do
    test "validate success" do
      {:ok, opts} = ByteSize.init(attribute: :body, max: 4)
      changeset = Post |> Ash.Changeset.for_create(:create, %{body: "🔥"})

      assert :ok = ByteSize.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = ByteSize.init(attribute: :body, max: 3)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: "🔥"})

      assert_error(changeset, opts, "must have byte size of no more than 3")
    end
  end

  describe "exact byte size" do
    test "validate success" do
      {:ok, opts} = ByteSize.init(attribute: :body, exact: 4)
      changeset = Post |> Ash.Changeset.for_create(:create, %{body: "🔥"})

      assert :ok = ByteSize.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = ByteSize.init(attribute: :body, exact: 4)

      changeset = Ash.Changeset.for_create(Post, :create, %{body: "yes"})
      assert_error(changeset, opts, "must have byte size of exactly 4")

      changeset = Ash.Changeset.for_create(Post, :create, %{body: "🔥a"})
      assert_error(changeset, opts, "must have byte size of exactly 4")
    end
  end

  describe "min and max byte size" do
    test "validate success" do
      {:ok, opts} = ByteSize.init(attribute: :body, min: 3, max: 4)
      changeset = Post |> Ash.Changeset.for_create(:create, %{body: "🔥"})

      assert :ok = ByteSize.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = ByteSize.init(attribute: :body, min: 3, max: 4)

      changeset = Ash.Changeset.for_create(Post, :create, %{body: "no"})
      assert_error(changeset, opts, "must have byte size of between 3 and 4")

      changeset = Ash.Changeset.for_create(Post, :create, %{body: "🔥a"})
      assert_error(changeset, opts, "must have byte size of between 3 and 4")
    end
  end

  defp assert_error(changeset, opts, expected_message) do
    {:error, %{message: message, vars: vars}} = ByteSize.validate(changeset, opts, %{})
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
