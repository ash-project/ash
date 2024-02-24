defmodule Ash.Test.Resource.Validation.StringLengthTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.StringLength

  defmodule Post do
    use Ash.Resource, domain: Ash.Test.Domain

    attributes do
      uuid_primary_key :id
      attribute :body, :string
    end
  end

  describe "min length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 3)
      changeset = Post |> Ash.Changeset.new(%{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 3)
      changeset = Ash.Changeset.new(Post, %{body: "no"})

      assert_error(changeset, opts, "must have length of at least 3")
    end
  end

  describe "max length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 3)
      changeset = Post |> Ash.Changeset.new(%{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 3)
      changeset = Ash.Changeset.new(Post, %{body: "invalid"})

      assert_error(changeset, opts, "must have length of no more than 3")
    end
  end

  describe "exact length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, exact: 3)
      changeset = Post |> Ash.Changeset.new(%{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, exact: 3)

      changeset = Ash.Changeset.new(Post, %{body: "no"})
      assert_error(changeset, opts, "must have length of exactly 3")

      changeset = Ash.Changeset.new(Post, %{body: "invalid"})
      assert_error(changeset, opts, "must have length of exactly 3")
    end
  end

  describe "min and max length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 2, max: 4)
      changeset = Post |> Ash.Changeset.new(%{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 2, max: 4)

      changeset = Ash.Changeset.new(Post, %{body: "n"})
      assert_error(changeset, opts, "must have length of between 2 and 4")

      changeset = Ash.Changeset.new(Post, %{body: "invalid"})
      assert_error(changeset, opts, "must have length of between 2 and 4")
    end
  end

  defp assert_error(changeset, opts, expected_message) do
    {:error, %{message: message, vars: vars}} = StringLength.validate(changeset, opts, %{})
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
