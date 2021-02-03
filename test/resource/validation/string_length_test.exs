defmodule Ash.Test.Resource.Validation.StringLengthTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.StringLength

  defmodule Post do
    use Ash.Resource

    attributes do
      uuid_primary_key :id
      attribute :body, :string
    end
  end

  describe "min length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 3)
      changeset = Post |> Ash.Changeset.new(%{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts)
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 3)
      changeset = Ash.Changeset.new(Post, %{body: "no"})

      assert_error(changeset, opts, "body must have length of at least 3")
    end

    test "validate failure with message" do
      {:ok, opts} =
        StringLength.init(
          attribute: :body,
          min: 3,
          message: "too short, minimum of %{min} characters"
        )

      changeset = Ash.Changeset.new(Post, %{body: "no"})

      assert_error(changeset, opts, "too short, minimum of 3 characters")
    end
  end

  describe "max length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 3)
      changeset = Post |> Ash.Changeset.new(%{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts)
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 3)
      changeset = Ash.Changeset.new(Post, %{body: "no way"})

      assert_error(changeset, opts, "body must have length of no more than 3")
    end

    test "validate failure with message" do
      {:ok, opts} =
        StringLength.init(
          attribute: :body,
          max: 3,
          message: "too long, maximum of %{max} characters"
        )

      changeset = Ash.Changeset.new(Post, %{body: "no way"})

      assert_error(changeset, opts, "too long, maximum of 3 characters")
    end
  end

  describe "exact length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, exact: 3)
      changeset = Post |> Ash.Changeset.new(%{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts)
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, exact: 3)

      changeset = Ash.Changeset.new(Post, %{body: "no"})
      assert_error(changeset, opts, "body must have length of exactly 3")

      changeset = Ash.Changeset.new(Post, %{body: "no way"})
      assert_error(changeset, opts, "body must have length of exactly 3")
    end

    test "validate failure with message" do
      {:ok, opts} =
        StringLength.init(
          attribute: :body,
          exact: 3,
          message: "must be %{exact} characters"
        )

      changeset = Ash.Changeset.new(Post, %{body: "no"})
      assert_error(changeset, opts, "must be 3 characters")

      changeset = Ash.Changeset.new(Post, %{body: "no way"})
      assert_error(changeset, opts, "must be 3 characters")
    end
  end

  describe "min and max length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 2, max: 4)
      changeset = Post |> Ash.Changeset.new(%{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts)
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 2, max: 4)

      changeset = Ash.Changeset.new(Post, %{body: "n"})
      assert_error(changeset, opts, "body must have length of between 2 and 4")

      changeset = Ash.Changeset.new(Post, %{body: "no way"})
      assert_error(changeset, opts, "body must have length of between 2 and 4")
    end

    test "validate failure with message" do
      {:ok, opts} =
        StringLength.init(
          attribute: :body,
          min: 2,
          max: 4,
          message: "must be at least %{min} and at most %{max} characters"
        )

      changeset = Ash.Changeset.new(Post, %{body: "n"})
      assert_error(changeset, opts, "must be at least 2 and at most 4 characters")

      changeset = Ash.Changeset.new(Post, %{body: "no way"})
      assert_error(changeset, opts, "must be at least 2 and at most 4 characters")
    end
  end

  defp assert_error(changeset, opts, expected_message) do
    {:error, %{message: message, vars: vars}} = StringLength.validate(changeset, opts)
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
