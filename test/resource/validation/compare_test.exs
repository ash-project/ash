defmodule Ash.Test.Resource.Validation.CompareTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.Compare

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource, domain: Domain

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :number_one, :integer do
        public?(true)
      end

      attribute :number_two, :integer do
        public?(true)
      end

      attribute :number_three, :decimal do
        public?(true)
      end

      attribute :number_four, :float do
        public?(true)
      end
    end
  end

  describe "greater than" do
    test "validate success against number" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: 1)
      changeset = Post |> Ash.Changeset.for_create(:create, %{number_one: 100})

      assert :ok = Compare.validate(changeset, opts, %{})
    end

    test "validate success against argument" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: :foo)

      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_argument(:foo, 1)
        |> Ash.Changeset.for_create(:create, %{number_one: 100})

      assert :ok = Compare.validate(changeset, opts, %{})
    end

    test "validate success against attribute" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: :number_two)

      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{number_one: 100, number_two: 1})

      assert :ok = Compare.validate(changeset, opts, %{})
    end

    test "decimals can be compared against" do
      {:ok, opts} = Compare.init(attribute: :number_three, greater_than: 0)

      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{number_three: Decimal.new(1)})

      assert :ok = Compare.validate(changeset, opts, %{})
    end

    test "floats can be compared against" do
      {:ok, opts} = Compare.init(attribute: :number_four, greater_than: 0)

      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{number_four: 1.0})

      assert :ok = Compare.validate(changeset, opts, %{})
    end

    test "decimals can be compared with" do
      {:ok, opts} =
        Compare.init(attribute: :number_one, greater_than: Decimal.new(0))

      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{number_one: 1})

      assert :ok = Compare.validate(changeset, opts, %{})
    end

    test "floats can be compared with" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: 0.0)

      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{number_one: 1})

      assert :ok = Compare.validate(changeset, opts, %{})
    end

    test "validate failure against number" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: 100)
      changeset = Post |> Ash.Changeset.for_create(:create, %{number_one: 1})

      assert_error(changeset, opts, "must be greater than 100")
    end

    test "validate failure against argument" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: :foo)

      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_argument(:foo, 100)
        |> Ash.Changeset.for_create(:create, %{number_one: 1})

      assert_error(changeset, opts, "must be greater than foo")
    end

    test "validate failure against attribute" do
      {:ok, opts} = Compare.init(attribute: :number_one, greater_than: :number_two)

      changeset =
        Post
        |> Ash.Changeset.for_create(:create, %{number_one: 1, number_two: 100})

      assert_error(changeset, opts, "must be greater than number_two")
    end
  end

  defp assert_error(changeset, opts, expected_message) do
    {:error, %{message: message, vars: vars}} = Compare.validate(changeset, opts, %{})
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
