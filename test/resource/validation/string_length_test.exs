# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.StringLengthTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Expr

  alias Ash.Resource.Validation.StringLength

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

  defmodule AtomicPost do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*]

      update :update_graphemes do
        accept [:body]
        require_atomic? true
        validate string_length(:body, max: 2, count: :graphemes)
      end

      update :update_codepoints do
        accept [:body]
        require_atomic? true
        validate string_length(:body, max: 2, count: :codepoints)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :body, :string, public?: true
    end
  end

  describe "min length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 3)
      changeset = Post |> Ash.Changeset.for_create(:create, %{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 3)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: "no"})

      assert_error(changeset, opts, "must have length of at least 3")
    end
  end

  describe "max length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 3)
      changeset = Post |> Ash.Changeset.for_create(:create, %{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 3)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: "invalid"})

      assert_error(changeset, opts, "must have length of no more than 3")
    end
  end

  describe "exact length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, exact: 3)
      changeset = Post |> Ash.Changeset.for_create(:create, %{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, exact: 3)

      changeset = Ash.Changeset.for_create(Post, :create, %{body: "no"})
      assert_error(changeset, opts, "must have length of exactly 3")

      changeset = Ash.Changeset.for_create(Post, :create, %{body: "invalid"})
      assert_error(changeset, opts, "must have length of exactly 3")
    end
  end

  describe "min and max length" do
    test "validate success" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 2, max: 4)
      changeset = Post |> Ash.Changeset.for_create(:create, %{body: "yes"})

      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "validate failure" do
      {:ok, opts} = StringLength.init(attribute: :body, min: 2, max: 4)

      changeset = Ash.Changeset.for_create(Post, :create, %{body: "n"})
      assert_error(changeset, opts, "must have length of between 2 and 4")

      changeset = Ash.Changeset.for_create(Post, :create, %{body: "invalid"})
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

  describe "count option" do
    @combining "a" <> String.duplicate("\u0301", 1_000)

    test "defaults to the configured unit (codepoints in the test config)" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 2)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: @combining})

      assert_error(changeset, opts, "must have length of no more than 2")
    end

    test "can count codepoints" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 2, count: :codepoints)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: @combining})

      assert_error(changeset, opts, "must have length of no more than 2")

      {:ok, opts} = StringLength.init(attribute: :body, exact: 5, count: :codepoints)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: "héllo"})
      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "can count bytes" do
      {:ok, opts} = StringLength.init(attribute: :body, max: 5, count: :bytes)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: "héllo"})

      assert_error(changeset, opts, "must have length of no more than 5")

      {:ok, opts} = StringLength.init(attribute: :body, min: 6, count: :bytes)
      changeset = Ash.Changeset.for_create(Post, :create, %{body: "héllo"})
      assert :ok = StringLength.validate(changeset, opts, %{})
    end

    test "rejects unknown counts" do
      assert {:error, _} = StringLength.init(attribute: :body, max: 5, count: :words)
    end

    test "is not atomic for expressions when explicitly counting graphemes" do
      marks = String.duplicate("\u0301", 1_000)

      post =
        AtomicPost
        |> Ash.Changeset.for_create(:create, %{body: "a"})
        |> Ash.create!()

      assert_raise Ash.Error.Framework, ~r/can't atomically run string length validation/, fn ->
        post
        |> Ash.Changeset.for_update(:update_graphemes, %{})
        |> Ash.Changeset.atomic_update(:body, Ash.Expr.expr(body <> ^marks))
        |> Ash.update!()
      end

      # literal values are still validated, even via atomic_update
      assert %{body: "ab"} =
               post
               |> Ash.Changeset.for_update(:update_graphemes, %{})
               |> Ash.Changeset.atomic_update(:body, "ab")
               |> Ash.update!()

      assert_raise Ash.Error.Invalid, ~r/must have length of no more than 2/, fn ->
        post
        |> Ash.Changeset.for_update(:update_graphemes, %{})
        |> Ash.Changeset.atomic_update(:body, "abc")
        |> Ash.update!()
      end

      assert_raise Ash.Error.Invalid, ~r/must have length of no more than 2/, fn ->
        post
        |> Ash.Changeset.for_update(:update_graphemes, %{body: "abc"})
        |> Ash.update!()
      end
    end

    test "is applied atomically when counting codepoints or bytes" do
      marks = String.duplicate("\u0301", 1_000)

      post =
        AtomicPost
        |> Ash.Changeset.for_create(:create, %{body: "a"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/must have length of at most 2/, fn ->
        post
        |> Ash.Changeset.for_update(:update_codepoints, %{})
        |> Ash.Changeset.atomic_update(:body, Ash.Expr.expr(body <> ^marks))
        |> Ash.update!()
      end

      assert %{body: "ab"} =
               post
               |> Ash.Changeset.for_update(:update_codepoints, %{})
               |> Ash.Changeset.atomic_update(:body, Ash.Expr.expr(body <> "b"))
               |> Ash.update!()
    end
  end
end
