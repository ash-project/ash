# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.StringTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Expr

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      # Possible constraint combinations:
      #
      # a. [allow_empty?: false, trim?: true] (default)
      # b. [allow_empty?: false, trim?: false]
      # c. [allow_empty?: true, trim?: true]
      # d. [allow_empty?: true, trim?: false]
      #
      attribute :string_a, :string, public?: true
      attribute :string_b, :string, constraints: [trim?: false], public?: true
      attribute :string_c, :string, constraints: [allow_empty?: true], public?: true
      attribute :string_d, :string, constraints: [allow_empty?: true, trim?: false], public?: true

      attribute :string_e, :string, constraints: [min_length: 3, max_length: 6], public?: true

      attribute :string_f, :string,
        constraints: [min_length: 3, max_length: 6, trim?: false],
        public?: true
    end
  end

  test "it handles non-empty values" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        string_a: "  foo  ",
        string_b: "  foo  ",
        string_c: "  bar  ",
        string_d: "  bar  "
      })
      |> Ash.create!()

    assert post.string_a == "foo"
    assert post.string_b == "  foo  "
    assert post.string_c == "bar"
    assert post.string_d == "  bar  "
  end

  test "it handles empty values" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        string_a: " ",
        string_b: " ",
        string_c: " ",
        string_d: " "
      })
      |> Ash.create!()

    assert post.string_a == nil
    assert post.string_b == nil
    assert post.string_c == ""
    assert post.string_d == " "
  end

  test "it handles values with length constraints" do
    e_allowed_values = ["123", "123456", " 123456 "]
    f_allowed_values = [" 2 ", "123456", "  34  "]

    allowed_values = Enum.zip(e_allowed_values, f_allowed_values)

    Enum.each(allowed_values, fn {e_val, f_val} ->
      Post
      |> Ash.Changeset.for_create(:create, %{string_e: e_val, string_f: f_val})
      |> Ash.create!()
    end)
  end

  test "it handles too short values with length constraints" do
    assert_raise(Ash.Error.Invalid, ~r/string_e: length must be greater/, fn ->
      Post
      |> Ash.Changeset.for_create(:create, %{string_e: "   45   "})
      |> Ash.create!()
    end)

    assert_raise(Ash.Error.Invalid, ~r/string_f: length must be greater/, fn ->
      Post
      |> Ash.Changeset.for_create(:create, %{string_f: "12"})
      |> Ash.create!()
    end)
  end

  test "it handles too long values with length constraints" do
    assert_raise(Ash.Error.Invalid, ~r/string_e: length must be less/, fn ->
      Post
      |> Ash.Changeset.for_create(:create, %{string_e: "1234567"})
      |> Ash.create!()
    end)

    assert_raise(Ash.Error.Invalid, ~r/string_f: length must be less/, fn ->
      Post
      |> Ash.Changeset.for_create(:create, %{string_f: "   45   "})
      |> Ash.create!()
    end)
  end

  test "does not run the match regex once a length constraint is violated" do
    constraints = [max_length: 3, match: ~r/^\d+$/]

    assert {:error, error} = Ash.Type.String.apply_constraints("abcd", constraints)
    errors = if Keyword.keyword?(error), do: [error], else: error

    assert Enum.any?(errors, &(&1[:message] =~ "less than or equal"))
    refute Enum.any?(errors, &(&1[:message] =~ "must match"))
  end

  describe "length_count constraint" do
    @combining "a" <> String.duplicate("\u0301", 1_000)

    test "defaults to the configured unit (codepoints in the test config)" do
      assert {:error, _} = Ash.Type.String.apply_constraints(@combining, max_length: 2)
      assert {:ok, _} = Ash.Type.String.apply_constraints("héllo", max_length: 5)

      assert {:ok, _} =
               Ash.Type.String.apply_constraints(@combining,
                 max_length: 2,
                 length_count: :graphemes
               )
    end

    test "counts codepoints when configured" do
      assert {:error, error} =
               Ash.Type.String.apply_constraints(@combining,
                 max_length: 2,
                 length_count: :codepoints
               )

      assert error[:message] =~ "less than or equal"

      assert {:ok, _} =
               Ash.Type.String.apply_constraints("héllo",
                 max_length: 5,
                 length_count: :codepoints
               )

      assert {:error, _} =
               Ash.Type.String.apply_constraints("héllo",
                 min_length: 6,
                 length_count: :codepoints
               )
    end

    test "counts bytes when configured" do
      assert {:error, _} =
               Ash.Type.String.apply_constraints(@combining, max_length: 2, length_count: :bytes)

      # é is two bytes in UTF-8
      assert {:error, _} =
               Ash.Type.String.apply_constraints("héllo", max_length: 5, length_count: :bytes)

      assert {:ok, _} =
               Ash.Type.String.apply_constraints("héllo", max_length: 6, length_count: :bytes)
    end

    test "is not atomic for expressions when explicitly counting graphemes" do
      expr = Ash.Expr.expr(string_a)

      assert {:not_atomic, message} =
               Ash.Type.String.cast_atomic(expr, max_length: 2, length_count: :graphemes)

      assert message =~ "graphemes"

      assert {:not_atomic, _} =
               Ash.Type.String.cast_atomic(expr, min_length: 2, length_count: :graphemes)

      # the configured default (codepoints) is atomic
      assert {:atomic, _} = Ash.Type.String.cast_atomic(expr, max_length: 2)

      # no length constraints, so nothing to count
      assert {:atomic, _} = Ash.Type.String.cast_atomic(expr, [])

      for count <- [:codepoints, :bytes] do
        assert {:atomic, _} =
                 Ash.Type.String.cast_atomic(expr, max_length: 2, length_count: count)
      end

      # literals are validated eagerly regardless of the count
      assert {:atomic, "ab"} = Ash.Type.String.cast_atomic("ab", max_length: 2)
      assert {:ok, "ab"} = Ash.Type.String.apply_atomic_constraints("ab", max_length: 2)
      assert {:error, _} = Ash.Type.String.apply_atomic_constraints("abc", max_length: 2)

      assert {:error, _} =
               Ash.Type.String.apply_atomic_constraints(@combining,
                 max_length: 2,
                 length_count: :codepoints
               )
    end

    test "applies to atomic constraints" do
      for count <- [:codepoints, :bytes] do
        {:ok, expr} =
          Ash.Type.String.apply_atomic_constraints(Ash.Expr.expr(string_a),
            max_length: 2,
            length_count: count
          )

        assert_raise Ash.Error.Changes.InvalidChanges, ~r/less than or equal to 2/, fn ->
          Ash.Expr.eval!(expr, record: %Post{string_a: @combining}, resource: Post)
        end

        {:ok, expr} =
          Ash.Type.String.apply_atomic_constraints(Ash.Expr.expr(string_a),
            max_length: 5,
            length_count: count
          )

        assert Ash.Expr.eval!(expr, record: %Post{string_a: "abc"}, resource: Post) == "abc"
      end
    end

    test "the generator respects the configured count" do
      for count <- [:graphemes, :codepoints, :bytes] do
        constraints = [min_length: 2, max_length: 5, length_count: count, trim?: false]

        Ash.Type.String.generator(constraints)
        |> Enum.take(50)
        |> Enum.each(fn value ->
          assert {:ok, _} = Ash.Type.String.apply_constraints(value, constraints),
                 "generated #{inspect(value)} violates #{inspect(constraints)}"
        end)
      end
    end
  end
end
