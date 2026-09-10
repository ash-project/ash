# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.CiString do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

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
      attribute :string_a, :ci_string, public?: true
      attribute :string_b, :ci_string, constraints: [trim?: false], public?: true
      attribute :string_c, :ci_string, constraints: [allow_empty?: true], public?: true

      attribute :string_d, :ci_string,
        constraints: [allow_empty?: true, trim?: false],
        public?: true

      attribute :string_e, :ci_string, constraints: [min_length: 3, max_length: 6], public?: true

      attribute :string_f, :ci_string,
        constraints: [min_length: 3, max_length: 6, trim?: false],
        public?: true

      attribute :string_g, :ci_string,
        allow_nil?: true,
        constraints: [match: ~r/^string_[a-z]+$/i],
        public?: true

      attribute :string_h, :ci_string,
        allow_nil?: true,
        constraints: [match: ~r/^string_[a-z]+$/i],
        public?: true

      attribute :string_upper, :ci_string,
        allow_nil?: true,
        constraints: [max_length: 3, casing: :upper],
        public?: true
    end
  end

  test "validates length against the case-folded value" do
    assert {:error, %Ash.Error.Invalid{}} =
             Post
             |> Ash.Changeset.for_create(:create, %{string_upper: "ßßß"})
             |> Ash.create()
  end

  test "it handles non-empty values" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        string_a: "  Foo  ",
        string_b: "  fOo  ",
        string_c: "  baR  ",
        string_d: "  BaR  "
      })
      |> Ash.create!()

    assert Comp.equal?(post.string_a, "foo")
    assert Comp.equal?(post.string_b, "  foo  ")
    assert Comp.equal?(post.string_c, "bar")
    assert Comp.equal?(post.string_d, "  bar  ")
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
    assert Comp.equal?(post.string_c, "")
    assert Comp.equal?(post.string_d, " ")
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

  test "filters are case insensitive" do
    Post
    |> Ash.Changeset.for_create(:create, %{string_f: "foobar"})
    |> Ash.create!()

    assert [_] =
             Post
             |> Ash.Query.filter(string_f == "FoObAr")
             |> Ash.read!()
  end

  test "match for :string_g succeeds on good regexes" do
    Post
    |> Ash.Changeset.for_create(:create, %{string_g: "string_a"})
    |> Ash.create!()

    Post
    |> Ash.Changeset.for_create(:create, %{string_g: "string_b"})
    |> Ash.create!()
  end

  test "match for :string_g rejects bad regexes" do
    assert {:error, %Ash.Error.Invalid{}} =
             Post
             |> Ash.Changeset.for_create(:create, %{string_g: "string_1"})
             |> Ash.create()

    assert {:error, %Ash.Error.Invalid{}} =
             Post
             |> Ash.Changeset.for_create(:create, %{string_g: "string"})
             |> Ash.create()
  end

  test "match for :string_h succeeds on good regexes" do
    Post
    |> Ash.Changeset.for_create(:create, %{string_h: "string_a"})
    |> Ash.create!()

    Post
    |> Ash.Changeset.for_create(:create, %{string_h: "string_b"})
    |> Ash.create!()
  end

  test "match for :string_h rejects bad regexes" do
    assert {:error, %Ash.Error.Invalid{}} =
             Post
             |> Ash.Changeset.for_create(:create, %{string_h: "string_1"})
             |> Ash.create()

    assert {:error, %Ash.Error.Invalid{}} =
             Post
             |> Ash.Changeset.for_create(:create, %{string_h: "string"})
             |> Ash.create()
  end

  describe "length_count constraint" do
    @combining "a" <> String.duplicate("\u0301", 1_000)

    test "defaults to the configured unit (codepoints in the test config)" do
      assert {:error, _} = Ash.Type.CiString.apply_constraints(@combining, max_length: 2)

      assert {:ok, _} =
               Ash.Type.CiString.apply_constraints(@combining,
                 max_length: 2,
                 length_count: :graphemes
               )
    end

    test "counts codepoints or bytes when configured" do
      assert {:error, _} =
               Ash.Type.CiString.apply_constraints(@combining,
                 max_length: 2,
                 length_count: :codepoints
               )

      assert {:error, _} =
               Ash.Type.CiString.apply_constraints(@combining,
                 max_length: 2,
                 length_count: :bytes
               )

      assert {:error, _} =
               Ash.Type.CiString.apply_constraints(Ash.CiString.new("héllo"),
                 max_length: 5,
                 length_count: :bytes
               )

      assert {:ok, %Ash.CiString{}} =
               Ash.Type.CiString.apply_constraints(Ash.CiString.new("héllo"),
                 max_length: 5,
                 length_count: :codepoints
               )
    end

    test "the generator respects the configured count" do
      for count <- [:graphemes, :codepoints, :bytes] do
        constraints = [min_length: 2, max_length: 5, length_count: count, trim?: false]

        Ash.Type.CiString.generator(constraints)
        |> Enum.take(50)
        |> Enum.each(fn value ->
          assert {:ok, _} = Ash.Type.CiString.apply_constraints(value, constraints),
                 "generated #{inspect(value)} violates #{inspect(constraints)}"
        end)
      end
    end
  end
end
