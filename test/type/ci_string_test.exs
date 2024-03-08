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
      defaults [:create, :read, :update, :destroy]
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
    end
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
end
