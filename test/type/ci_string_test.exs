defmodule Ash.Test.Type.CiString do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset
  require Ash.Query

  alias Ash.Test.AnyApi, as: Api

  defmodule Post do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
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
      attribute :string_a, :ci_string
      attribute :string_b, :ci_string, constraints: [trim?: false]
      attribute :string_c, :ci_string, constraints: [allow_empty?: true]
      attribute :string_d, :ci_string, constraints: [allow_empty?: true, trim?: false]

      attribute :string_e, :ci_string, constraints: [min_length: 3, max_length: 6]
      attribute :string_f, :ci_string, constraints: [min_length: 3, max_length: 6, trim?: false]
    end
  end

  test "it handles non-empty values" do
    post =
      Post
      |> new(%{
        string_a: "  Foo  ",
        string_b: "  fOo  ",
        string_c: "  baR  ",
        string_d: "  BaR  "
      })
      |> Api.create!()

    assert Comp.equal?(post.string_a, "foo")
    assert Comp.equal?(post.string_b, "  foo  ")
    assert Comp.equal?(post.string_c, "bar")
    assert Comp.equal?(post.string_d, "  bar  ")
  end

  test "it handles empty values" do
    post =
      Post
      |> new(%{
        string_a: " ",
        string_b: " ",
        string_c: " ",
        string_d: " "
      })
      |> Api.create!()

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
      |> new(%{string_e: e_val, string_f: f_val})
      |> Api.create!()
    end)
  end

  test "it handles too short values with length constraints" do
    assert_raise(Ash.Error.Invalid, ~r/string_e: length must be greater/, fn ->
      Post
      |> new(%{string_e: "   45   "})
      |> Api.create!()
    end)

    assert_raise(Ash.Error.Invalid, ~r/string_f: length must be greater/, fn ->
      Post
      |> new(%{string_f: "12"})
      |> Api.create!()
    end)
  end

  test "it handles too long values with length constraints" do
    assert_raise(Ash.Error.Invalid, ~r/string_e: length must be less/, fn ->
      Post
      |> new(%{string_e: "1234567"})
      |> Api.create!()
    end)

    assert_raise(Ash.Error.Invalid, ~r/string_f: length must be less/, fn ->
      Post
      |> new(%{string_f: "   45   "})
      |> Api.create!()
    end)
  end

  test "filters are case insensitive" do
    Post
    |> new(%{string_f: "foobar"})
    |> Api.create!()

    assert [_] =
             Post
             |> Ash.Query.filter(string_f == "FoObAr")
             |> Api.read!()
  end
end
