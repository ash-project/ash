defmodule Ash.Test.Type.StringTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id

      # Possible constraint combinations:
      #
      # a. [allow_empty?: false, trim?: false] (default)
      # b. [allow_empty?: false, trim?: true]
      # c. [allow_empty?: true, trim?: false]
      # d. [allow_empty?: true, trim?: true]
      #
      attribute :string_a, :string
      attribute :string_b, :string, constraints: [trim?: true]
      attribute :string_c, :string, constraints: [allow_empty?: true]
      attribute :string_d, :string, constraints: [allow_empty?: true, trim?: true]
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Post)
    end
  end

  import Ash.Changeset

  test "it handles non-empty values" do
    post =
      Post
      |> new(%{
        string_a: "  foo  ",
        string_b: "  bar  ",
        string_c: "  foo  ",
        string_d: "  bar  "
      })
      |> Api.create!()

    assert post.string_a == "  foo  "
    assert post.string_b == "bar"
    assert post.string_c == "  foo  "
    assert post.string_d == "bar"
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
    assert post.string_c == " "
    assert post.string_d == ""
  end
end
