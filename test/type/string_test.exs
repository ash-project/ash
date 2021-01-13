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
      attribute :optional_string, :string
      attribute :required_string, :string, allow_nil?: false
      attribute :original_string, :string, constraints: [trim?: false]
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

  test "it trims values" do
    post =
      Post
      |> new(%{required_string: "    foobar   "})
      |> Api.create!()

    assert post.required_string == "foobar"
  end

  test "it sets empty values to nil" do
    post =
      Post
      |> new(%{required_string: "value", optional_string: "   "})
      |> Api.create!()

    assert post.optional_string == nil
  end

  test "it rejects required but empty values" do
    assert_raise(Ash.Error.Invalid, ~r/attribute required_string is required/, fn ->
      Post
      |> new(%{required_string: "   "})
      |> Api.create!()
    end)
  end

  test "it doesn't change values if trim? is set to false" do
    post =
      Post
      |> new(%{required_string: "value", original_string: "  value  "})
      |> Api.create!()

    assert post.original_string == "  value  "

    post2 =
      Post
      |> new(%{required_string: "value", original_string: "   "})
      |> Api.create!()

    assert post2.original_string == "   "
  end
end
