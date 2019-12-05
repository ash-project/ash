defmodule Ash.Test.Type.TypeTest do
  use ExUnit.Case, async: true

  defmodule PostTitle do
    use Ash.Type

    def describe() do
      "A post title is less than 10 characters long and is only alphabetic characters and whitespace"
    end

    def storage_type(), do: :string

    def cast_input(value) when is_bitstring(value) do
      if String.length(value) <= 10 && String.match?(value, ~r/[a-zA-Z\w]*/) do
        {:ok, value}
      else
        :error
      end
    end

    def cast_input(_), do: :error

    def cast_stored(value) when is_bitstring(value), do: value
    def cast_stored(_), do: :error

    def dump_to_native(value) when is_bitstring(value), do: value
    def dump_to_native(_), do: :error
  end

  defmodule Post do
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

    attributes do
      attribute :title, PostTitle
    end

    actions do
      defaults [:create]
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Post]
  end

  test "it accepts valid data" do
    {:ok, post} = Api.create(Post, %{attributes: %{title: "foobar"}})

    assert post.title == "foobar"
  end

  test "it rejects invalid data" do
    # As we add informative errors, this test will fail and we will know to test those
    # more informative errors.
    assert {:error, "invalid"} = Api.create(Post, %{attributes: %{title: "foobarbazbuzbiz"}})
  end
end
