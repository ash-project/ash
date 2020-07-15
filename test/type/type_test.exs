defmodule Ash.Test.Type.TypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule PostTitle do
    @moduledoc false
    use Ash.Type

    def storage_type, do: :string

    def constraints do
      [
        max_length: [
          type: :pos_integer,
          doc: "The maximum length of the title"
        ]
      ]
    end

    def apply_constraints(value, constraints) do
      if constraints[:max_length] && String.length(value) >= constraints[:max_length] do
        {:error, "is too long, max_length is #{inspect(constraints[:max_length])}"}
      else
        :ok
      end
    end

    def cast_input(value) when is_bitstring(value) do
      if String.match?(value, ~r/[a-zA-Z\w]*/) do
        {:ok, value}
      else
        {:error, "must match ~r/[a-zA-Z\w]*/"}
      end
    end

    def cast_input(_), do: :error

    def cast_stored(value) when is_bitstring(value), do: value
    def cast_stored(_), do: :error

    def dump_to_native(value) when is_bitstring(value), do: value
    def dump_to_native(_), do: :error
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :title, PostTitle, constraints: [max_length: 10]
    end

    actions do
      create :default
      read :default
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

  test "it accepts valid data" do
    post =
      Post
      |> new(%{title: "foobar"})
      |> Api.create!()

    assert post.title == "foobar"
  end

  test "it rejects invalid data" do
    assert_raise(Ash.Error.Invalid, ~r/is too long, max_length is 10/, fn ->
      Post
      |> new(%{title: "foobarbazbuzbiz"})
      |> Api.create!()
    end)
  end
end
