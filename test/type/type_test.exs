# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.TypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule PostTitle do
    @moduledoc false
    use Ash.Type

    def storage_type(_), do: :string

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
        {:error,
         message: "is too long, max_length is %{max_length}", max_length: constraints[:max_length]}
      else
        :ok
      end
    end

    def cast_input(value, _) when is_binary(value) do
      if String.match?(value, ~r/[a-zA-Z\w]*/) do
        {:ok, value}
      else
        {:error, "must match ~r/[a-zA-Z\w]*/"}
      end
    end

    def cast_input(_, _), do: :error

    def cast_stored(value, _) when is_binary(value), do: {:ok, value}
    def cast_stored(_, _), do: :error

    def dump_to_native(nil, _), do: {:ok, nil}
    def dump_to_native(value, _) when is_binary(value), do: {:ok, value}
    def dump_to_native(_, _), do: :error
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :title, PostTitle, constraints: [max_length: 10], public?: true

      attribute :post_type, :atom,
        allow_nil?: false,
        constraints: [one_of: [:text, :video]],
        public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end
  end

  test "it accepts valid data" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "foobar", post_type: :text})
      |> Ash.create!()

    assert post.title == "foobar"
  end

  test "it rejects invalid title data" do
    assert_raise(Ash.Error.Invalid, ~r/is too long, max_length is 10/, fn ->
      Post
      |> Ash.Changeset.for_create(:create, %{title: "foobarbazbuzbiz", post_type: :text})
      |> Ash.create!()
    end)
  end

  test "returns error for invalid keys" do
    foo = {:foo, [type: :string]}
    bar = {:bar, [type: :integer]}
    constraints = [fields: [foo, bar]]
    invalid_key = {:something, :invalid}

    assert {:error, error} = Ash.Type.init(:map, constraints ++ [invalid_key])

    assert error =~
             "unknown options [:something], valid options are: [:preserve_nil_values?, :fields]"

    invalid_foo = {:foo, [type: :string, instance_of: "invalid"]}
    invalid_constraints = [fields: [invalid_foo, bar]]

    assert {:error, error} = Ash.Type.init(:map, invalid_constraints)

    assert error =~
             "unknown options [:instance_of], valid options are: [:type, :allow_nil?, :description, :constraints] (in options [:fields, :foo])"

    invalid_foo = {:foo, [type: :string, constraints: [invalid_key]]}
    invalid_constraints = [fields: [invalid_foo, bar]]

    assert {:error, error} = Ash.Type.init(:map, invalid_constraints)

    assert error =~
             "unknown options [:something], valid options are: [:max_length, :min_length, :match, :trim?, :allow_empty?]"
  end
end
