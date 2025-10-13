defmodule Ash.Test.Type.NilApplyConstraintsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule StrictString do
    @moduledoc false
    use Ash.Type

    def storage_type(_), do: :string

    def cast_input(value, _) when is_binary(value), do: {:ok, value}
    def cast_input(nil, _), do: {:ok, nil}
    def cast_input(_, _), do: :error

    def cast_stored(value, _) when is_binary(value), do: {:ok, value}
    def cast_stored(nil, _), do: {:ok, nil}
    def cast_stored(_, _), do: :error

    def dump_to_native(nil, _), do: {:ok, nil}
    def dump_to_native(value, _) when is_binary(value), do: {:ok, value}
    def dump_to_native(_, _), do: :error

    # This intentionally errors when nil is passed to apply_constraints
    # to test that Ash properly handles nil values before calling this callback
    def apply_constraints(nil, _) do
      {:error, message: "StrictString does not allow nil in apply_constraints"}
    end

    def apply_constraints(value, _) when is_binary(value) do
      {:ok, value}
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :title, StrictString, allow_nil?: true, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]

      update :clear_title do
        accept []
        change set_attribute(:title, nil)
      end
    end
  end

  test "can execute action that sets nil-able custom type attribute to nil" do
    # Create a post with a non-nil title
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Hello World"})
      |> Ash.create!()

    assert post.title == "Hello World"

    # Execute the clear_title action that sets title to nil
    updated_post =
      post
      |> Ash.Changeset.for_update(:clear_title)
      |> Ash.update!()

    assert updated_post.title == nil
  end

  test "can create resource with nil value for nullable attribute" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: nil})
      |> Ash.create!()

    assert post.title == nil
  end

  test "custom type still validates non-nil values properly" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Valid Title"})
      |> Ash.create!()

    assert post.title == "Valid Title"
  end
end
