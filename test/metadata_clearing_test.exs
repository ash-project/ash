# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.MetadataClearingTest do
  use ExUnit.Case, async: true

  defmodule Profile do
    use Ash.Resource,
      domain: :test

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

  end

  setup do
    record = struct(Profile, %{
      id: Ash.UUID.generate(),
      name: "test",
      __metadata__: %{test_key: "test_value"}
    })

    {:ok, %{record: record}}
  end

  test "metadata is cleared in for_update", %{record: record} do
    # Before our changes, metadata would persist
    changeset = Ash.Changeset.for_update(Profile, :update, record)
    assert changeset.data.__metadata__ == %{}
  end

  test "metadata is cleared in for_destroy", %{record: record} do
    # Before our changes, metadata would persist
    changeset = Ash.Changeset.for_destroy(Profile, :destroy, record)
    assert changeset.data.__metadata__ == %{}
  end
end

  describe "metadata clearing" do
    test "metadata is cleared when record is passed to for_update" do
      # Create an author with metadata
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "John"})
        |> Ash.Changeset.put_metadata(:test_key, "test_value")
        |> Ash.create!()

      # Verify metadata exists after creation
      assert author.__metadata__[:test_key] == "test_value"

      # Update the author
      updated_changeset =
        author
        |> Ash.Changeset.for_update(:update, %{name: "John Updated"})

      # Verify metadata is cleared in the changeset
      assert updated_changeset.data.__metadata__ == %{}

      # Complete the update
      updated_author = Ash.update!(updated_changeset)

      # Verify final state has no old metadata
      assert updated_author.__metadata__ != %{test_key: "test_value"}
    end

    test "metadata is cleared when record is passed to for_destroy" do
      # Create an author with metadata
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "John"})
        |> Ash.Changeset.put_metadata(:test_key, "test_value")
        |> Ash.create!()

      # Verify metadata exists after creation
      assert author.__metadata__[:test_key] == "test_value"

      # Create destroy changeset
      destroy_changeset =
        author
        |> Ash.Changeset.for_destroy(:destroy)

      # Verify metadata is cleared in the changeset
      assert destroy_changeset.data.__metadata__ == %{}

      # Complete the destroy operation
      Ash.destroy!(destroy_changeset)
    end
  end
end
