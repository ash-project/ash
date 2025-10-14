# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.AtomTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule TestResource do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*

      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :value, :atom do
        constraints one_of: [
                      :value_a,
                      :value_b
                    ]

        public? true
      end

      attribute :safe_value, :atom do
        public? true
      end

      attribute :unsafe_value, :atom do
        constraints unsafe_to_atom?: true
        public? true
      end
    end
  end

  test "it rejects invalid atom option" do
    assert_raise(Ash.Error.Invalid, ~r/atom must be one of/, fn ->
      TestResource
      |> Ash.Changeset.for_create(:create, %{value: :value_c})
      |> Ash.create!()
    end)
  end

  test "it rejects non existing atom" do
    new_atom = non_existing_atom_string()

    assert_raise(Ash.Error.Invalid, ~r/Invalid value provided for safe_value/, fn ->
      TestResource
      |> Ash.Changeset.for_create(:create, %{safe_value: new_atom})
      |> Ash.create!()
    end)
  end

  test "it accepts non existing atom when unsafe_to_atom constraint is set" do
    new_atom = non_existing_atom_string()

    assert {:ok, %{unsafe_value: cast_atom}} =
             TestResource
             |> Ash.Changeset.for_create(:create, %{unsafe_value: new_atom})
             |> Ash.create()

    # Atom should now exist, so this shouldn't throw an error
    assert cast_atom == String.to_existing_atom(new_atom)
  end

  test "it should cast non-existing atoms when unsafe_to_atom constraint is set" do
    %{id: created_id} =
      TestResource
      |> Ash.Changeset.for_create(:create, %{unsafe_value: "non_existing_atom"})
      |> Ash.create!()

    new_atom = non_existing_atom_string()

    # Modify the record stored in ETS
    # This will ensure that there is no atom with this key in the system.
    table = Process.get({:ash_ets_table, TestResource, nil})
    {key, record} = ETS.Set.get!(table, %{id: created_id})

    updated_record = %{
      record
      | unsafe_value: new_atom
    }

    ETS.Set.put(table, {key, updated_record})

    # Make sure the atom doesnt exist
    assert_raise(ArgumentError, fn -> String.to_existing_atom(new_atom) end)

    # Load the record using Ash, this should cast the string to an atom
    assert [%{id: ^created_id, unsafe_value: cast_atom}] = TestResource |> Ash.read!()

    # Ensure that the atom is created
    assert cast_atom == String.to_existing_atom(new_atom)
  end

  def non_existing_atom_string do
    uuid = Ash.UUID.generate() |> String.replace("-", "_")
    "non_existing_atom_#{uuid}"
  end
end
