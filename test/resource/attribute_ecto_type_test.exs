# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.AttributeEctoTypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  # A data layer can take charge of an attribute's field in the generated Ecto
  # schema, so that dumping and loading its stored value go through a type of the
  # data layer's choosing.

  defmodule DataLayer do
    @moduledoc false
    use Spark.Dsl.Extension, transformers: [], sections: []

    @behaviour Ash.DataLayer

    defdelegate can?(resource, feature), to: Ash.DataLayer.Simple
    defdelegate resource_to_query(resource, domain), to: Ash.DataLayer.Simple
    defdelegate run_query(query, resource), to: Ash.DataLayer.Simple

    @doc false
    def attribute_ecto_type(_resource, %{name: :taken_charge_of}) do
      Ash.Type.ecto_type(Ash.Type.CiString)
    end

    def attribute_ecto_type(_resource, _attribute), do: nil
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: DataLayer

    attributes do
      uuid_primary_key :id
      attribute :taken_charge_of, :string
      attribute :left_alone, :string
    end
  end

  defmodule UnimplementedPost do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :taken_charge_of, :string
    end
  end

  test "the data layer's type is used for the attribute it claims" do
    assert {:parameterized, {Ash.Type.CiString.EctoType, _}} =
             Post.__schema__(:type, :taken_charge_of)
  end

  test "returning nil falls back to the attribute type's own Ecto type" do
    assert {:parameterized, {Ash.Type.String.EctoType, _}} =
             Post.__schema__(:type, :left_alone)
  end

  test "a data layer that does not implement the callback is unaffected" do
    assert {:parameterized, {Ash.Type.String.EctoType, _}} =
             UnimplementedPost.__schema__(:type, :taken_charge_of)
  end
end
