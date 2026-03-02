# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Type.StructInferenceTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule EmbeddedResource do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute(:name, :string, allow_nil?: false, public?: true)
      attribute(:title, :string, allow_nil?: false, public?: true)
    end
  end

  defmodule NonEmbeddedResource do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string, allow_nil?: false, public?: true)
    end
  end

  test "cast_stored infers fields for embedded resources" do
    assert {:ok, %EmbeddedResource{name: "fred", title: "Engineer"}} =
             Ash.Type.cast_stored(Ash.Type.Struct, %{"name" => "fred", "title" => "Engineer"},
               instance_of: EmbeddedResource
             )
  end

  test "cast_stored does not infer fields for non-embedded resources" do
    assert :error =
             Ash.Type.cast_stored(Ash.Type.Struct, %{"id" => Ecto.UUID.generate()},
               instance_of: NonEmbeddedResource
             )
  end
end
