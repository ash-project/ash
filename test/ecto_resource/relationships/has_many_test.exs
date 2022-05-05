defmodule Ash.Test.Resource.Relationshihps.HasManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.HasMany

  defmodule EctoUser do
    use Ecto.Schema

    schema "users" do
      has_many :posts, EctoPost
    end
  end

  defmodule EctoUser.AshResource do
    use Ash.EctoResource, schema: EctoUser, data_layer: Ash.DataLayer.Ets
  end

  defmodule EctoPost do
    use Ecto.Schema

    schema "posts" do
      field(:name, :string)
    end
  end

  defmodule EctoPost.AshResource do
    use Ash.EctoResource, schema: EctoPost, data_layer: Ash.DataLayer.Ets
  end

  describe "representation" do
    test "it creates a relationship" do
      assert [
               %HasMany{
                 cardinality: :many,
                 destination_field: :ecto_user_id,
                 name: :posts,
                 source_field: :id,
                 type: :has_many,
                 private?: false
               }
             ] = Ash.Resource.Info.relationships(EctoUser.AshResource)

      assert [%HasMany{name: :posts}] =
               Ash.Resource.Info.public_relationships(EctoUser.AshResource)

      assert %HasMany{name: :posts} =
               Ash.Resource.Info.public_relationship(EctoUser.AshResource, :posts)
    end
  end
end
