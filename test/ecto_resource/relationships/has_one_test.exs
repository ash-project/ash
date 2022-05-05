defmodule Ash.Test.Resource.Relationshihps.HasOneTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.HasOne

  defmodule EctoPost do
    use Ecto.Schema

    schema "posts" do
      has_one :user, EctoUser
    end
  end

  defmodule EctoPost.AshResource do
    use Ash.EctoResource, schema: EctoPost, data_layer: Ash.DataLayer.Ets
  end

  defmodule EctoUser do
    use Ecto.Schema

    schema "users" do
      field(:name, :string)
    end
  end

  defmodule EctoUser.AshResource do
    use Ash.EctoResource, schema: EctoUser, data_layer: Ash.DataLayer.Ets
  end

  describe "representation" do
    test "it creates a relationship" do
      assert [
               %HasOne{
                 cardinality: :one,
                 destination_field: :ecto_post_id,
                 name: :user,
                 source_field: :id,
                 type: :has_one,
                 private?: false
               }
             ] = Ash.Resource.Info.relationships(EctoPost.AshResource)

      assert [%HasOne{name: :user}] = Ash.Resource.Info.public_relationships(EctoPost.AshResource)

      assert %HasOne{name: :user} =
               Ash.Resource.Info.public_relationship(EctoPost.AshResource, :user)
    end
  end
end
