defmodule Ash.Test.Resource.Relationships.BelongsToTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.BelongsTo

  defmacrop defposts(do: body) do
    quote do
      defmodule EctoUser do
        use Ecto.Schema

        schema "users" do
          field(:name, :string)
        end
      end

      defmodule EctoUser.AshResource do
        use Ash.EctoResource, schema: EctoUser, data_layer: Ash.DataLayer.Ets
      end

      defmodule EctoPost do
        use Ecto.Schema

        schema "posts" do
          unquote(body)
        end
      end

      defmodule EctoPost.AshResource do
        use Ash.EctoResource, schema: EctoPost, data_layer: Ash.DataLayer.Ets
      end
    end
  end

  describe "representation" do
    test "it creates an attribute" do
      defposts do
        belongs_to :user, EctoUser
      end

      assert [
               _,
               %Ash.Resource.Attribute{
                 name: :user_id,
                 primary_key?: false,
                 type: Ash.Type.UUID,
                 private?: true
               }
             ] = Ash.Resource.Info.attributes(EctoPost.AshResource)
    end

    test "it creates a relationship" do
      defposts do
        belongs_to :user, EctoUser
      end

      assert [
               %BelongsTo{
                 cardinality: :one,
                 define_field?: true,
                 destination: EctoUser.AshResource,
                 destination_field: :id,
                 field_type: :uuid,
                 name: :user,
                 primary_key?: false,
                 source_field: :user_id,
                 type: :belongs_to,
                 private?: false
               }
             ] = Ash.Resource.Info.relationships(EctoPost.AshResource)

      assert [%BelongsTo{name: :user}] =
               Ash.Resource.Info.public_relationships(EctoPost.AshResource)

      assert %BelongsTo{name: :user} =
               Ash.Resource.Info.public_relationship(EctoPost.AshResource, :user)
    end
  end
end
