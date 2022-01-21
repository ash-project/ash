defmodule Ash.Test.EctoResource.PrimaryKeyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Attribute

  describe "primary keys" do
    test "default" do
      defmodule EctoUser do
        use Ecto.Schema

        schema "users" do
        end
      end

      defmodule User do
        use Ash.EctoResource, schema: EctoUser
      end

      assert [
               %Attribute{name: :id, type: Ash.Type.Integer, primary_key?: true}
             ] = Ash.Resource.Info.attributes(User)
    end

    test ":id" do
      defmodule EctoUser do
        use Ecto.Schema
        @primary_key {:id, :id, autogenerate: true}

        schema "users" do
        end
      end

      defmodule User do
        use Ash.EctoResource, schema: EctoUser
      end

      assert [
               %Attribute{name: :id, type: Ash.Type.Integer, primary_key?: true}
             ] = Ash.Resource.Info.attributes(User)
    end

    test ":binary_id" do
      defmodule EctoUser do
        use Ecto.Schema
        @primary_key {:id, :binary_id, autogenerate: true}

        schema "users" do
        end
      end

      defmodule User do
        use Ash.EctoResource, schema: EctoUser
      end

      assert [
               %Attribute{name: :id, type: Ash.Type.UUID, primary_key?: true}
             ] = Ash.Resource.Info.attributes(User)
    end

    test "Ecto.UUID" do
      defmodule EctoUser do
        use Ecto.Schema
        @primary_key {:id, Ecto.UUID, autogenerate: true}

        schema "users" do
        end
      end

      defmodule User do
        use Ash.EctoResource, schema: EctoUser
      end

      assert [
               %Attribute{name: :id, type: Ash.Type.UUID, primary_key?: true}
             ] = Ash.Resource.Info.attributes(User)
    end
  end
end
