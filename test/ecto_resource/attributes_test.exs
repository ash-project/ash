defmodule Ash.Test.EctoResource.AttributesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Attribute

  defmacrop defuser(do: body) do
    quote do
      defmodule EctoUser do
        use Ecto.Schema

        schema "users" do
          unquote(body)
        end
      end

      defmodule User do
        use Ash.EctoResource, schema: EctoUser
      end
    end
  end

  describe "representation" do
    test "attributes are persisted on the resource properly" do
      defuser do
        field(:foo, :string)
        field(:bar, :boolean)
      end

      assert [
               _,
               %Attribute{name: :foo, type: Ash.Type.String, primary_key?: false},
               %Attribute{name: :bar, type: Ash.Type.Boolean, primary_key?: false}
             ] = Ash.Resource.Info.attributes(User)

      assert %Attribute{name: :bar} = Ash.Resource.Info.attribute(User, :bar)

      assert nil == Ash.Resource.Info.attribute(User, :totally_valid_attributes)
    end
  end
end
