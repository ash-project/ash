defmodule Ash.Test.Resource.IdentitiesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          uuid_primary_key :id

          attribute :name, :string
          attribute :contents, :string
        end

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "identities are persisted on the resource properly" do
      defposts do
        resource do
          identities do
            identity :foobar, [:name, :contents]
          end
        end
      end

      assert [%Ash.Resource.Identity{name: :foobar, keys: [:name, :contents]}] =
               Ash.Resource.identities(Post)
    end

    test "Identity descriptions are allowed" do
      defposts do
        resource do
          identities do
            identity :foobar, [:name, :contents], description: "require one of name/contents"
          end
        end
      end

      assert [
               %Ash.Resource.Identity{description: "require one of name/contents"}
             ] = Ash.Resource.identities(Post)
    end
  end
end
