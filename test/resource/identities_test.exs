defmodule Ash.Test.Resource.IdentitiesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.AnyApi, as: Api

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, api: Api

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
        actions do
          read :read do
            primary? true
          end
        end

        identities do
          identity :foobar, [:name, :contents], pre_check_with: Api
        end
      end

      assert [%Ash.Resource.Identity{name: :foobar, keys: [:name, :contents]}] =
               Ash.Resource.Info.identities(Post)
    end

    test "eager_check_with requires a primary read action" do
      assert_raise Spark.Error.DslError,
                   ~r/but the resource has no primary read action./,
                   fn ->
                     defposts do
                       identities do
                         identity :foobar, [:name], eager_check_with: Api, pre_check_with: Api
                       end
                     end
                   end
    end

    test "Identity descriptions are allowed" do
      defposts do
        actions do
          read :read do
            primary? true
          end
        end

        identities do
          identity :foobar, [:name, :contents],
            description: "require one of name/contents",
            pre_check_with: Api
        end
      end

      assert [
               %Ash.Resource.Identity{description: "require one of name/contents"}
             ] = Ash.Resource.Info.identities(Post)
    end

    test "enforce identity constraints on attributes" do
      assert_raise Spark.Error.DslError,
                   ~r/All identity keys must be attributes. Got: :naem/,
                   fn ->
                     defmodule Site do
                       @moduledoc false
                       use Ash.Resource, api: Api

                       attributes do
                         uuid_primary_key :id
                         attribute :url, :string
                       end
                     end

                     defposts do
                       actions do
                         read :read do
                           primary? true
                         end
                       end

                       identities do
                         identity :name_site, [:naem, :site]
                       end

                       relationships do
                         belongs_to :site, Site
                       end
                     end
                   end
    end
  end
end
