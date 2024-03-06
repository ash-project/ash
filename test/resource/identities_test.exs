defmodule Ash.Test.Resource.IdentitiesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource, domain: Domain

        attributes do
          uuid_primary_key :id

          attribute :name, :string do
            public?(true)
          end

          attribute :contents, :string do
            public?(true)
          end
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "identities are persisted on the resource properly" do
      defposts do
        actions do
          default_accept :*

          read :read do
            primary? true
          end
        end

        identities do
          identity :foobar, [:name, :contents], pre_check_with: Domain
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
                         identity :foobar, [:name],
                           eager_check_with: Domain,
                           pre_check_with: Domain
                       end
                     end
                   end
    end

    test "Identity descriptions are allowed" do
      defposts do
        actions do
          default_accept :*

          read :read do
            primary? true
          end
        end

        identities do
          identity :foobar, [:name, :contents],
            description: "require one of name/contents",
            pre_check_with: Domain
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
                       use Ash.Resource, domain: Domain

                       attributes do
                         uuid_primary_key :id

                         attribute :url, :string do
                           public?(true)
                         end
                       end
                     end

                     defposts do
                       actions do
                         default_accept :*

                         read :read do
                           primary? true
                         end
                       end

                       identities do
                         identity :name_site, [:naem, :site]
                       end

                       relationships do
                         belongs_to :site, Site do
                           public?(true)
                         end
                       end
                     end
                   end
    end
  end
end
