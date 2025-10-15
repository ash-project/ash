# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
      output =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          defposts do
            identities do
              identity :foobar, [:name],
                eager_check_with: Domain,
                pre_check_with: Domain
            end
          end
        end)

      assert String.contains?(output, "but the resource has no primary read action")
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

    test "Identity field names are allowed" do
      defposts do
        actions do
          default_accept :*

          read :read do
            primary? true
          end
        end

        identities do
          identity :foobar, [:name, :contents], field_names: [:contents]
        end
      end

      assert [
               %Ash.Resource.Identity{field_names: [:contents]}
             ] = Ash.Resource.Info.identities(Post)
    end

    test "enforce identity domain is inferred" do
      assert_raise Spark.Error.DslError,
                   ~r/Cannot infer eager_check_with, because the domain is not specified on this resource./,
                   fn ->
                     defmodule EagerSite do
                       @moduledoc false
                       use Ash.Resource, domain: nil

                       attributes do
                         uuid_primary_key :id
                         attribute :url, :string, public?: true
                       end

                       identities do
                         identity :unique_url, [:url], eager_check?: true
                       end
                     end
                   end

      assert_raise Spark.Error.DslError,
                   ~r/Cannot infer pre_check_with, because the domain is not specified on this resource./,
                   fn ->
                     defmodule PreSite do
                       @moduledoc false
                       use Ash.Resource, domain: nil

                       attributes do
                         uuid_primary_key :id
                         attribute :url, :string, public?: true
                       end

                       identities do
                         identity :unique_url, [:url], pre_check?: true
                       end
                     end
                   end
    end

    test "enforce identity constraints on attributes" do
      output =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
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
        end)

      assert String.contains?(output, "All identity keys must be attributes or calculations")
      assert String.contains?(output, ":naem")
    end
  end
end
