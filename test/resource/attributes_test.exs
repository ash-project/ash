defmodule Ash.Test.Resource.AttributesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Attribute
  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource, domain: Domain

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "attributes are persisted on the resource properly" do
      defposts do
        attributes do
          attribute :foo, :string, public?: true
          attribute :bar, :boolean
        end
      end

      assert [
               _,
               %Attribute{name: :foo, type: Ash.Type.String, primary_key?: false},
               %Attribute{
                 name: :bar,
                 type: Ash.Type.Boolean,
                 primary_key?: false
               }
             ] = Ash.Resource.Info.attributes(Post)

      assert [_, %Attribute{name: :foo}] = Ash.Resource.Info.public_attributes(Post)

      assert %Attribute{name: :bar} = Ash.Resource.Info.attribute(Post, :bar)

      assert nil == Ash.Resource.Info.attribute(Post, :totally_valid_attributes)

      assert nil == Ash.Resource.Info.public_attribute(Post, :bar)
    end
  end

  describe "validation" do
    test "raises if the attribute name is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/expected atom, got: 10/,
        fn ->
          defposts do
            attributes do
              attribute 10, :string
            end
          end
        end
      )
    end

    test "raises if you pass an invalid value for `primary_key?`" do
      assert_raise(
        Spark.Error.DslError,
        ~r/expected boolean, got: 10/,
        fn ->
          defposts do
            attributes do
              attribute :foo, :string, primary_key?: 10, public?: true
            end
          end
        end
      )
    end

    test "raises if you pass an invalid value for `public?`" do
      assert_raise(
        Spark.Error.DslError,
        ~r/expected boolean, got: "an_invalid_value"/,
        fn ->
          defposts do
            attributes do
              attribute :foo, :string, public?: "an_invalid_value"
            end
          end
        end
      )
    end

    test "raises if you pass a reserved name for `name`" do
      for name <- Ash.Resource.reserved_names() -- [:*] do
        assert_raise(
          Spark.Error.DslError,
          ~r/Field #{name} is using a reserved name/,
          fn ->
            defmodule :"Elixir.Resource#{name}" do
              @moduledoc false
              use Ash.Resource, domain: Domain

              attributes do
                uuid_primary_key :id
                attribute name, :string
              end
            end
          end
        )
      end
    end
  end
end
