defmodule Ash.TestTest do
  use ExUnit.Case
  import Ash.Test

  describe "assert_stripped/1" do
    test "compares single resources with ==" do
      resource1 = %{
        name: "John",
        age: 30,
        __metadata__: %{foo: 1},
        __meta__: %Ecto.Schema.Metadata{context: 1}
      }

      resource2 = %{
        name: "John",
        age: 30,
        __metadata__: %{foo: 2},
        __meta__: %Ecto.Schema.Metadata{context: 2}
      }

      assert_raise ExUnit.AssertionError, fn ->
        assert resource1 == resource2
      end

      assert_stripped resource1 == resource2
    end

    test "compares single resources with ===" do
      resource1 = %{
        name: "John",
        age: 30,
        __metadata__: %{foo: 1},
        __meta__: %Ecto.Schema.Metadata{context: 1}
      }

      resource2 = %{
        name: "John",
        age: 30,
        __metadata__: %{foo: 2},
        __meta__: %Ecto.Schema.Metadata{context: 2}
      }

      assert_raise ExUnit.AssertionError, fn ->
        assert resource1 === resource2
      end

      assert_stripped resource1 === resource2
    end

    test "compares single resources with !=" do
      resource1 = %{
        name: "John",
        age: 30,
        __metadata__: %{foo: 1},
        __meta__: %Ecto.Schema.Metadata{context: 1}
      }

      resource2 = %{
        name: "Jane",
        age: 25,
        __metadata__: %{foo: 2},
        __meta__: %Ecto.Schema.Metadata{context: 2}
      }

      assert_stripped resource1 != resource2
    end

    test "compares single resources with !==" do
      resource1 = %{
        name: "John",
        age: 30,
        __metadata__: %{foo: 1},
        __meta__: %Ecto.Schema.Metadata{context: 1}
      }

      resource2 = %{
        name: "Jane",
        age: 25,
        __metadata__: %{foo: 2},
        __meta__: %Ecto.Schema.Metadata{context: 2}
      }

      assert_stripped resource1 !== resource2
    end

    test "compares lists of resources" do
      list1 = [
        %{
          name: "John",
          age: 30,
          __metadata__: %{foo: 1},
          __meta__: %Ecto.Schema.Metadata{context: 1}
        },
        %{
          name: "Jane",
          age: 25,
          __metadata__: %{foo: 2},
          __meta__: %Ecto.Schema.Metadata{context: 2}
        }
      ]

      list2 = [
        %{
          name: "John",
          age: 30,
          __metadata__: %{foo: 3},
          __meta__: %Ecto.Schema.Metadata{context: 3}
        },
        %{
          name: "Jane",
          age: 25,
          __metadata__: %{foo: 4},
          __meta__: %Ecto.Schema.Metadata{context: 4}
        }
      ]

      assert_raise ExUnit.AssertionError, fn ->
        assert list1 === list2
      end

      assert_stripped list1 === list2
    end

    test "checks if resource is in a list" do
      resource = %{
        name: "John",
        age: 30,
        __metadata__: %{foo: 1},
        __meta__: %Ecto.Schema.Metadata{context: 1}
      }

      list = [
        %{
          name: "Jane",
          age: 25,
          __metadata__: %{foo: 2},
          __meta__: %Ecto.Schema.Metadata{context: 2}
        },
        %{
          name: "John",
          age: 30,
          __metadata__: %{foo: 3},
          __meta__: %Ecto.Schema.Metadata{context: 3}
        }
      ]

      assert_raise ExUnit.AssertionError, fn ->
        assert resource in list
      end

      assert_stripped resource in list
    end

    test "checks if resource is not in a list" do
      resource = %{
        name: "Bob",
        age: 40,
        __metadata__: %{foo: 1},
        __meta__: %Ecto.Schema.Metadata{context: 1}
      }

      list = [
        %{
          name: "Jane",
          age: 25,
          __metadata__: %{foo: 2},
          __meta__: %Ecto.Schema.Metadata{context: 2}
        },
        %{
          name: "John",
          age: 30,
          __metadata__: %{foo: 3},
          __meta__: %Ecto.Schema.Metadata{context: 3}
        }
      ]

      assert_stripped resource not in list
    end

    test "raises error for unsupported operators" do
      exception =
        assert_raise ArgumentError,
                     fn -> assert_stripped 1 + 1 end

      assert exception.message == ~s"""
             assert_stripped received an unsupported operator or function.

             Expected one of the following:

             * assert_stripped left == right
             * assert_stripped left === right
             * assert_stripped left != right
             * assert_stripped left !== right
             * assert_stripped left in right
             * assert_stripped left not in right

             Got:

             assert_stripped 1 + 1
             """
    end
  end
end
