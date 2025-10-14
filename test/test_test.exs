# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.TestTest do
  use ExUnit.Case
  import Ash.Test

  describe "strip_metadata/1" do
    test "strips map with all metadata fields" do
      input = %{
        id: 1,
        name: "Test",
        __metadata__: %{selected: [:id, :name]},
        __meta__: %Ecto.Schema.Metadata{state: :loaded},
        __lateral_join_source__: %{id: "123"},
        __order__: "123"
      }

      expected_result = %{
        id: 1,
        name: "Test",
        __metadata__: %{},
        __meta__: %Ecto.Schema.Metadata{},
        __lateral_join_source__: nil,
        __order__: nil
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips map with only __metadata__ field" do
      input = %{
        id: 1,
        name: "Test",
        __metadata__: %{selected: [:id, :name]}
      }

      expected_result = %{
        id: 1,
        name: "Test",
        __metadata__: %{}
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips map with only __meta__ field" do
      input = %{
        id: 1,
        name: "Test",
        __meta__: %Ecto.Schema.Metadata{state: :loaded}
      }

      expected_result = %{
        id: 1,
        name: "Test",
        __meta__: %Ecto.Schema.Metadata{}
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips map with only __lateral_join_source__ field" do
      input = %{
        id: 1,
        name: "Test",
        __lateral_join_source__: %{id: "123"}
      }

      expected_result = %{
        id: 1,
        name: "Test",
        __lateral_join_source__: nil
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips map with only __order__ field" do
      input = %{
        id: 1,
        name: "Test",
        __order__: "123"
      }

      expected_result = %{
        id: 1,
        name: "Test",
        __order__: nil
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips a list of maps" do
      input = [
        %{
          id: 1,
          name: "User 1",
          __metadata__: %{selected: [:id, :name]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123"
        },
        %{
          id: 2,
          name: "User 2",
          __metadata__: %{selected: [:id, :name]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123"
        }
      ]

      expected_result = [
        %{
          id: 1,
          name: "User 1",
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil
        },
        %{
          id: 2,
          name: "User 2",
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil
        }
      ]

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips a tuple of maps" do
      input = {
        %{
          id: 1,
          name: "User 1",
          __metadata__: %{selected: [:id, :name]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123"
        },
        %{
          id: 2,
          name: "User 2",
          __metadata__: %{selected: [:id, :name]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123"
        }
      }

      expected_result = {
        %{
          id: 1,
          name: "User 1",
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil
        },
        %{
          id: 2,
          name: "User 2",
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil
        }
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips a map of maps" do
      input = %{
        user1: %{
          id: 1,
          name: "User 1",
          __metadata__: %{selected: [:id, :name]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123"
        },
        user2: %{
          id: 2,
          name: "User 2",
          __metadata__: %{selected: [:id, :name]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123"
        }
      }

      expected_result = %{
        user1: %{
          id: 1,
          name: "User 1",
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil
        },
        user2: %{
          id: 2,
          name: "User 2",
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil
        }
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips Ash.Page.Offset with results" do
      input = %Ash.Page.Offset{
        results: [
          %{
            id: 1,
            name: "User 1",
            __metadata__: %{selected: [:id, :name]},
            __meta__: %Ecto.Schema.Metadata{state: :loaded},
            __lateral_join_source__: %{id: "123"},
            __order__: "123"
          },
          %{
            id: 2,
            name: "User 2",
            __metadata__: %{selected: [:id, :name]},
            __meta__: %Ecto.Schema.Metadata{state: :loaded},
            __lateral_join_source__: %{id: "123"},
            __order__: "123"
          }
        ],
        limit: 10,
        count: 2,
        offset: 0,
        more?: false
      }

      expected_result = %Ash.Page.Offset{
        results: [
          %{
            id: 1,
            name: "User 1",
            __metadata__: %{},
            __meta__: %Ecto.Schema.Metadata{},
            __lateral_join_source__: nil,
            __order__: nil
          },
          %{
            id: 2,
            name: "User 2",
            __metadata__: %{},
            __meta__: %Ecto.Schema.Metadata{},
            __lateral_join_source__: nil,
            __order__: nil
          }
        ],
        limit: 10,
        count: 2,
        offset: 0,
        more?: false
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips Ash.Page.Keyset with results" do
      input = %Ash.Page.Keyset{
        results: [
          %{
            id: 1,
            name: "User 1",
            __metadata__: %{selected: [:id, :name]},
            __meta__: %Ecto.Schema.Metadata{state: :loaded},
            __lateral_join_source__: %{id: "123"},
            __order__: "123"
          },
          %{
            id: 2,
            name: "User 2",
            __metadata__: %{selected: [:id, :name]},
            __meta__: %Ecto.Schema.Metadata{state: :loaded},
            __lateral_join_source__: %{id: "123"},
            __order__: "123"
          }
        ],
        limit: 5,
        count: 2,
        after: "abc",
        before: nil,
        more?: true
      }

      expected_result = %Ash.Page.Keyset{
        results: [
          %{
            id: 1,
            name: "User 1",
            __metadata__: %{},
            __meta__: %Ecto.Schema.Metadata{},
            __lateral_join_source__: nil,
            __order__: nil
          },
          %{
            id: 2,
            name: "User 2",
            __metadata__: %{},
            __meta__: %Ecto.Schema.Metadata{},
            __lateral_join_source__: nil,
            __order__: nil
          }
        ],
        limit: 5,
        count: 2,
        after: "abc",
        before: nil,
        more?: true
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips nested lists" do
      input = [
        %{
          id: 1,
          __metadata__: %{selected: [:id]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123",
          users: [
            %{
              name: "John",
              __metadata__: %{selected: [:name]},
              __meta__: %Ecto.Schema.Metadata{state: :loaded},
              __lateral_join_source__: %{id: "123"},
              __order__: "123",
              profiles: [
                %{
                  bio: "Developer",
                  __metadata__: %{selected: [:bio]},
                  __meta__: %Ecto.Schema.Metadata{state: :loaded},
                  __lateral_join_source__: %{id: "123"},
                  __order__: "123"
                }
              ]
            }
          ]
        }
      ]

      expected_result = [
        %{
          id: 1,
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil,
          users: [
            %{
              name: "John",
              __metadata__: %{},
              __meta__: %Ecto.Schema.Metadata{},
              __lateral_join_source__: nil,
              __order__: nil,
              profiles: [
                %{
                  bio: "Developer",
                  __metadata__: %{},
                  __meta__: %Ecto.Schema.Metadata{},
                  __lateral_join_source__: nil,
                  __order__: nil
                }
              ]
            }
          ]
        }
      ]

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips nested tuples" do
      input = {
        %{
          id: 1,
          __metadata__: %{selected: [:id]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123",
          users: {
            %{
              name: "John",
              __metadata__: %{selected: [:name]},
              __meta__: %Ecto.Schema.Metadata{state: :loaded},
              __lateral_join_source__: %{id: "123"},
              __order__: "123",
              profiles: {
                %{
                  bio: "Developer",
                  __metadata__: %{selected: [:bio]},
                  __meta__: %Ecto.Schema.Metadata{state: :loaded},
                  __lateral_join_source__: %{id: "123"},
                  __order__: "123"
                }
              }
            }
          }
        }
      }

      expected_result = {
        %{
          id: 1,
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil,
          users: {
            %{
              name: "John",
              __metadata__: %{},
              __meta__: %Ecto.Schema.Metadata{},
              __lateral_join_source__: nil,
              __order__: nil,
              profiles: {
                %{
                  bio: "Developer",
                  __metadata__: %{},
                  __meta__: %Ecto.Schema.Metadata{},
                  __lateral_join_source__: nil,
                  __order__: nil
                }
              }
            }
          }
        }
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "strips nested maps" do
      input = %{
        id: 1,
        __metadata__: %{selected: [:id]},
        __meta__: %Ecto.Schema.Metadata{state: :loaded},
        __lateral_join_source__: %{id: "123"},
        __order__: "123",
        user: %{
          name: "John",
          __metadata__: %{selected: [:name]},
          __meta__: %Ecto.Schema.Metadata{state: :loaded},
          __lateral_join_source__: %{id: "123"},
          __order__: "123",
          profile: %{
            bio: "Developer",
            __metadata__: %{selected: [:bio]},
            __meta__: %Ecto.Schema.Metadata{state: :loaded},
            __lateral_join_source__: %{id: "123"},
            __order__: "123"
          }
        }
      }

      expected_result = %{
        id: 1,
        __metadata__: %{},
        __meta__: %Ecto.Schema.Metadata{},
        __lateral_join_source__: nil,
        __order__: nil,
        user: %{
          name: "John",
          __metadata__: %{},
          __meta__: %Ecto.Schema.Metadata{},
          __lateral_join_source__: nil,
          __order__: nil,
          profile: %{
            bio: "Developer",
            __metadata__: %{},
            __meta__: %Ecto.Schema.Metadata{},
            __lateral_join_source__: nil,
            __order__: nil
          }
        }
      }

      result = strip_metadata(input)
      assert result == expected_result
    end

    test "handles maps without metadata" do
      input = %{
        id: 1,
        name: "Test"
      }

      assert ^input = strip_metadata(input)
    end

    test "handles Ash.Page.Offset without results" do
      input = %Ash.Page.Offset{
        results: [],
        limit: 10,
        count: 0,
        offset: 0,
        more?: false
      }

      assert ^input = strip_metadata(input)
    end

    test "handles Ash.Page.Keyset without results" do
      input = %Ash.Page.Keyset{
        results: [],
        limit: 10,
        count: 0,
        after: nil,
        before: nil,
        more?: false
      }

      assert ^input = strip_metadata(input)
    end

    test "handles an empty list" do
      assert [] = strip_metadata([])
    end

    test "handles an empty tuple" do
      assert {} = strip_metadata({})
    end

    test "handles basic types" do
      assert strip_metadata(nil) == nil
      assert strip_metadata(42) == 42
      assert strip_metadata(3.14) == 3.14
      assert strip_metadata(true) == true
      assert strip_metadata(false) == false
      assert strip_metadata(:atom) == :atom
      assert strip_metadata("string") == "string"
    end
  end

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
