# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.ValidateAttributeArgumentValidationsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers
  import Spark.Test

  test "attribute_equals with a missing attribute is a compile-time error" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:validations]} do
        defposts do
          validations do
            validate attribute_equals(:number, 0)
          end
        end
      end

    assert error.message =~ "`attribute_equals` refers to `:number`"
    assert error.message =~ "is not an attribute"
  end

  test "attribute_equals suggests argument_equals when the name is an argument" do
    error =
      assert_dsl_error do
        defposts do
          actions do
            create :create do
              argument :number, :integer
            end
          end

          validations do
            validate attribute_equals(:number, 0)
          end
        end
      end

    assert error.message =~ "`attribute_equals` refers to `:number`"
    assert error.message =~ "is an argument"
    assert error.message =~ "Did you mean `argument_equals`?"
  end

  test "argument_equals suggests attribute_equals when the name is an attribute" do
    error =
      assert_dsl_error do
        defposts do
          attributes do
            attribute :number, :integer, public?: true
          end

          actions do
            defaults [:create, :read]
          end

          validations do
            validate argument_equals(:number, 0)
          end
        end
      end

    assert error.message =~ "`argument_equals` refers to `:number`"
    assert error.message =~ "is an attribute"
    assert error.message =~ "Did you mean `attribute_equals`?"
  end

  test "argument_does_not_equal suggests attribute_does_not_equal when mixed up" do
    error =
      assert_dsl_error do
        defposts do
          attributes do
            attribute :admin, :boolean, public?: true
          end

          actions do
            defaults [:create, :update]
          end

          validations do
            validate argument_does_not_equal(:admin, true)
          end
        end
      end

    assert error.message =~ "`argument_does_not_equal` refers to `:admin`"
    assert error.message =~ "Did you mean `attribute_does_not_equal`?"
  end

  test "attribute_does_not_equal suggests argument_does_not_equal when mixed up" do
    error =
      assert_dsl_error do
        defposts do
          actions do
            create :create do
              argument :admin, :boolean
            end
          end

          validations do
            validate attribute_does_not_equal(:admin, true)
          end
        end
      end

    assert error.message =~ "`attribute_does_not_equal` refers to `:admin`"
    assert error.message =~ "Did you mean `argument_does_not_equal`?"
  end

  test "action-level attribute_equals suggests argument_equals for action arguments" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:actions, :create]} do
        defposts do
          actions do
            create :create do
              argument :number, :integer
              validate attribute_equals(:number, 0)
            end
          end
        end
      end

    assert error.message =~ "Did you mean `argument_equals`?"
  end

  test "action-level argument_equals suggests attribute_equals for attributes" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:actions, :create]} do
        defposts do
          attributes do
            attribute :title, :string, public?: true
          end

          actions do
            create :create do
              validate argument_equals(:title, "hello")
            end
          end
        end
      end

    assert error.message =~ "Did you mean `attribute_equals`?"
    assert error.message =~ "on the `:create` action"
  end

  test "nested where validations are checked" do
    error =
      assert_dsl_error do
        defposts do
          attributes do
            attribute :title, :string, public?: true
          end

          actions do
            defaults [:create]
          end

          validations do
            validate present(:title), where: [argument_equals(:title, "hello")]
          end
        end
      end

    assert error.message =~ "`argument_equals` refers to `:title`"
    assert error.message =~ "Did you mean `attribute_equals`?"
  end

  test "nested all/any/negate validations are checked" do
    error =
      assert_dsl_error do
        defposts do
          attributes do
            attribute :title, :string, public?: true
          end

          validations do
            validate all([
                       negate(attribute_equals(:missing, true))
                     ])
          end
        end
      end

    assert error.message =~ "`attribute_equals` refers to `:missing`"
  end

  test "argument_in suggests one_of when the name is an attribute" do
    error =
      assert_dsl_error do
        defposts do
          attributes do
            attribute :state, :atom, public?: true
          end

          actions do
            defaults [:create]
          end

          validations do
            validate argument_in(:state, [:open, :closed])
          end
        end
      end

    assert error.message =~ "`argument_in` refers to `:state`"
    assert error.message =~ "Did you mean `one_of`?"
  end

  test "one_of, match, and string_length are not flagged when the name is an argument" do
    refute_dsl_errors do
      defposts do
        actions do
          create :create do
            argument :state, :atom
            argument :email, :string
            argument :password, :string
            validate one_of(:state, [:open, :closed])
            validate match(:email, ~r/@/)
            validate string_length(:password, min: 8)
          end
        end
      end
    end
  end

  test "valid attribute and argument validations compile" do
    refute_dsl_errors do
      defposts do
        attributes do
          attribute :title, :string, public?: true
        end

        actions do
          create :create do
            argument :number, :integer
            validate argument_does_not_equal(:number, 0)
            validate attribute_equals(:title, "hello")
          end
        end

        validations do
          validate attribute_equals(:title, "hello")
          validate argument_equals(:number, 1)
        end
      end
    end
  end

  test "present and compare are not flagged when the name is an argument" do
    refute_dsl_errors do
      defposts do
        actions do
          create :create do
            argument :count, :integer
            validate present(:count)
            validate compare(:count, greater_than: 0)
          end
        end
      end
    end
  end

  test "pipeline validations are checked" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:pipelines, :check_title]} do
        defposts do
          attributes do
            attribute :title, :string, public?: true
          end

          pipelines do
            pipeline :check_title do
              validate argument_equals(:title, "hello")
            end
          end
        end
      end

    assert error.message =~ "Did you mean `attribute_equals`?"
  end
end
