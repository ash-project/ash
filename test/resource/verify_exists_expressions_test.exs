defmodule Ash.Test.Resource.Verifiers.VerifyExistsExpressionsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Spark.Error.DslError

  defmodule TestDomain do
    use Ash.Domain
  end

  defmodule TestComment do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: nil

    attributes do
      uuid_primary_key(:id)
      attribute(:text, :string, public?: true)
      attribute(:score, :integer, public?: true)
      attribute(:test_resource_valid_action_exists_id, :uuid, public?: true)
      attribute(:test_resource_valid_policy_exists_id, :uuid, public?: true)
      attribute(:test_resource_valid_exists_no_condition_id, :uuid, public?: true)
    end

    actions do
      default_accept(:*)
      defaults([:create, :read, :update, :destroy])
    end
  end

  describe "action filter exists expressions" do
    test "raises error for invalid relationship in action filter" do
      assert_raise DslError,
                   ~r/Exists expression references undefined relationship `invalid_relation`/,
                   fn ->
                     defmodule TestResourceInvalidFilterRelation do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])

                         read :with_filter do
                           filter expr(exists(invalid_relation, name == "test"))
                         end
                       end
                     end
                   end
    end

    test "raises error for syntax error with comparison operator in action filter" do
      assert_raise DslError,
                   ~r/Exists expression has invalid syntax\. Found operator `!=`/,
                   fn ->
                     defmodule TestResourceSyntaxErrorFilter do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       relationships do
                         has_many(:comments, TestComment)
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])

                         read :with_syntax_error do
                           filter expr(exists(comments.text != "deleted"))
                         end
                       end
                     end
                   end
    end

    test "raises error for invalid resource in action filter" do
      assert_raise DslError,
                   ~r/Exists expression references undefined resource `NonExistentResource`/,
                   fn ->
                     defmodule TestResourceInvalidResourceFilter do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])

                         read :with_invalid_resource do
                           filter expr(exists(NonExistentResource, name == "test"))
                         end
                       end
                     end
                   end
    end
  end

  describe "policy exists expressions" do
    test "raises error for invalid relationship in policy check" do
      assert_raise DslError,
                   ~r/Exists expression references undefined relationship `invalid_relation`/,
                   fn ->
                     defmodule TestResourceInvalidPolicyRelation do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil,
                         authorizers: [Ash.Policy.Authorizer]

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       policies do
                         policy always() do
                           authorize_if expr(exists(invalid_relation, name == "test"))
                         end
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])
                       end
                     end
                   end
    end

    test "raises error for syntax error in policy forbid_if check" do
      assert_raise DslError,
                   ~r/Exists expression has invalid syntax\. Found operator `==`/,
                   fn ->
                     defmodule TestResourcePolicySyntaxError do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil,
                         authorizers: [Ash.Policy.Authorizer]

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       relationships do
                         has_many(:comments, TestComment)
                       end

                       policies do
                         policy action(:update) do
                           forbid_if expr(exists(comments.score == 100))
                         end
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])
                       end
                     end
                   end
    end

    test "raises error for syntax error in policy authorize_unless check" do
      assert_raise DslError,
                   ~r/Exists expression has invalid syntax\. Found operator `>`/,
                   fn ->
                     defmodule TestResourcePolicyAuthorizeUnlessSyntaxError do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil,
                         authorizers: [Ash.Policy.Authorizer]

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       relationships do
                         has_many(:comments, TestComment)
                       end

                       policies do
                         policy action(:create) do
                           authorize_unless expr(exists(comments.score > 50))
                         end
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])
                       end
                     end
                   end
    end

    test "raises error for invalid resource in policy check" do
      assert_raise DslError,
                   ~r/Exists expression references undefined resource `InvalidResource`/,
                   fn ->
                     defmodule TestResourceInvalidPolicyResource do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil,
                         authorizers: [Ash.Policy.Authorizer]

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       policies do
                         policy always() do
                           authorize_if expr(exists(InvalidResource, name == "test"))
                         end
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])
                       end
                     end
                   end
    end

    test "raises error for nested relationship path with invalid relationship" do
      assert_raise DslError,
                   ~r/Exists expression references undefined relationship `invalid_nested`/,
                   fn ->
                     defmodule TestResourceNestedInvalidRelation do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil,
                         authorizers: [Ash.Policy.Authorizer]

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       relationships do
                         has_many(:comments, TestComment)
                       end

                       policies do
                         policy always() do
                           authorize_if expr(exists(comments.invalid_nested, name == "test"))
                         end
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])
                       end
                     end
                   end
    end

    test "raises error for policy condition with invalid exists" do
      assert_raise DslError,
                   ~r/Exists expression references undefined relationship `missing_rel`/,
                   fn ->
                     defmodule TestResourceInvalidPolicyCondition do
                       use Ash.Resource,
                         data_layer: Ash.DataLayer.Ets,
                         domain: nil,
                         authorizers: [Ash.Policy.Authorizer]

                       attributes do
                         uuid_primary_key(:id)
                         attribute(:name, :string, public?: true)
                       end

                       policies do
                         policy expr(exists(missing_rel, active == true)) do
                           authorize_if always()
                         end
                       end

                       actions do
                         default_accept(:*)
                         defaults([:create, :read, :update, :destroy])
                       end
                     end
                   end
    end
  end

  describe "valid exists expressions" do
    test "allows valid relationship exists in action filter" do
      defmodule TestResourceValidActionExists do
        use Ash.Resource,
          data_layer: Ash.DataLayer.Ets,
          domain: nil

        attributes do
          uuid_primary_key(:id)
          attribute(:name, :string, public?: true)
        end

        relationships do
          has_many(:comments, TestComment)
        end

        actions do
          default_accept(:*)
          defaults([:create, :read, :update, :destroy])

          read :with_valid_exists do
            filter expr(exists(comments, text == "published"))
          end
        end
      end
    end

    test "allows valid resource exists in policy" do
      defmodule TestResourceValidPolicyExists do
        use Ash.Resource,
          data_layer: Ash.DataLayer.Ets,
          domain: nil,
          authorizers: [Ash.Policy.Authorizer]

        attributes do
          uuid_primary_key(:id)
          attribute(:name, :string, public?: true)
        end

        relationships do
          has_many(:comments, TestComment)
        end

        policies do
          policy always() do
            authorize_if expr(exists(comments, score > 10))
          end

          policy action(:update) do
            forbid_if expr(exists(TestComment, text == "blocked"))
            authorize_if always()
          end
        end

        actions do
          default_accept(:*)
          defaults([:create, :read, :update, :destroy])
        end
      end
    end

    test "allows valid exists expressions without filter condition" do
      defmodule TestResourceValidExistsNoCondition do
        use Ash.Resource,
          data_layer: Ash.DataLayer.Ets,
          domain: nil,
          authorizers: [Ash.Policy.Authorizer]

        attributes do
          uuid_primary_key(:id)
          attribute(:name, :string, public?: true)
        end

        relationships do
          has_many(:comments, TestComment)
        end

        policies do
          policy always() do
            authorize_if expr(exists(comments))
          end
        end

        actions do
          default_accept(:*)
          defaults([:create, :read, :update, :destroy])

          read :check_comments_exist do
            filter expr(exists(comments))
          end
        end
      end
    end
  end
end
