defmodule Ash.Resource.Verifiers.VerifyCodeInterfaceArgumentCompletenessTest do
  @moduledoc false
  use ExUnit.Case, async: true

  
  describe "verify_code_interface_argument_completeness" do
    test "raises error when required arguments are missing from interface" do
      assert_raise(
        Spark.Error.DslError,
        ~r/Code interface `test_action` is missing required arguments for action `test_action`/,
        fn ->
          defmodule TestResource do
            use Ash.Resource, domain: Ash.Test.Domain
            
            actions do
              action :test_action, :atom do
                argument :required_arg, :string, allow_nil?: false
              end
            end
            
            code_interface do
              define :test_action  # Missing args: [:required_arg]
            end
          end
        end
      )
    end
    
    test "passes when all required arguments are declared" do
      # Should not raise
      defmodule TestResource2 do
        use Ash.Resource, domain: Ash.Test.Domain
        
        actions do
          action :test_action, :atom do
            argument :required_arg, :string, allow_nil?: false
          end
        end
        
        code_interface do
          define :test_action, args: [:required_arg]
        end
      end
    end
    
    test "passes when argument has allow_nil? true" do
      # Should not raise because argument allows nil
      defmodule TestResource3 do
        use Ash.Resource, domain: Ash.Test.Domain
        
        actions do
          action :test_action, :atom do
            argument :optional_arg, :string, allow_nil?: true
          end
        end
        
        code_interface do
          define :test_action  # No args needed since argument allows nil
        end
      end
    end
    
    test "passes when argument has a default value" do
      # Should not raise because argument has a default
      defmodule TestResource4 do
        use Ash.Resource, domain: Ash.Test.Domain
        
        actions do
          action :test_action, :atom do
            argument :arg_with_default, :string, default: "default_value"
          end
        end
        
        code_interface do
          define :test_action  # No args needed since argument has default
        end
      end
    end
    
    test "raises error with multiple missing required arguments" do
      assert_raise(
        Spark.Error.DslError,
        ~r/The action `complex_action` has required arguments: \[:arg1, :arg2\]/,
        fn ->
          defmodule TestResource5 do
            use Ash.Resource, domain: Ash.Test.Domain
            
            actions do
              action :complex_action, :atom do
                argument :arg1, :string, allow_nil?: false
                argument :arg2, :integer, allow_nil?: false
                argument :optional_arg, :boolean, allow_nil?: true
              end
            end
            
            code_interface do
              define :complex_action  # Missing args: [:arg1, :arg2]
            end
          end
        end
      )
    end
    
    test "passes when using {:optional, arg} syntax" do
      # Should not raise
      defmodule TestResource6 do
        use Ash.Resource, domain: Ash.Test.Domain
        
        actions do
          action :test_action, :atom do
            argument :required_arg, :string, allow_nil?: false
          end
        end
        
        code_interface do
          define :test_action, args: [{:optional, :required_arg}]
        end
      end
    end
    
    test "works with create actions" do
      assert_raise(
        Spark.Error.DslError,
        ~r/Code interface `create_user` is missing required arguments/,
        fn ->
          defmodule TestResource7 do
            use Ash.Resource, domain: Ash.Test.Domain
            
            attributes do
              uuid_primary_key :id
              attribute :name, :string, public?: true
            end
            
            actions do
              create :create_user do
                argument :password, :string, allow_nil?: false
                argument :password_confirmation, :string, allow_nil?: false
              end
            end
            
            code_interface do
              define :create_user  # Missing args: [:password, :password_confirmation]
            end
          end
        end
      )
    end
    
    test "works with read actions" do
      assert_raise(
        Spark.Error.DslError,
        ~r/Code interface `search` is missing required arguments/,
        fn ->
          defmodule TestResource8 do
            use Ash.Resource, domain: Ash.Test.Domain
            
            attributes do
              uuid_primary_key :id
              attribute :title, :string, public?: true
            end
            
            actions do
              read :search do
                argument :query, :string, allow_nil?: false
              end
            end
            
            code_interface do
              define :search  # Missing args: [:query]
            end
          end
        end
      )
    end
    
    test "passes when interface name differs from action name" do
      # Should not raise
      defmodule TestResource9 do
        use Ash.Resource, domain: Ash.Test.Domain
        
        actions do
          action :internal_action, :atom do
            argument :required_arg, :string, allow_nil?: false
          end
        end
        
        code_interface do
          define :public_name, action: :internal_action, args: [:required_arg]
        end
      end
    end
    
    test "includes helpful error message with fix suggestion" do
      error = assert_raise(
        Spark.Error.DslError,
        fn ->
          defmodule TestResource10 do
            use Ash.Resource, domain: Ash.Test.Domain
            
            actions do
              action :analyze_sentiment, :atom do
                argument :text, :string, allow_nil?: false
              end
            end
            
            code_interface do
              define :analyze_sentiment  # Missing args: [:text]
            end
          end
        end
      )
      
      assert error.message =~ "Fix by updating your code interface:"
      assert error.message =~ "define :analyze_sentiment, args: [:text]"
      assert error.message =~ "Alternatively, if these arguments should be optional"
    end
    
    test "passes when custom input transforms to required argument" do
      # Should not raise because custom input transforms :map to :id
      defmodule TestResource11 do
        use Ash.Resource, domain: Ash.Test.Domain
        
        attributes do
          uuid_primary_key :id
        end
        
        actions do
          update :update_user do
            argument :user_id, :string, allow_nil?: false
          end
        end
        
        code_interface do
          define :update_by_map do
            action :update_user
            args [:map]
            
            custom_input :map, :map do
              transform to: :user_id, using: &Map.fetch!(&1, :user_id)
            end
          end
        end
      end
    end
    
    test "passes when argument is excluded" do
      # Should not raise because argument is explicitly excluded
      defmodule TestResource12 do
        use Ash.Resource, domain: Ash.Test.Domain
        
        attributes do
          uuid_primary_key :id
        end
        
        actions do
          update :update_user do
            argument :user_id, :string, allow_nil?: false
          end
        end
        
        code_interface do
          define :update_user, exclude_inputs: [:user_id]
        end
      end
    end
  end
end