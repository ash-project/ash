defmodule Ash.Test.Actions.GenericActionsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule PassingFredOrGeorge do
    use Ash.Policy.SimpleCheck

    def describe(_), do: "is one of the twins"

    def match?(_, %{action_input: action_input}, _) do
      String.downcase(action_input.arguments.name) in ["fred", "george"]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      action :hello_with_default, :string do
        argument :name, :string, allow_nil?: false, default: "default"

        run(fn input, _context ->
          {:ok, "Hello #{input.arguments.name}"}
        end)
      end

      action :hello, :string do
        argument :name, :string, allow_nil?: false

        run(fn input, _context ->
          {:ok, "Hello #{input.arguments.name}"}
        end)
      end

      action :do_nothing do
        run fn _, _ -> :ok end
      end

      action :typed_with_value, :integer do
        run fn _, _ -> {:ok, 100} end
      end

      action :untyped_without_value do
        run fn _, _ -> :ok end
      end

      action :typed_without_value, :integer do
        run fn _, _ -> :ok end
      end

      action :returns_context, :string do
        run fn %{context: %{thing: thing}}, _ ->
          {:ok, thing}
        end
      end

      action :untyped_with_value do
        run fn _, _ -> {:ok, :unexpected} end
      end

      action :with_preparation, :string do
        argument :name, :string, allow_nil?: false

        run fn input, _ ->
          {:ok, "Hello #{input.arguments.name}"}
        end
      end

      action :with_validation, :string do
        argument :value, :integer, allow_nil?: false

        run fn input, _ ->
          {:ok, "Value is #{input.arguments.value}"}
        end
      end

      action :with_lifecycle_hooks, :string do
        argument :message, :string, allow_nil?: false

        run fn input, _ ->
          {:ok, "Processed: #{input.arguments.message}"}
        end
      end

      action :with_before_action_validation, :string do
        argument :name, :string, allow_nil?: false

        run fn input, _ ->
          {:ok, "Hello #{input.arguments.name}"}
        end
      end

      action :with_notifications, :string do
        argument :message, :string, allow_nil?: false

        run fn input, _ ->
          notification = %Ash.Notifier.Notification{
            resource: __MODULE__,
            action: input.action,
            data: %{message: input.arguments.message}
          }

          {:ok, "Processed: #{input.arguments.message}", [notification]}
        end
      end

      action :with_action_preparation, :string do
        argument :name, :string, allow_nil?: false

        prepare fn input, _context ->
          # Lowercase the name in action-level preparation
          Ash.ActionInput.set_argument(input, :name, String.downcase(input.arguments[:name]))
        end

        run fn input, _ ->
          {:ok, "Action prepared: #{input.arguments.name}"}
        end
      end

      action :with_action_validation, :string do
        argument :count, :integer, allow_nil?: false

        validate fn input, _context ->
          if input.arguments[:count] >= 5 do
            :ok
          else
            {:error, "Count must be at least 5"}
          end
        end

        run fn input, _ ->
          {:ok, "Count is valid: #{input.arguments.count}"}
        end
      end

      action :with_mixed_preparations_validations, :string do
        argument :text, :string, allow_nil?: false

        prepare fn input, _context ->
          # Action-level preparation: add prefix
          current_text = input.arguments[:text]
          Ash.ActionInput.set_argument(input, :text, "action_prep_" <> current_text)
        end

        validate fn input, _context ->
          # Action-level validation: must contain "test"
          if String.contains?(input.arguments[:text], "test") do
            :ok
          else
            {:error, "Text must contain 'test'"}
          end
        end

        run fn input, _ ->
          {:ok, "Final result: #{input.arguments.text}"}
        end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string, allow_nil?: false, public?: true)

      timestamps()
    end

    preparations do
      prepare fn input, _context ->
                case input.action.name do
                  :with_preparation ->
                    # Uppercase the name in preparation
                    Ash.ActionInput.set_argument(
                      input,
                      :name,
                      String.upcase(input.arguments[:name])
                    )

                  _ ->
                    input
                end
              end,
              on: [:action]
    end

    validations do
      validate fn input, _context ->
                 case input.action.name do
                   :with_validation ->
                     if input.arguments[:value] > 10 do
                       :ok
                     else
                       {:error, "Value must be greater than 10"}
                     end

                   _ ->
                     :ok
                 end
               end,
               on: [:action]
    end

    code_interface do
      define :returns_context
    end

    policies do
      policy action(:hello) do
        authorize_if PassingFredOrGeorge
      end

      policy action(:hello_with_default) do
        authorize_if always()
      end

      policy action(:typed_with_value) do
        authorize_if always()
      end

      policy action(:untyped_without_value) do
        authorize_if always()
      end

      policy action(:typed_without_value) do
        authorize_if always()
      end

      policy action(:returns_context) do
        authorize_if always()
      end

      policy action(:untyped_with_value) do
        authorize_if always()
      end

      policy action(:with_preparation) do
        authorize_if always()
      end

      policy action(:with_validation) do
        authorize_if always()
      end

      policy action(:with_lifecycle_hooks) do
        authorize_if always()
      end

      policy action(:with_before_action_validation) do
        authorize_if always()
      end

      policy action(:with_notifications) do
        authorize_if always()
      end

      policy action(:with_action_preparation) do
        authorize_if always()
      end

      policy action(:with_action_validation) do
        authorize_if always()
      end

      policy action(:with_mixed_preparations_validations) do
        authorize_if always()
      end
    end
  end

  describe "generic actions can be called" do
    test "generic actions can be run" do
      assert "Hello fred" =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: "fred"})
               |> Ash.run_action!()
    end

    test "generic actions validate their input" do
      assert {:error, %Ash.Error.Invalid{}} =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: %{a: 10}})
               |> Ash.run_action()

      assert_raise Ash.Error.Invalid, ~r/Invalid Error/, fn ->
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: %{a: 10}})
        |> Ash.run_action!()
      end
    end

    test "generic actions set default values for arguments" do
      assert "Hello default" ==
               Post
               |> Ash.ActionInput.for_action(:hello_with_default, %{})
               |> Ash.run_action!()
    end

    test "generic actions don't accept unknown keys" do
      assert {:error, %Ash.Error.Invalid{}} =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: "fred", one: 1})
               |> Ash.run_action()

      assert_raise Ash.Error.Invalid, ~r/Invalid Error/, fn ->
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: "fred", one: 1})
        |> Ash.run_action!()
      end
    end

    test "generic actions return the value if they have a return type and a return value" do
      assert 100 =
               Post
               |> Ash.ActionInput.for_action(:typed_with_value, %{})
               |> Ash.run_action!()
    end

    test "generic actions return :ok if they don't have a return type and a return value" do
      assert :ok =
               Post
               |> Ash.ActionInput.for_action(:untyped_without_value, %{})
               |> Ash.run_action!()
    end

    test "generic actions raise if they have a return type but don't have a return value" do
      assert_raise Ash.Error.Framework.InvalidReturnType,
                   ~r/Expected {:ok, result} or {:error, error}/,
                   fn ->
                     Post
                     |> Ash.ActionInput.for_action(:typed_without_value, %{})
                     |> Ash.run_action!()
                   end
    end

    test "generic actions raise if they don't have a return type but have an return value" do
      assert_raise Ash.Error.Framework.InvalidReturnType,
                   ~r/Expected :ok or {:error, error}/,
                   fn ->
                     Post
                     |> Ash.ActionInput.for_action(:untyped_with_value, %{})
                     |> Ash.run_action!()
                   end
    end
  end

  describe "code interfaces" do
    test "accept context" do
      assert "thing" == Post.returns_context!(context: %{thing: "thing"})
    end
  end

  describe "authorization" do
    test "generic actions can be authorized" do
      # assert "Hello fred" =
      #          Post
      #          |> Ash.ActionInput.for_action(:hello, %{name: "fred"})
      #          |> Ash.run_action!(authorize?: true)

      assert_raise Ash.Error.Forbidden, ~r/Forbidden/, fn ->
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: "mike"})
        |> Ash.run_action!(authorize?: true)
      end
    end
  end

  describe "preparations on generic actions" do
    test "preparations can modify action input arguments" do
      input =
        Post
        |> Ash.ActionInput.for_action(:with_preparation, %{name: "world"})

      assert input.arguments.name == "WORLD"

      result =
        input
        |> Ash.run_action!()

      # The preparation should have uppercased the name
      assert result == "Hello WORLD"
    end

    test "preparations can be conditional based on action name" do
      # Test that preparation only affects the specific action
      result =
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: "fred"})
        |> Ash.run_action!()

      # Should not be uppercased since it's not the :with_preparation action
      assert result == "Hello fred"
    end
  end

  describe "validations on generic actions" do
    test "validations can validate action input arguments" do
      # This should succeed
      result =
        Post
        |> Ash.ActionInput.for_action(:with_validation, %{value: 15})
        |> Ash.run_action!()

      assert result == "Value is 15"
    end

    test "validations can fail and prevent action execution" do
      # This should fail validation
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_validation, %{value: 5})
        |> Ash.run_action!()
      end
    end

    test "validation errors are properly formatted" do
      {:error, error} =
        Post
        |> Ash.ActionInput.for_action(:with_validation, %{value: 5})
        |> Ash.run_action()

      assert %Ash.Error.Invalid{} = error
      assert error.errors != []
    end

    test "validations are conditional based on action name" do
      # Test that validation only affects the specific action
      result =
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: "fred"})
        |> Ash.run_action!()

      # Should succeed even though we're not providing a :value argument
      assert result == "Hello fred"
    end
  end

  describe "lifecycle hooks on generic actions" do
    test "before_action hooks can modify input" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "World"})
        |> Ash.ActionInput.before_action(fn input ->
          # Modify the argument in the before_action hook
          Ash.ActionInput.set_argument(input, :message, "Modified World")
        end)
        |> Ash.run_action!()

      assert result == "Processed: Modified World"
    end

    test "after_action hooks can modify result" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "World"})
        |> Ash.ActionInput.after_action(fn _input, result ->
          {:ok, result <> " - Enhanced"}
        end)
        |> Ash.run_action!()

      assert result == "Processed: World - Enhanced"
    end

    test "before_action hooks can add errors" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "World"})
        |> Ash.ActionInput.before_action(fn input ->
          Ash.ActionInput.add_error(input, "Custom error from before_action")
        end)
        |> Ash.run_action!()
      end
    end

    test "after_action hooks can add errors" do
      assert_raise Ash.Error.Unknown, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "World"})
        |> Ash.ActionInput.after_action(fn _input, _result ->
          {:error, "Error from after_action"}
        end)
        |> Ash.run_action!()
      end
    end

    test "before_action hooks can return notifications" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "World"})
        |> Ash.ActionInput.before_action(fn input ->
          notification = %Ash.Notifier.Notification{
            resource: Post,
            action: input.action,
            data: %{before_action: true}
          }

          {input, %{notifications: [notification]}}
        end)
        |> Ash.run_action!()

      assert result == "Processed: World"
    end

    test "after_action hooks can return notifications" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "World"})
        |> Ash.ActionInput.after_action(fn _input, result ->
          notification = %Ash.Notifier.Notification{
            resource: Post,
            data: %{after_action: true}
          }

          {:ok, result, [notification]}
        end)
        |> Ash.run_action!()

      assert result == "Processed: World"
    end

    test "multiple before_action hooks run in order" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "test"})
        |> Ash.ActionInput.before_action(fn input ->
          # First hook: add prefix
          current = Ash.ActionInput.get_argument(input, :message)
          Ash.ActionInput.set_argument(input, :message, "first_" <> current)
        end)
        |> Ash.ActionInput.before_action(fn input ->
          # Second hook: add another prefix
          current = Ash.ActionInput.get_argument(input, :message)
          Ash.ActionInput.set_argument(input, :message, "second_" <> current)
        end)
        |> Ash.run_action!()

      assert result == "Processed: second_first_test"
    end

    test "multiple after_action hooks run in order" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "test"})
        |> Ash.ActionInput.after_action(fn _input, result ->
          {:ok, result <> "_first"}
        end)
        |> Ash.ActionInput.after_action(fn _input, result ->
          {:ok, result <> "_second"}
        end)
        |> Ash.run_action!()

      assert result == "Processed: test_first_second"
    end
  end

  describe "full lifecycle integration on generic actions" do
    test "preparations, validations, before_action, action, and after_action run in correct order" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_preparation, %{name: "test"})
        |> Ash.ActionInput.before_action(fn input ->
          # Add a prefix in before_action (after preparation has uppercased)
          current_name = Ash.ActionInput.get_argument(input, :name)
          Ash.ActionInput.set_argument(input, :name, "PREFIX_" <> current_name)
        end)
        |> Ash.ActionInput.after_action(fn _input, result ->
          {:ok, result <> "_SUFFIX"}
        end)
        |> Ash.run_action!()

      assert result == "Hello PREFIX_TEST_SUFFIX"
    end

    test "validation failure prevents before_action and action execution" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_validation, %{value: 5})
        |> Ash.ActionInput.before_action(fn input ->
          # This should not run because validation fails
          Ash.ActionInput.set_argument(input, :value, 999)
        end)
        |> Ash.run_action!()
      end
    end

    test "before_action error prevents action and after_action execution" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "test"})
        |> Ash.ActionInput.before_action(fn input ->
          Ash.ActionInput.add_error(input, "Before action error")
        end)
        |> Ash.ActionInput.after_action(fn _input, result ->
          # This should not run because before_action fails
          {:ok, result <> "_should_not_appear"}
        end)
        |> Ash.run_action!()
      end
    end
  end

  describe "notifications on generic actions" do
    test "actions can return notifications" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_notifications, %{message: "test"})
        |> Ash.run_action!()

      assert result == "Processed: test"
      # Note: We can't easily test that notifications were actually sent without
      # setting up a notifier, but we can verify the action completes successfully
    end

    test "lifecycle hooks can add notifications to action notifications" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_notifications, %{message: "test"})
        |> Ash.ActionInput.before_action(fn input ->
          notification = %Ash.Notifier.Notification{
            resource: Post,
            data: %{from: "before_action"}
          }

          {input, %{notifications: [notification]}}
        end)
        |> Ash.ActionInput.after_action(fn _input, result ->
          notification = %Ash.Notifier.Notification{
            resource: Post,
            data: %{from: "after_action"}
          }

          {:ok, result, [notification]}
        end)
        |> Ash.run_action!()

      assert result == "Processed: test"
    end
  end

  describe "action-level preparations and validations" do
    test "action-level preparation modifies input" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_action_preparation, %{name: "UPPERCASE"})
        |> Ash.run_action!()

      # Action preparation should have lowercased the name
      assert result == "Action prepared: uppercase"
    end

    test "action-level validation passes when valid" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_action_validation, %{count: 10})
        |> Ash.run_action!()

      assert result == "Count is valid: 10"
    end

    test "action-level validation fails when invalid" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_action_validation, %{count: 3})
        |> Ash.run_action!()
      end
    end

    test "action-level and global preparations/validations work together" do
      result =
        Post
        |> Ash.ActionInput.for_action(:with_mixed_preparations_validations, %{text: "test_value"})
        |> Ash.run_action!()

      assert result == "Final result: action_prep_test_value"
    end

    test "action-level validation failure prevents action execution" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_mixed_preparations_validations, %{
          text: "no_necessary_word"
        })
        |> Ash.run_action!()
      end
    end
  end

  describe "error handling in generic actions with lifecycle" do
    test "invalid before_action hook return value raises appropriate error" do
      assert_raise RuntimeError, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "test"})
        |> Ash.ActionInput.before_action(fn _input ->
          # Invalid return - should return input or {input, notifications}
          "invalid return"
        end)
        |> Ash.run_action!()
      end
    end

    test "invalid after_action hook return value raises appropriate error" do
      assert_raise RuntimeError, ~r/Invalid return value from after_action hook/, fn ->
        Post
        |> Ash.ActionInput.for_action(:with_lifecycle_hooks, %{message: "test"})
        |> Ash.ActionInput.after_action(fn _input, _result ->
          # Invalid return - should return {:ok, result} or {:ok, result, notifications} or {:error, error}
          "invalid return"
        end)
        |> Ash.run_action!()
      end
    end
  end

  describe "generic actions wrapping a reactor" do
    defmodule EchoReactor do
      @moduledoc false
      use Reactor

      input :input

      step :echo do
        argument :echo, input(:input)
        run &Map.fetch(&1, :echo)
      end
    end

    defmodule EchoReactorWithOptionalInput do
      @moduledoc false
      use Reactor

      input :input
      input :something_else

      step :echo do
        argument :echo, input(:input)
        run &Map.fetch(&1, :echo)
      end
    end

    defmodule EchoResource do
      @moduledoc false
      use Ash.Resource, domain: Domain

      actions do
        action :echo, :string do
          argument :input, :string, allow_nil?: false

          run EchoReactor
        end

        action :echo2, :string do
          argument :input, :string, allow_nil?: false
          argument :something_else, :string

          run EchoReactorWithOptionalInput
        end
      end

      code_interface do
        define :echo, args: [:input]
        define :echo2, args: [:input]
      end
    end

    test "it automatically runs the reactor" do
      assert {:ok, "Marty"} = EchoResource.echo("Marty")
    end

    test "it does not require setting optional inputs" do
      assert {:ok, "Marty"} = EchoResource.echo2("Marty")
    end
  end
end
