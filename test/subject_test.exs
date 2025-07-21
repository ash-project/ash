defmodule Ash.SubjectTest do
  use ExUnit.Case, async: true

  defmodule Post do
    use Ash.Resource,
      domain: Ash.Test.Domain

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      action :custom_action, :struct do
        argument :name, :string, allow_nil?: false
        run fn input, _ -> {:ok, input} end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false
      attribute :body, :string
      attribute :published, :boolean, default: false
      attribute :tenant_id, :string
    end

    multitenancy do
      strategy :attribute
      attribute :tenant_id
    end
  end

  # Test constants and helpers
  @test_errors [
    [field: :title, message: "is required"],
    [field: :body, message: "is too short"]
  ]

  @subject_types [
    Ash.Changeset,
    Ash.Query,
    Ash.ActionInput
  ]

  # Helper function to create subjects consistently for each type
  defp new_subject(subject_type) do
    subject = apply(subject_type, :new, [Post])

    if subject_type == Ash.ActionInput do
      # ActionInput needs an action to properly handle arguments
      Ash.ActionInput.for_action(subject, :custom_action, %{name: "test"})
    else
      subject
    end
  end

  describe "add_error/2 - All Subject Types" do
    test "adds string error" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        result = Ash.Subject.add_error(subject, "Test error")

        assert is_struct(result, subject_type)
        assert length(result.errors) == 1
        refute result.valid?

        # Handle different error structures
        [error | _] = result.errors

        case subject_type do
          Ash.Query -> assert error.error == "Test error"
          _ -> assert error.message == "Test error"
        end
      end
    end

    test "adds error list" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        result = Ash.Subject.add_error(subject, @test_errors)

        assert is_struct(result, subject_type)
        assert length(result.errors) == 2
        refute result.valid?
      end
    end

    test "handles empty error list" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        result = Ash.Subject.add_error(subject, [])

        assert is_struct(result, subject_type)
        assert result.errors == []
        assert result.valid?
      end
    end

    test "adds multiple errors sequentially" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result =
          subject
          |> Ash.Subject.add_error("Error 1")
          |> Ash.Subject.add_error("Error 2")

        assert is_struct(result, subject_type)
        assert length(result.errors) == 2
        refute result.valid?
      end
    end
  end

  describe "put_context/3 - All Subject Types" do
    test "puts single key-value pair" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result = Ash.Subject.put_context(subject, :custom, "value")

        assert result.context.custom == "value"
      end
    end
  end

  describe "set_context/2 - All Subject Types" do
    test "sets entire context map" do
      context = %{key1: "value1", key2: "value2"}

      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result = Ash.Subject.set_context(subject, context)

        assert result.context.key1 == "value1"
        assert result.context.key2 == "value2"
      end
    end

    test "handles nil context" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result = Ash.Subject.set_context(subject, nil)

        assert result == subject
      end
    end

    test "deep merges context" do
      initial = %{level1: %{a: 1, b: 2}}
      update = %{level1: %{b: 3, c: 4}}

      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result =
          subject
          |> Ash.Subject.set_context(initial)
          |> Ash.Subject.set_context(update)

        assert result.context.level1 == %{a: 1, b: 3, c: 4}
      end
    end

    test "handles shared context merging" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result = Ash.Subject.set_context(subject, %{shared: %{key: "value"}, other: "data"})

        assert result.context.key == "value"
        assert result.context.other == "data"
      end
    end
  end

  describe "get_argument/2,3 - All Subject Types" do
    test "gets argument with atom key" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result =
          case subject_type do
            Ash.ActionInput -> Ash.Subject.set_argument(subject, :name, "value")
            _ -> Ash.Subject.set_argument(subject, :custom_arg, "value")
          end

        expected_arg = if subject_type == Ash.ActionInput, do: :name, else: :custom_arg
        assert Ash.Subject.get_argument(result, expected_arg) == "value"
        assert Ash.Subject.get_argument(result, :non_existent) == nil
        assert is_struct(result, subject_type)
      end
    end

    test "gets argument with string key" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result =
          case subject_type do
            Ash.ActionInput -> Ash.Subject.set_argument(subject, :name, "value")
            _ -> Ash.Subject.set_argument(subject, :test_arg, "value")
          end

        # Should find argument by string key even though it was set with atom
        expected_arg_str = if subject_type == Ash.ActionInput, do: "name", else: "test_arg"
        assert Ash.Subject.get_argument(result, expected_arg_str) == "value"
        assert is_struct(result, subject_type)
      end
    end

    test "returns default when argument not found" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        assert Ash.Subject.get_argument(subject, :missing_arg, "default_value") == "default_value"
        assert Ash.Subject.get_argument(subject, "missing_arg", :default_atom) == :default_atom
        assert Ash.Subject.get_argument(subject, :missing_arg, 42) == 42
        assert is_struct(subject, subject_type)
      end
    end

    test "returns argument value when found, ignoring default" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result =
          case subject_type do
            Ash.ActionInput -> Ash.Subject.set_argument(subject, :name, "actual_value")
            _ -> Ash.Subject.set_argument(subject, :existing_arg, "actual_value")
          end

        expected_arg = if subject_type == Ash.ActionInput, do: :name, else: :existing_arg
        expected_arg_str = if subject_type == Ash.ActionInput, do: "name", else: "existing_arg"

        assert Ash.Subject.get_argument(result, expected_arg, "default_value") == "actual_value"

        assert Ash.Subject.get_argument(result, expected_arg_str, "default_value") ==
                 "actual_value"

        assert is_struct(result, subject_type)
      end
    end
  end

  describe "fetch_argument/2 - All Subject Types" do
    test "fetches existing argument" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result =
          case subject_type do
            Ash.ActionInput -> Ash.Subject.set_argument(subject, :name, "value")
            _ -> Ash.Subject.set_argument(subject, :custom_arg, "value")
          end

        expected_arg = if subject_type == Ash.ActionInput, do: :name, else: :custom_arg
        assert {:ok, "value"} = Ash.Subject.fetch_argument(result, expected_arg)
        assert :error = Ash.Subject.fetch_argument(result, :missing)
        assert is_struct(result, subject_type)
      end
    end

    test "supports atom to string conversion" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result =
          case subject_type do
            Ash.ActionInput -> Ash.Subject.set_argument(subject, :name, "value")
            _ -> Ash.Subject.set_argument(subject, :test_arg, "value")
          end

        expected_arg_str = if subject_type == Ash.ActionInput, do: "name", else: "test_arg"
        assert {:ok, "value"} = Ash.Subject.fetch_argument(result, expected_arg_str)
        assert is_struct(result, subject_type)
      end
    end
  end

  describe "get_attribute/2 - Changeset Only" do
    test "gets attribute value" do
      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "Test Title")
        |> Ash.Changeset.change_attribute(:body, "Test Body")

      assert Ash.Subject.get_attribute(changeset, :title) == "Test Title"
      assert Ash.Subject.get_attribute(changeset, :body) == "Test Body"
    end

    test "returns nil for non-existent attribute" do
      changeset = Ash.Changeset.new(Post)

      assert Ash.Subject.get_attribute(changeset, :non_existent) == nil
    end

    test "gets attribute from data when no changes exist" do
      # Create a struct with data
      post_data = %Post{title: "Existing Title", body: "Existing Body"}
      changeset = Ash.Changeset.new(post_data)

      assert Ash.Subject.get_attribute(changeset, :title) == "Existing Title"
      assert Ash.Subject.get_attribute(changeset, :body) == "Existing Body"
    end

    test "prioritizes changes over data" do
      # Create a struct with data
      post_data = %Post{title: "Existing Title", body: "Existing Body"}

      changeset =
        post_data
        |> Ash.Changeset.new()
        |> Ash.Changeset.change_attribute(:title, "Updated Title")

      # Should get the changed value, not the original data
      assert Ash.Subject.get_attribute(changeset, :title) == "Updated Title"
      # Should get the original data value when no change exists
      assert Ash.Subject.get_attribute(changeset, :body) == "Existing Body"
    end
  end

  describe "get_argument_or_attribute/2,3 - All Subject Types" do
    test "gets argument when exists for all types" do
      # Test Changeset and Query
      for subject_type <- [Ash.Changeset, Ash.Query] do
        subject =
          new_subject(subject_type)
          |> Ash.Subject.set_argument(:custom_arg, "arg_value")

        assert Ash.Subject.get_argument_or_attribute(subject, :custom_arg) == "arg_value"
        assert is_struct(subject, subject_type)
      end

      # Test ActionInput
      input = Ash.ActionInput.for_action(Post, :custom_action, %{name: "arg_value"})
      assert Ash.Subject.get_argument_or_attribute(input, :name) == "arg_value"
      assert is_struct(input, Ash.ActionInput)
    end

    test "gets attribute for Changeset, falls back to argument for others" do
      # For Changeset - tests both argument priority and attribute fallback
      changeset =
        Post
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_argument(:custom_arg, "arg_value")
        |> Ash.Changeset.change_attribute(:title, "Test Title")

      # Should get argument when both exist
      assert Ash.Subject.get_argument_or_attribute(changeset, :custom_arg) == "arg_value"
      # Should get attribute when no argument exists
      assert Ash.Subject.get_argument_or_attribute(changeset, :title) == "Test Title"

      # For Query - only arguments
      query =
        Post
        |> Ash.Query.new()
        |> Ash.Query.set_argument(:test_arg, "test_value")

      assert Ash.Subject.get_argument_or_attribute(query, :test_arg) == "test_value"
      assert is_struct(query, Ash.Query)

      # For ActionInput - only arguments
      input = Ash.ActionInput.for_action(Post, :custom_action, %{name: "test_value"})
      assert Ash.Subject.get_argument_or_attribute(input, :name) == "test_value"
      assert is_struct(input, Ash.ActionInput)
    end

    test "returns default when not found" do
      for subject_type <- @subject_types do
        subject =
          case subject_type do
            Ash.Changeset -> Ash.Changeset.new(Post)
            Ash.Query -> Ash.Query.new(Post)
            Ash.ActionInput -> Ash.ActionInput.new(Post)
          end

        assert Ash.Subject.get_argument_or_attribute(subject, :missing, "default_value") ==
                 "default_value"

        assert Ash.Subject.get_argument_or_attribute(subject, :missing, 42) == 42
      end
    end

    test "returns actual value when found, ignoring default" do
      for subject_type <- @subject_types do
        subject =
          case subject_type do
            Ash.Changeset ->
              Post
              |> Ash.Changeset.new()
              |> Ash.Changeset.set_argument(:arg_name, "arg_value")
              |> Ash.Changeset.change_attribute(:title, "Title Value")

            Ash.Query ->
              Post
              |> Ash.Query.new()
              |> Ash.Query.set_argument(:arg_name, "arg_value")

            Ash.ActionInput ->
              Ash.ActionInput.for_action(Post, :custom_action, %{name: "arg_value"})
          end

        # Should get argument value and ignore default
        expected_arg = if subject_type == Ash.ActionInput, do: :name, else: :arg_name

        assert Ash.Subject.get_argument_or_attribute(subject, expected_arg, "default") ==
                 "arg_value"

        # For Changeset, also test attribute value
        if subject_type == Ash.Changeset do
          assert Ash.Subject.get_argument_or_attribute(subject, :title, "default") ==
                   "Title Value"
        end

        assert is_struct(subject, subject_type)
      end
    end
  end

  describe "set_argument/3 - All Subject Types" do
    test "sets argument" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result =
          case subject_type do
            Ash.ActionInput -> Ash.Subject.set_argument(subject, :name, "value1")
            _ -> Ash.Subject.set_argument(subject, :arg1, "value1")
          end

        expected_arg = if subject_type == Ash.ActionInput, do: :name, else: :arg1
        assert Ash.Subject.get_argument(result, expected_arg) == "value1"
        assert is_struct(result, subject_type)
      end
    end
  end

  describe "set_arguments/2 - All Subject Types" do
    test "sets multiple arguments" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Use arguments that work for each type
        args =
          case subject_type do
            # ActionInput can only set defined action arguments
            Ash.ActionInput -> %{name: "value1"}
            _ -> %{arg1: "value1", arg2: "value2", arg3: "value3"}
          end

        result = Ash.Subject.set_arguments(subject, args)

        if subject_type == Ash.ActionInput do
          assert Ash.Subject.get_argument(result, :name) == "value1"
        else
          assert Ash.Subject.get_argument(result, :arg1) == "value1"
          assert Ash.Subject.get_argument(result, :arg2) == "value2"
          assert Ash.Subject.get_argument(result, :arg3) == "value3"
        end

        assert is_struct(result, subject_type)
      end
    end

    test "handles empty map" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result = Ash.Subject.set_arguments(subject, %{})

        assert is_struct(result, subject_type)
      end
    end
  end

  describe "delete_argument/2 - All Subject Types" do
    test "deletes single argument" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set arguments that work for each type first
        with_args =
          case subject_type do
            Ash.ActionInput ->
              # ActionInput starts with name="test", add more if possible
              Ash.Subject.set_argument(subject, :name, "v1")

            _ ->
              Ash.Subject.set_arguments(subject, %{arg1: "v1", arg2: "v2", arg3: "v3"})
          end

        result =
          if subject_type == Ash.ActionInput do
            # For ActionInput, we can only delete the name argument
            delete_result = Ash.Subject.delete_argument(with_args, :name)
            refute Map.has_key?(delete_result.arguments, :name)
            delete_result
          else
            delete_result = Ash.Subject.delete_argument(with_args, :arg1)
            refute Map.has_key?(delete_result.arguments, :arg1)
            assert Map.has_key?(delete_result.arguments, :arg2)
            delete_result
          end

        assert is_struct(result, subject_type)
      end
    end

    test "deletes multiple arguments" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set arguments that work for each type first
        with_args =
          case subject_type do
            Ash.ActionInput ->
              # ActionInput only has name argument defined, so we can only test deleting that
              subject

            _ ->
              Ash.Subject.set_arguments(subject, %{arg1: "v1", arg2: "v2", arg3: "v3"})
          end

        result =
          if subject_type == Ash.ActionInput do
            # For ActionInput, delete the name argument
            delete_result = Ash.Subject.delete_argument(with_args, [:name])
            refute Map.has_key?(delete_result.arguments, :name)
            delete_result
          else
            delete_result = Ash.Subject.delete_argument(with_args, [:arg1, :arg2])
            refute Map.has_key?(delete_result.arguments, :arg1)
            refute Map.has_key?(delete_result.arguments, :arg2)
            assert Map.has_key?(delete_result.arguments, :arg3)
            delete_result
          end

        assert is_struct(result, subject_type)
      end
    end

    test "deletes non-existing argument gracefully" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result = Ash.Subject.delete_argument(subject, :non_existent)

        assert is_struct(result, subject_type)
      end
    end
  end

  describe "set_private_argument/3 - Changeset & ActionInput Only" do
    @supported_types [Ash.Changeset, Ash.ActionInput]

    test "sets private argument for supported types" do
      for subject_type <- @supported_types do
        subject =
          case subject_type do
            Ash.Changeset -> Ash.Changeset.for_create(Post, :create, %{title: "Test"})
            Ash.ActionInput -> Ash.ActionInput.for_action(Post, :custom_action, %{name: "test"})
          end

        result = Ash.Subject.set_private_argument(subject, :private_arg, "secret")

        # Private arguments are stored internally - just verify no error
        assert is_struct(result, subject_type)
      end
    end

    test "raises FunctionClauseError for Query (unsupported)" do
      query = Ash.Query.new(Post)

      assert_raise FunctionClauseError, fn ->
        Ash.Subject.set_private_argument(query, :private_arg, "secret")
      end
    end
  end

  describe "before_action/2,3 - All Subject Types" do
    test "registers before_action callback" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback = fn subj -> {:ok, subj} end

        result = Ash.Subject.before_action(subject, callback)

        assert length(result.before_action) == 1
      end
    end

    test "registers before_action callback with options" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback = fn subj -> {:ok, subj} end

        result = Ash.Subject.before_action(subject, callback, prepend?: true)

        assert length(result.before_action) == 1
      end
    end

    test "handles prepend option" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback1 = fn subj -> {:ok, subj} end
        callback2 = fn subj -> {:ok, subj} end

        result =
          subject
          |> Ash.Subject.before_action(callback1)
          |> Ash.Subject.before_action(callback2, prepend?: true)

        assert length(result.before_action) == 2
        assert [^callback2, ^callback1] = result.before_action
      end
    end
  end

  describe "after_action/2,3 - All Subject Types" do
    test "registers after_action callback" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback = fn _subj, result -> {:ok, result} end

        result = Ash.Subject.after_action(subject, callback)

        assert is_struct(result, subject_type)
        assert length(result.after_action) == 1
        assert [^callback] = result.after_action
      end
    end

    test "registers after_action callback with options" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback = fn _subj, result -> {:ok, result} end

        result = Ash.Subject.after_action(subject, callback, prepend?: true)

        assert is_struct(result, subject_type)
        assert length(result.after_action) == 1
      end
    end
  end

  describe "run_before_actions/1 - Changeset & ActionInput Only" do
    test "runs before_actions with no callbacks for supported types" do
      for subject_type <- [Ash.Changeset, Ash.ActionInput] do
        subject = new_subject(subject_type)

        {result, _} = Ash.Subject.run_before_actions(subject)

        assert is_struct(result, subject_type)
      end
    end

    test "runs before_actions with callbacks for supported types" do
      for subject_type <- [Ash.Changeset, Ash.ActionInput] do
        subject = new_subject(subject_type)
        callback = fn subj -> subj end

        subject_with_callback = Ash.Subject.before_action(subject, callback)
        {result, _} = Ash.Subject.run_before_actions(subject_with_callback)

        assert is_struct(result, subject_type)
      end
    end

    test "raises FunctionClauseError for Query (unsupported)" do
      query = Ash.Query.new(Post)

      assert_raise FunctionClauseError, fn ->
        Ash.Subject.run_before_actions(query)
      end
    end
  end

  describe "run_after_actions/3 - Changeset & ActionInput Only" do
    test "runs after_actions for supported types" do
      for subject_type <- [Ash.Changeset, Ash.ActionInput] do
        subject = new_subject(subject_type)

        result_data = %{name: "test"}

        result = Ash.Subject.run_after_actions(result_data, subject, %{notifications: []})

        # run_after_actions returns a complex result
        assert result
      end
    end

    test "raises FunctionClauseError for Query (unsupported)" do
      query = Ash.Query.new(Post)
      result_data = %{data: "test"}

      assert_raise FunctionClauseError, fn ->
        Ash.Subject.run_after_actions(result_data, query, %{notifications: []})
      end
    end
  end
end
