# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.SubjectTest do
  use ExUnit.Case, async: true

  defmodule Post do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      primary_read_warning?: false

    actions do
      defaults [:destroy, :update]
      default_accept [:title, :body, :published, :tenant_id]

      read :read do
        primary? true

        argument :name, :string, public?: false
      end

      create :create do
        primary? true
        argument :name, :string, public?: false
      end

      action :custom_action, :struct do
        argument :name, :string, public?: false
        run fn input, _ -> {:ok, input} end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
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
    case subject_type do
      Ash.ActionInput -> Ash.ActionInput.for_action(Post, :custom_action, %{})
      _ -> apply(subject_type, :new, [Post])
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
        result = %{subject | arguments: %{age: 42}}

        assert Ash.Subject.get_argument(result, :age) == 42
        assert is_struct(result, subject_type)
      end
    end

    test "gets argument with string key" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result = %{subject | arguments: %{"age" => 42}}

        assert Ash.Subject.get_argument(result, "age") == 42
        assert is_struct(result, subject_type)
      end
    end

    test "returns default when argument not found" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        assert Ash.Subject.get_argument(subject, :missing_arg, "default_value") == "default_value"
        assert Ash.Subject.get_argument(subject, "missing_arg", :default_atom) == :default_atom
        assert is_struct(subject, subject_type)
      end
    end

    test "returns argument value when found, ignoring default" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result = %{subject | arguments: %{name: "actual_value"}}

        assert Ash.Subject.get_argument(result, :name, "default_value") == "actual_value"
        assert is_struct(result, subject_type)
      end
    end
  end

  describe "fetch_argument/2 - All Subject Types" do
    test "fetches existing argument" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result = %{subject | arguments: %{"age" => 42, name: "value"}}

        assert {:ok, "value"} = Ash.Subject.fetch_argument(result, :name)
        assert {:ok, 42} = Ash.Subject.fetch_argument(result, "age")
        assert :error = Ash.Subject.fetch_argument(result, :missing)
        assert is_struct(result, subject_type)
      end
    end

    test "supports atom to string conversion" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        # Set an argument that works for all types
        result = %{subject | arguments: %{"name" => "value"}}

        assert {:ok, "value"} = Ash.Subject.fetch_argument(result, :name)
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
      for subject_type <- [Ash.Changeset, Ash.Query, Ash.ActionInput] do
        subject = new_subject(subject_type)

        subject = %{subject | arguments: %{custom_arg: "arg_value"}}

        assert Ash.Subject.get_argument_or_attribute(subject, :custom_arg) == "arg_value"
        assert is_struct(subject, subject_type)
      end
    end

    test "gets attribute for Changeset, falls back to argument for others" do
      # For Changeset - tests both argument priority and attribute fallback
      changeset = Ash.Changeset.new(%Post{title: "Existing Title"})

      assert Ash.Subject.get_argument_or_attribute(changeset, :title) == "Existing Title"

      changeset = %{changeset | arguments: %{title: "New Title"}}

      assert Ash.Subject.get_argument_or_attribute(changeset, :title) == "New Title"

      for subject_type <- [Ash.Query, Ash.ActionInput] do
        subject = new_subject(subject_type)
        subject = %{subject | arguments: %{name: "New Title"}}
        assert Ash.Subject.get_argument_or_attribute(subject, :name) == "New Title"
        assert is_struct(subject, subject_type)
      end
    end

    test "returns default when not found" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        assert Ash.Subject.get_argument_or_attribute(subject, :missing, 42) == 42
      end
    end

    test "returns actual value when found, ignoring default" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        subject = %{subject | arguments: %{name: "arg_value"}}

        assert Ash.Subject.get_argument_or_attribute(subject, :name, "default") ==
                 "arg_value"

        assert is_struct(subject, subject_type)
      end
    end
  end

  describe "set_argument/3 - All Subject Types" do
    test "sets argument" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result = %{subject | arguments: %{name: "value1"}}

        assert Ash.Subject.get_argument(result, :name) == "value1"
        assert is_struct(result, subject_type)
      end
    end
  end

  describe "set_arguments/2 - All Subject Types" do
    test "sets multiple arguments" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        result = Ash.Subject.set_arguments(subject, %{name: "value1"})

        assert is_struct(result, subject_type)
        assert result.arguments.name == "value1"
      end
    end
  end

  describe "delete_argument/2 - All Subject Types" do
    test "deletes single argument" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        subject = %{subject | arguments: %{name: "value1"}}

        result = Ash.Subject.delete_argument(subject, :name)

        assert is_struct(result, subject_type)
        refute Map.has_key?(result.arguments, :name)
      end
    end

    test "deletes multiple arguments" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)

        subject = %{subject | arguments: %{name: "value1", age: 42}}

        result = Ash.Subject.delete_argument(subject, [:name, :age])

        assert is_struct(result, subject_type)
        refute Map.has_key?(result.arguments, :name)
        refute Map.has_key?(result.arguments, :age)
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

  describe "before_transaction/2-3 - All Subject Types" do
    test "registers before_transaction callback" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback = fn subj -> subj end

        result = Ash.Subject.before_transaction(subject, callback)

        assert is_struct(result, subject_type)
        assert length(result.before_transaction) == 1
        assert [^callback] = result.before_transaction
      end
    end

    test "handles prepend option" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback1 = fn subj -> subj end
        callback2 = fn subj -> subj end

        result =
          subject
          |> Ash.Subject.before_transaction(callback1)
          |> Ash.Subject.before_transaction(callback2, prepend?: true)

        assert is_struct(result, subject_type)
        assert length(result.before_transaction) == 2
        assert [^callback2, ^callback1] = result.before_transaction
      end
    end

    test "appends callbacks by default" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback1 = fn subj -> subj end
        callback2 = fn subj -> subj end
        callback3 = fn subj -> subj end

        result =
          subject
          |> Ash.Subject.before_transaction(callback1)
          |> Ash.Subject.before_transaction(callback2)
          |> Ash.Subject.before_transaction(callback3)

        assert is_struct(result, subject_type)
        assert length(result.before_transaction) == 3
        assert [^callback1, ^callback2, ^callback3] = result.before_transaction
      end
    end
  end

  describe "after_transaction/2-3 - All Subject Types" do
    test "registers after_transaction callback" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback = fn _subj, result -> result end

        result = Ash.Subject.after_transaction(subject, callback)

        assert is_struct(result, subject_type)
        assert length(result.after_transaction) == 1
        assert [^callback] = result.after_transaction
      end
    end

    test "handles prepend option" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback1 = fn _subj, result -> result end
        callback2 = fn _subj, result -> result end

        result =
          subject
          |> Ash.Subject.after_transaction(callback1)
          |> Ash.Subject.after_transaction(callback2, prepend?: true)

        assert is_struct(result, subject_type)
        assert length(result.after_transaction) == 2
        assert [^callback2, ^callback1] = result.after_transaction
      end
    end

    test "appends callbacks by default" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback1 = fn _subj, result -> result end
        callback2 = fn _subj, result -> result end
        callback3 = fn _subj, result -> result end

        result =
          subject
          |> Ash.Subject.after_transaction(callback1)
          |> Ash.Subject.after_transaction(callback2)
          |> Ash.Subject.after_transaction(callback3)

        assert length(result.after_transaction) == 3
        assert [^callback1, ^callback2, ^callback3] = result.after_transaction
      end
    end
  end

  describe "around_transaction/2-3 - All Subject Types" do
    test "registers around_transaction callback" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback = fn subj, func -> func.(subj) end

        result = Ash.Subject.around_transaction(subject, callback)

        assert is_struct(result, subject_type)
        assert length(result.around_transaction) == 1
        assert [^callback] = result.around_transaction
      end
    end

    test "handles prepend option" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback1 = fn subj, func -> func.(subj) end
        callback2 = fn subj, func -> func.(subj) end

        result =
          subject
          |> Ash.Subject.around_transaction(callback1)
          |> Ash.Subject.around_transaction(callback2, prepend?: true)

        assert is_struct(result, subject_type)
        assert length(result.around_transaction) == 2
        assert [^callback2, ^callback1] = result.around_transaction
      end
    end

    test "appends callbacks by default" do
      for subject_type <- @subject_types do
        subject = new_subject(subject_type)
        callback1 = fn subj, func -> func.(subj) end
        callback2 = fn subj, func -> func.(subj) end
        callback3 = fn subj, func -> func.(subj) end

        result =
          subject
          |> Ash.Subject.around_transaction(callback1)
          |> Ash.Subject.around_transaction(callback2)
          |> Ash.Subject.around_transaction(callback3)

        assert length(result.around_transaction) == 3
        assert [^callback1, ^callback2, ^callback3] = result.around_transaction
      end
    end
  end
end
