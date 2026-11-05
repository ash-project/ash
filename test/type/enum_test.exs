# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.EnumTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :copy_previous_state do
        require_atomic? true
        change atomic_update(:state, expr(previous_state))
      end

      update :copy_previous_status do
        require_atomic? true
        change atomic_update(:status, expr(previous_status))
      end

      update :copy_raw_value do
        require_atomic? true
        change atomic_update(:state, expr(raw_value))
        change atomic_update(:status, expr(raw_value))
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :status, Status do
        public?(true)
      end

      attribute :previous_status, Status do
        public?(true)
      end

      attribute :state, OverriddenEnum do
        public?(true)
      end

      attribute :previous_state, OverriddenEnum do
        public?(true)
      end

      attribute :raw_value, :string do
        public?(true)
      end
    end
  end

  test "it handles exact matches" do
    Post
    |> Ash.Changeset.for_create(:create, %{status: :open})
    |> Ash.create!()
  end

  test "it handles string matches" do
    Post
    |> Ash.Changeset.for_create(:create, %{status: "open"})
    |> Ash.create!()
  end

  test "it handles mixed case string matches" do
    Post
    |> Ash.Changeset.for_create(:create, %{status: "OpEn"})
    |> Ash.create!()
  end

  test "it handles mixed case string matches against mixed case atoms" do
    Post
    |> Ash.Changeset.for_create(:create, %{status: "nEveRHAppened"})
    |> Ash.create!()
  end

  test "it allows overriding `match/1` and accepting custom input values" do
    Post
    |> Ash.Changeset.for_create(:create, %{status: "never_happened"})
    |> Ash.create!()
  end

  test "it fails on mismatches" do
    assert_raise Ash.Error.Invalid, fn ->
      Post
      |> Ash.Changeset.for_create(:create, %{status: "what"})
      |> Ash.create!()
    end
  end

  describe "atomic updates" do
    test "copying a valid value" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{state: :active, previous_state: :archived})
        |> Ash.create!()
        |> Ash.Changeset.for_update(:copy_previous_state, %{})
        |> Ash.update!()

      assert post.state == :archived
    end

    test "copying a nil value" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{state: :active})
        |> Ash.create!()
        |> Ash.Changeset.for_update(:copy_previous_state, %{})
        |> Ash.update!()

      assert is_nil(post.state)
    end

    test "copying a valid value with mixed case values" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :open, previous_status: :Closed})
        |> Ash.create!()
        |> Ash.Changeset.for_update(:copy_previous_status, %{})
        |> Ash.update!()

      assert post.status == :Closed
    end

    test "copying a nil value with mixed case values" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{status: :open})
        |> Ash.create!()
        |> Ash.Changeset.for_update(:copy_previous_status, %{})
        |> Ash.update!()

      assert is_nil(post.status)
    end

    test "copying an invalid value fails" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{raw_value: "nope"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/must be one of/, fn ->
        post
        |> Ash.Changeset.for_update(:copy_raw_value, %{})
        |> Ash.update!()
      end
    end
  end

  test "the values are returned in the introspection function" do
    assert Status.values() == [:open, :Closed, :NeverHappened, :Always_Was]
    assert Status.match("OPEN") == {:ok, :open}
    assert Status.match?(:always_was)
  end

  test "it handles descriptions" do
    assert DescriptiveEnum.values() == [
             :foo,
             :bar,
             :baz,
             :a_thing_with_no_description,
             :another_thing_with_no_description,
             :with_details
           ]

    assert DescriptiveEnum.description(:foo) == "Clearly a foo"
    assert DescriptiveEnum.description(:a_thing_with_no_description) == nil
    assert DescriptiveEnum.description(:another_thing_with_no_description) == nil
    assert DescriptiveEnum.description(:with_details) == "Look I have a description"
  end

  test "it handles labels and generates them when no label is provided" do
    assert DescriptiveEnum.label(:foo) == "Foo"
    assert DescriptiveEnum.label(:a_thing_with_no_description) == "A thing with no description"

    assert DescriptiveEnum.label(:another_thing_with_no_description) ==
             "Another thing with no description"

    assert DescriptiveEnum.label(:with_details) == "I have a label"
  end

  test "label and description can be overridden" do
    assert OverriddenEnum.label(:active) == "Currently Active"
    assert OverriddenEnum.label(:inactive) == "Inactive"
    assert OverriddenEnum.label(:archived) == "Archived"

    assert OverriddenEnum.description(:active) == "This item is currently active"
    assert OverriddenEnum.description(:inactive) == nil
    assert OverriddenEnum.description(:archived) == nil
  end

  describe "types are correctly generated" do
    test "simple atoms" do
      assert {:ok,
              [
                type:
                  {:t, {:type, 0, :union, [{:atom, 0, :Always_Was}, _, _, {:atom, 0, :open}]}, []}
              ]} = Code.Typespec.fetch_types(Status)
    end

    test "descriptive atoms" do
      assert {:ok,
              [
                type:
                  {:t,
                   {:type, 0, :union,
                    [
                      {:atom, 0, :with_details},
                      {:atom, 0, :another_thing_with_no_description},
                      _,
                      _,
                      _,
                      {:atom, 0, :foo}
                    ]}, []}
              ]} = Code.Typespec.fetch_types(DescriptiveEnum)
    end

    test "strings" do
      assert {:ok, [type: {:t, {:remote_type, 0, [{:atom, 0, String}, {:atom, 0, :t}, []]}, []}]} =
               Code.Typespec.fetch_types(StringEnum)
    end

    test "mixed values" do
      assert {:ok,
              [
                type:
                  {:t,
                   {:type, 0, :union,
                    [
                      {:type, 0, :union,
                       [{:atom, 0, :with_details}, {:atom, 0, :another_one}, {:atom, 0, :foo}]},
                      {:remote_type, 0, [{:atom, 0, String}, {:atom, 0, :t}, []]}
                    ]}, []}
              ]} = Code.Typespec.fetch_types(MixedEnum)
    end
  end
end
