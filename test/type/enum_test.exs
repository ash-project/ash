defmodule Ash.Test.Type.EnumTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Status do
    use Ash.Type.Enum, values: [:open, :Closed, :NeverHappened, :Always_Was]
  end

  defmodule DescriptiveEnum do
    use Ash.Type.Enum,
      values: [
        {:foo, "Clearly a foo"},
        {:bar, "Obviously a bar"},
        {:baz, "Undoubtedly a baz"},
        :a_thing_with_no_description,
        {:another_thing_with_no_description, nil}
      ]
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :status, Status do
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

  test "it fails on mismatches" do
    assert_raise Ash.Error.Invalid, fn ->
      Post
      |> Ash.Changeset.for_create(:create, %{status: "what"})
      |> Ash.create!()
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
             :another_thing_with_no_description
           ]

    assert DescriptiveEnum.description(:foo) == "Clearly a foo"
    assert DescriptiveEnum.description(:a_thing_with_no_description) == nil
    assert DescriptiveEnum.description(:another_thing_with_no_description) == nil
  end
end
