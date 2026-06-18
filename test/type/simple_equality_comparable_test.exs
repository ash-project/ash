# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.SimpleEqualityComparableTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule CiStringNewType do
    use Ash.Type.NewType, subtype_of: :ci_string
  end

  defmodule DecimalNewType do
    use Ash.Type.NewType, subtype_of: :decimal
  end

  defmodule Parent do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      attribute :id, :ci_string do
        primary_key?(true)
        allow_nil?(false)
        public?(true)
      end
    end

    relationships do
      has_many(:children, Ash.Test.Type.SimpleEqualityComparableTest.Child,
        destination_attribute: :parent_id,
        public?: true
      )

      many_to_many :tags, Ash.Test.Type.SimpleEqualityComparableTest.Tag do
        through(Ash.Test.Type.SimpleEqualityComparableTest.ParentTag)
        source_attribute_on_join_resource(:parent_id)
        destination_attribute_on_join_resource(:tag_id)
        public?(true)
      end
    end
  end

  defmodule Tag do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])

      # Paginated read used to force the lateral-join attach path.
      read :paginated do
        pagination do
          offset? true
          default_limit 100
          countable true
        end
      end
    end

    attributes do
      attribute :id, :ci_string do
        primary_key?(true)
        allow_nil?(false)
        public?(true)
      end
    end
  end

  defmodule ParentTag do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      uuid_primary_key(:id)
    end

    # A unique join makes the m2m eligible for the lateral-join path.
    identities do
      identity :unique_pair, [:parent_id, :tag_id], pre_check_with: Domain
    end

    relationships do
      belongs_to :parent, Ash.Test.Type.SimpleEqualityComparableTest.Parent do
        attribute_type(:ci_string)
        public?(true)
      end

      belongs_to :tag, Ash.Test.Type.SimpleEqualityComparableTest.Tag do
        attribute_type(:ci_string)
        public?(true)
      end
    end
  end

  defmodule Child do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])

      # Paginated read used to force the lateral-join attach path.
      read :paginated do
        pagination do
          offset? true
          default_limit 100
          countable true
        end
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:label, :string, public?: true)
    end

    relationships do
      belongs_to :parent, Ash.Test.Type.SimpleEqualityComparableTest.Parent do
        attribute_type(:ci_string)
        public?(true)
      end
    end
  end

  # Resource with a composite PK that mixes a ci_string and an integer.
  # Exercises the multi-field branch of pkey_normalizer/2.
  defmodule CompositeParent do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      attribute :id, :ci_string, primary_key?: true, allow_nil?: false, public?: true
      attribute :version, :integer, primary_key?: true, allow_nil?: false, public?: true
    end
  end

  # Resource with a non-comparable PK (decimal). Used purely to assert
  # primary_key_simple_equality_comparable?/1 can return false.
  defmodule DecimalKeyResource do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, create: :*])
    end

    attributes do
      attribute :id, :decimal, primary_key?: true, allow_nil?: false, public?: true
    end
  end

  describe "Ash.Type.simple_equality_comparable?/1" do
    test "is true for simple-equality types" do
      assert Ash.Type.simple_equality_comparable?(:string)
      assert Ash.Type.simple_equality_comparable?(:integer)
      assert Ash.Type.simple_equality_comparable?(:uuid)
    end

    test "is true for :ci_string via the callback" do
      assert Ash.Type.simple_equality_comparable?(:ci_string)
    end

    test "is true for array of comparable types" do
      assert Ash.Type.simple_equality_comparable?({:array, :ci_string})
      assert Ash.Type.simple_equality_comparable?({:array, :string})
    end

    test "is true for a NewType wrapping a comparable subtype" do
      assert Ash.Type.simple_equality_comparable?(CiStringNewType)
    end

    test "is false for types without a comparable form" do
      refute Ash.Type.simple_equality_comparable?(:decimal)
    end

    test "is false for a NewType wrapping a non-comparable subtype" do
      refute Ash.Type.simple_equality_comparable?(DecimalNewType)
    end
  end

  describe "Ash.Type.to_simple_equality_comparable/2" do
    test "returns the value unchanged for simple-equality types" do
      assert "foo" == Ash.Type.to_simple_equality_comparable(:string, "foo")
      assert 42 == Ash.Type.to_simple_equality_comparable(:integer, 42)
    end

    test "downcases CiString values regardless of input shape" do
      assert "foo" == Ash.Type.to_simple_equality_comparable(:ci_string, "FoO")

      assert "foo" ==
               Ash.Type.to_simple_equality_comparable(:ci_string, %Ash.CiString{
                 string: "FoO"
               })

      assert "foo" ==
               Ash.Type.to_simple_equality_comparable(:ci_string, %Ash.CiString{
                 string: "foo",
                 case: :lower,
                 casted?: true
               })
    end

    test "produces equal terms for differently-cased CiStrings" do
      a = Ash.Type.to_simple_equality_comparable(:ci_string, "FRED")
      b = Ash.Type.to_simple_equality_comparable(:ci_string, "fReD")
      c = Ash.Type.to_simple_equality_comparable(:ci_string, %Ash.CiString{string: "fred"})

      assert a == b
      assert b == c
    end

    test "passes nil through" do
      assert nil == Ash.Type.to_simple_equality_comparable(:ci_string, nil)
    end

    test "normalizes each element of an array" do
      assert ["foo", "bar"] ==
               Ash.Type.to_simple_equality_comparable({:array, :ci_string}, ["FoO", "BAR"])
    end

    test "raises for types without a comparable form" do
      assert_raise ArgumentError, ~r/has no simple-equality-comparable form/, fn ->
        Ash.Type.to_simple_equality_comparable(:decimal, Decimal.new("1.0"))
      end
    end

    test "NewType delegates to its subtype" do
      assert "foo" == Ash.Type.to_simple_equality_comparable(CiStringNewType, "FoO")
    end

    test "NewType wrapping a non-comparable subtype raises" do
      assert_raise ArgumentError, ~r/has no simple-equality-comparable form/, fn ->
        Ash.Type.to_simple_equality_comparable(DecimalNewType, Decimal.new("1.0"))
      end
    end
  end

  describe "Ash.Resource.Info.primary_key_simple_equality_comparable?/1" do
    test "is true when all pkey attributes are comparable" do
      assert Ash.Resource.Info.primary_key_simple_equality_comparable?(Parent)
    end

    test "is true for a composite pkey of comparable types" do
      assert Ash.Resource.Info.primary_key_simple_equality_comparable?(CompositeParent)
    end

    test "is false when any pkey attribute has no comparable form" do
      refute Ash.Resource.Info.primary_key_simple_equality_comparable?(DecimalKeyResource)
    end

    test "Parent's pkey is not simple_equality? (so the integration tests below exercise the comparable path, not the identity path)" do
      refute Ash.Resource.Info.primary_key_simple_equality?(Parent)
    end
  end

  describe "loading has_many across a CI string PK" do
    setup do
      for id <- ["ALPHA", "Beta", "Gamma"] do
        Parent
        |> Ash.Changeset.for_create(:create, %{id: id})
        |> Ash.create!()
      end

      # Children reference parents with various casings — should all still match.
      for {parent_id, label} <- [
            {"alpha", "a1"},
            {"ALPHA", "a2"},
            {"aLpHa", "a3"},
            {"BETA", "b1"},
            {"beta", "b2"}
          ] do
        Child
        |> Ash.Changeset.for_create(:create, %{parent_id: parent_id, label: label})
        |> Ash.create!()
      end

      :ok
    end

    test "main attach path: stitches children regardless of case" do
      parents = Parent |> Ash.read!() |> Ash.load!(:children)
      result = group_labels(parents)

      assert result["ALPHA"] == ["a1", "a2", "a3"]
      assert result["Beta"] == ["b1", "b2"]
      assert result["Gamma"] == []
    end

    test "lazy attach path: re-load already-loaded children with lazy?: true" do
      parents = Parent |> Ash.read!() |> Ash.load!(:children)
      reloaded = Ash.load!(parents, [:children], lazy?: true)
      result = group_labels(reloaded)

      assert result["ALPHA"] == ["a1", "a2", "a3"]
      assert result["Beta"] == ["b1", "b2"]
      assert result["Gamma"] == []
    end

    test "lateral attach path: paginated children stitch back to mixed-case parents" do
      children_query =
        Child
        |> Ash.Query.for_read(:paginated)
        |> Ash.Query.page(limit: 100)

      parents = Parent |> Ash.read!() |> Ash.load!(children: children_query)

      # Pattern-matching Ash.Page.Offset confirms the paginated read fired —
      # which is what triggers the lateral-join attach path under the hood.
      result =
        Map.new(parents, fn parent ->
          %Ash.Page.Offset{results: results} = parent.children
          {to_string(parent.id), results |> Enum.map(& &1.label) |> Enum.sort()}
        end)

      assert result["ALPHA"] == ["a1", "a2", "a3"]
      assert result["Beta"] == ["b1", "b2"]
      assert result["Gamma"] == []
    end
  end

  describe "loading many_to_many across a CI string join key" do
    setup do
      for id <- ["Red", "Blue", "Green"] do
        Tag
        |> Ash.Changeset.for_create(:create, %{id: id})
        |> Ash.create!()
      end

      for id <- ["ALPHA", "Beta"] do
        Parent
        |> Ash.Changeset.for_create(:create, %{id: id})
        |> Ash.create!()
      end

      # Join rows whose parent_id and tag_id are cased differently from the
      # records they reference — the m2m stitch must match case-insensitively.
      for {parent_id, tag_id} <- [
            {"alpha", "red"},
            {"ALPHA", "BLUE"},
            {"beta", "rEd"}
          ] do
        ParentTag
        |> Ash.Changeset.for_create(:create, %{parent_id: parent_id, tag_id: tag_id})
        |> Ash.create!()
      end

      :ok
    end

    test "separate-query attach path: stitches tags back onto parents regardless of case" do
      parents = Parent |> Ash.read!() |> Ash.load!(:tags)

      result =
        Map.new(parents, fn parent ->
          {to_string(parent.id), parent.tags |> Enum.map(&to_string(&1.id)) |> Enum.sort()}
        end)

      assert result == %{"ALPHA" => ["Blue", "Red"], "Beta" => ["Red"]}
    end

    test "lateral attach path: paginated tags stitch back to mixed-case parents" do
      tags_query = Tag |> Ash.Query.for_read(:paginated) |> Ash.Query.page(limit: 100)
      parents = Parent |> Ash.read!() |> Ash.load!(tags: tags_query)

      result =
        Map.new(parents, fn parent ->
          %Ash.Page.Offset{results: results} = parent.tags
          {to_string(parent.id), results |> Enum.map(&to_string(&1.id)) |> Enum.sort()}
        end)

      assert result == %{"ALPHA" => ["Blue", "Red"], "Beta" => ["Red"]}
    end
  end

  defp group_labels(parents) do
    Map.new(parents, fn parent ->
      {to_string(parent.id), parent.children |> Enum.map(& &1.label) |> Enum.sort()}
    end)
  end
end
