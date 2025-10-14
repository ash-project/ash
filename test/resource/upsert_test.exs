# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.UpsertTest do
  @moduledoc false
  use ExUnit.Case, async: true
  import Ash.Expr

  alias Ash.Test.Domain, as: Domain

  defmodule LazyLoadVariants do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_action(changeset, fn _changeset, record ->
        Ash.load(record, :variants, lazy?: true)
      end)
    end
  end

  defmodule ProductCatalog.Product do
    @moduledoc false
    alias ProductCatalog.Variant, warn: false

    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Domain,
      extensions: []

    code_interface do
      define(:create)
      define(:upsert_variants, args: [:variants])
      define :upsert
      define :upsert_condition
    end

    changes do
      change(LazyLoadVariants)
    end

    identities do
      identity :unique_name, [:name] do
        pre_check_with Domain
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :upsert_variants do
        require_atomic? false
        transaction?(true)

        argument :variants, {:array, :map} do
          allow_nil?(false)
        end

        change(
          manage_relationship(:variants, :variants,
            on_lookup: :ignore,
            on_no_match: :create,
            on_match: :update,
            use_identities: [:_primary_key, :sku]
          )
        )
      end

      create :upsert do
        upsert? true
        upsert_identity :unique_name
        upsert_fields({:replace, [:name]})
      end

      create :upsert_condition do
        upsert? true
        upsert_identity :unique_name
        upsert_condition expr(other == ^actor(:name))
      end
    end

    attributes do
      uuid_primary_key(:id)

      attribute :name, :string do
        public?(true)
      end

      attribute :other, :string do
        public?(true)
      end
    end

    relationships do
      has_many :variants, ProductCatalog.Variant do
        public?(true)
        destination_attribute(:product_id)
      end
    end
  end

  defmodule ProductCatalog.Variant do
    @moduledoc false
    alias ProductCatalog.Product, warn: false

    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      extensions: []

    identities do
      identity(:sku, [:sku], pre_check_with: Domain)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      uuid_primary_key(:id)

      attribute :product_id, :uuid do
        public?(true)
        allow_nil?(false)
      end

      attribute :sku, :string do
        public?(true)
        allow_nil?(false)
      end

      attribute(:color, :string, public?: true)
    end

    relationships do
      belongs_to :product, ProductCatalog.Product do
        public?(true)
      end
    end
  end

  alias ProductCatalog.Product

  describe "atomics" do
    test "atomics can be added to a changeset" do
      product =
        Product
        |> Ash.Changeset.for_create(:create, %{name: "foo"},
          upsert?: true,
          upsert_identity: :unique_name
        )
        |> Ash.Changeset.atomic_update(:name, expr(name <> " bar"))
        |> Ash.create!()

      assert product.name == "foo"

      updated =
        Product
        |> Ash.Changeset.for_create(:create, %{name: "foo"},
          upsert?: true,
          upsert_identity: :unique_name
        )
        |> Ash.Changeset.atomic_update(:name, expr(name <> " bar"))
        |> Ash.create!()

      assert updated.id == product.id
      assert updated.name == "foo bar"
    end
  end

  describe "upsert and load children" do
    test "Product.upsert_variants" do
      {:ok, p} = Product.create(%{})

      v = %{sku: "SOME_SKU", color: "Red"}
      v_updated = %{sku: v.sku, color: "Blue"}

      %{variants: [expected]} = Product.upsert_variants!(p, [v])

      assert expected.color == v.color

      # v_1 gets matched by sku and then updated
      assert %{variants: [expected_updated]} = Product.upsert_variants!(p, [v_updated])

      assert expected_updated.color == v_updated.color
    end

    test "upsert with upsert_fields" do
      product = Product.upsert!(%{name: "fred", other: "george"})
      assert product.name == "fred"
      assert product.other == "george"

      product = Product.upsert!(%{name: "fred", other: "malfoy"})
      assert product.other == "george"
    end

    test "upsert with :replace_all" do
      product = Product.upsert!(%{name: "fred", other: "george"})
      assert product.name == "fred"
      assert product.other == "george"

      product =
        Product
        |> Ash.Changeset.for_create(:upsert, %{name: "fred", other: "malfoy"},
          upsert_fields: :replace_all
        )
        |> Ash.create!()

      assert product.other == "malfoy"
    end

    test "upsert with replace list" do
      product = Product.upsert!(%{name: "fred", other: "george"})
      assert product.name == "fred"
      assert product.other == "george"

      product =
        Product
        |> Ash.Changeset.for_create(:upsert, %{name: "fred", other: "malfoy"},
          upsert_fields: [:name]
        )
        |> Ash.create!()

      assert product.other == "george"
    end

    test "upsert with :replace" do
      product = Product.upsert!(%{name: "fred", other: "george"})
      assert product.name == "fred"
      assert product.other == "george"

      product =
        Product
        |> Ash.Changeset.for_create(:upsert, %{name: "fred", other: "malfoy"},
          upsert_fields: {:replace, [:name]}
        )
        |> Ash.create!()

      assert product.other == "george"
    end

    test "upsert with :replace_all_except" do
      product = Product.upsert!(%{name: "fred", other: "george"})
      assert product.name == "fred"
      assert product.other == "george"

      product =
        Product
        |> Ash.Changeset.for_create(:upsert, %{name: "fred", other: "malfoy"},
          upsert_fields: {:replace_all_except, [:other]}
        )
        |> Ash.create!()

      assert product.other == "george"
    end
  end

  describe "upsert with upsert_condition referencing actor" do
    test "with correct actor" do
      Product.upsert_condition!(%{name: "fred", other: "george"}, actor: %{name: "george"})
      Product.upsert_condition!(%{name: "fred", other: "george"}, actor: %{name: "george"})
    end

    test "with wrong actor" do
      Product.upsert_condition!(%{name: "fred", other: "george"}, actor: %{name: "george"})

      assert_raise Ash.Error.Invalid, fn ->
        Product.upsert_condition!(%{name: "fred", other: "george"}, actor: %{name: "bob"})
      end
    end

    test "with no actor" do
      assert_raise Ash.Error.Invalid, fn ->
        Product.upsert_condition!(%{name: "fred", other: "george"})
      end
    end
  end

  describe "upsert with false condition" do
    test "upsert with expr(false) condition fails on conflict" do
      # Create initial product
      Product.upsert!(%{name: "John", other: "initial"})

      # This should fail because the upsert_condition is false
      assert {:error, _} =
               Product
               |> Ash.Changeset.for_create(:create, %{name: "John", other: "updated"},
                 upsert?: true,
                 upsert_identity: :unique_name,
                 upsert_condition: expr(false)
               )
               |> Ash.create()
    end
  end
end
