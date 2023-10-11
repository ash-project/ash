defmodule Ash.Test.Resource.UpsertTest do
  @moduledoc false
  use ExUnit.Case, async: true
  import Ash.Expr

  defmodule ProductCatalog do
    use Ash.Api,
      extensions: []

    resources do
      registry(ProductCatalog.Registry)
    end
  end

  defmodule LazyLoadVariants do
    @moduledoc false
    use Ash.Resource.Change
    alias ProductCatalog

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_action(changeset, fn _changeset, record ->
        ProductCatalog.load(record, :variants, lazy?: true)
      end)
    end
  end

  defmodule ProductCatalog.Product do
    @moduledoc false
    alias ProductCatalog.Variant, warn: false

    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      extensions: []

    code_interface do
      define_for(ProductCatalog)

      define(:create)
      define(:upsert_variants, args: [:variants])
      define :upsert
    end

    changes do
      change(LazyLoadVariants)
    end

    identities do
      identity :unique_name, [:name] do
        pre_check_with ProductCatalog
      end
    end

    actions do
      defaults([:create, :update, :read, :destroy])

      update :upsert_variants do
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
        upsert_fields([:name])
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute :name, :string
      attribute :other, :string
    end

    relationships do
      has_many :variants, ProductCatalog.Variant do
        destination_attribute(:product_id)
      end
    end
  end

  defmodule ProductCatalog.Variant do
    @moduledoc false
    alias ProductCatalog.Product, warn: false

    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      extensions: []

    identities do
      identity(:sku, [:sku], pre_check_with: ProductCatalog)
    end

    actions do
      defaults([:create, :read, :update, :destroy])
    end

    attributes do
      uuid_primary_key(:id)

      attribute :product_id, :uuid do
        allow_nil?(false)
      end

      attribute :sku, :string do
        allow_nil?(false)
      end

      attribute(:color, :string)
    end

    relationships do
      belongs_to :product, ProductCatalog.Product do
      end
    end
  end

  defmodule ProductCatalog.Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(ProductCatalog.Product)
      entry(ProductCatalog.Variant)
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
        |> ProductCatalog.create!()

      assert product.name == "foo"

      updated =
        Product
        |> Ash.Changeset.for_create(:create, %{name: "foo"},
          upsert?: true,
          upsert_identity: :unique_name
        )
        |> Ash.Changeset.atomic_update(:name, expr(name <> " bar"))
        |> ProductCatalog.create!()

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
      %{variants: [expected_updated]} = Product.upsert_variants!(p, [v_updated])

      assert expected_updated.color == v_updated.color
    end

    test "upsert with upsert_fields" do
      product = Product.upsert!(%{name: "fred", other: "george"})
      assert product.name == "fred"
      assert product.other == "george"

      product = Product.upsert!(%{name: "fred", other: "malfoy"})
      assert product.other == "george"
    end
  end
end
