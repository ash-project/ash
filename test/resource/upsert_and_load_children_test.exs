defmodule Ash.Test.Resource.UpsertAndLoadChildrenTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule ProductCatalog do
    @moduledoc """
    An example API which could represent a Product Catalog bounded context
    in an ECommerce or PIM system.

    The example models a situation where a parent resource acts as a consistency boundary
    for the child resource. In this case the product maintains consistency rules for
    its set of child variants. i.e. any changes to the set of variants must be performed through the product.

    In the language of Domain Driven Design, the Product resource is acting as the "Aggregate Root" Entity type.

    The module LazyLoadVariants represents the most simple example of where
    we might want to add a validation to the product that lazy loads the set of variants
    to ensure that a consistency condition is met.
    """
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
        record
        |> ProductCatalog.load(:variants, lazy?: true)

        # !ATTN! Loading the variants seems to affect the upsert behaviour.
        # i.e. Uncommenting the line below fixes the upsert error.
        # {:ok, record}
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
    end

    changes do
      change(LazyLoadVariants)
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
            on_match: :update
          )
        )
      end
    end

    attributes do
      uuid_primary_key(:id)
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
    use Ash.Registry,
      extensions: [
        Ash.Registry.ResourceValidations
      ]

    entries do
      entry(ProductCatalog.Product)
      entry(ProductCatalog.Variant)
    end
  end

  alias ProductCatalog.Product

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
  end
end
