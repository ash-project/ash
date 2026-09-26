# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.ValidateMultitenancyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Spark.Test

  test "attribute strategy without an attribute set produces a clear error" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:multitenancy, :attribute]} do
        defmodule Elixir.NoAttributePost do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          multitenancy do
            strategy :attribute
          end
        end
      end

    assert error.message =~ "no `attribute` is configured"
  end

  test "attribute strategy with a non-existent attribute produces an error" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:multitenancy, :attribute]} do
        defmodule Elixir.MissingAttributePost do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          multitenancy do
            strategy :attribute
            attribute :org_id
          end
        end
      end

    assert error.message =~ "does not exist"
  end

  test "ancestor_attributes with a non-existent attribute produces an error" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:multitenancy, :ancestor_attributes]} do
        defmodule Elixir.MissingAncestorAttributePost do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
            attribute :department_id, :uuid, public?: true
          end

          multitenancy do
            strategy :attribute
            attribute :department_id
            ancestor_attributes([:organization_id])
          end
        end
      end

    assert error.message =~ "does not exist"
  end

  test "ancestor_attributes listing the whole hierarchy including the attribute produces an error" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:multitenancy, :ancestor_attributes]} do
        defmodule Elixir.WholeHierarchyAncestorAttributesPost do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
            attribute :organization_id, :uuid, public?: true
            attribute :department_id, :uuid, public?: true
          end

          multitenancy do
            strategy :attribute
            attribute :department_id
            ancestor_attributes([:organization_id, :department_id])
          end
        end
      end

    assert error.message =~ "not the tenant attribute itself"
  end

  test "ancestor_attributes without the attribute strategy produces an error" do
    error =
      assert_dsl_error %Spark.Error.DslError{path: [:multitenancy, :ancestor_attributes]} do
        defmodule Elixir.ContextAncestorAttributePost do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Mnesia

          attributes do
            uuid_primary_key :id
            attribute :organization_id, :uuid, public?: true
          end

          multitenancy do
            strategy :context
            ancestor_attributes([:organization_id])
          end
        end
      end

    assert error.message =~ "requires the `:attribute` strategy"
  end

  test "attribute strategy with valid ancestor_attributes is valid" do
    refute_dsl_errors do
      defmodule Elixir.ValidAncestorAttributesPost do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :organization_id, :uuid, public?: true
          attribute :department_id, :uuid, public?: true
          attribute :team_id, :uuid, public?: true
        end

        multitenancy do
          strategy :attribute
          attribute :team_id
          ancestor_attributes([:organization_id, :department_id])
        end
      end
    end

    assert Ash.Resource.Info.multitenancy_ancestor_attributes(Elixir.ValidAncestorAttributesPost) ==
             [:organization_id, :department_id]

    assert Ash.Resource.Info.multitenancy_attributes(Elixir.ValidAncestorAttributesPost) ==
             [:organization_id, :department_id, :team_id]
  end

  test "attribute strategy with a real attribute is valid" do
    refute_dsl_errors do
      defmodule Elixir.ValidMultitenancyPost do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :org_id, :uuid, public?: true
        end

        multitenancy do
          strategy :attribute
          attribute :org_id
        end
      end
    end

    assert Ash.Resource.Info.multitenancy_strategy(Elixir.ValidMultitenancyPost) == :attribute
    assert Ash.Resource.Info.multitenancy_attribute(Elixir.ValidMultitenancyPost) == :org_id
  end
end
