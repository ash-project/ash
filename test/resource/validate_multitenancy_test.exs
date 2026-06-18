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
