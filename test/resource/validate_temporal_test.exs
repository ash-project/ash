# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.ValidateTemporalTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Spark.Test

  alias Ash.Test.Temporal.Versioned
  alias Ash.Test.Temporal.VersionedNote

  describe "declaring a resource temporal" do
    test "is accepted by a data layer that supports it" do
      assert Ash.Resource.Info.temporal?(Versioned)
      assert Ash.Resource.Info.temporal_strategy(Versioned) == :context
      assert Ash.Resource.Info.temporal_attribute(Versioned) == :valid_at
    end

    test "is refused by a data layer that does not" do
      error =
        assert_dsl_error %Spark.Error.DslError{path: [:temporal, :strategy]} do
          defmodule UnsupportedTemporal do
            @moduledoc false
            use Ash.Resource, domain: Ash.Test.Domain

            temporal do
              strategy :context
              attribute :valid_at
            end

            attributes do
              uuid_primary_key :id
            end
          end
        end

      assert error.message =~ "Data layer does not support temporal resources"
    end

    test "leaves a resource that declares no strategy alone" do
      refute_dsl_errors do
        defmodule NotTemporal do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end
        end
      end

      refute Ash.Resource.Info.temporal?(__MODULE__.NotTemporal)
    end
  end

  describe "the period attribute" do
    test "is refused as action input" do
      assert_raise Spark.Error.DslError, ~r/must not be accepted as input/, fn ->
        defmodule AcceptsPeriod do
          @moduledoc false

          use Ash.Resource,
            domain: Ash.Test.Domain,
            data_layer: Ash.Test.Temporal.StubDataLayer

          temporal do
            strategy :context
            attribute :valid_at
          end

          actions do
            create :create do
              accept [:id, :valid_at]
            end
          end

          attributes do
            attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true

            attribute :valid_at, Ash.Type.Range,
              allow_nil?: false,
              generated?: true,
              constraints: [
                inner_type: :datetime,
                lower: [inclusive?: true],
                upper: [inclusive?: false]
              ]
          end
        end
      end
    end
  end

  describe "relationships involving a temporal resource" do
    test "overlap in time once both sides declare their period" do
      assert %Ash.Query.Call{
               name: :range_overlaps,
               args: [
                 %Ash.Query.Parent{expr: {:_ref, [], :valid_at}},
                 {:_ref, [], :valid_at}
               ]
             } = Ash.Resource.Info.relationship(Versioned, :notes).filter
    end

    test "must declare temporal_keys" do
      error =
        assert_dsl_error %Spark.Error.DslError{path: [:relationships, :notes]} do
          defmodule UnkeyedRelationship do
            @moduledoc false
            use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

            attributes do
              attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
            end

            relationships do
              has_many :notes, Ash.Test.Temporal.VersionedNote do
                no_attributes? true
              end
            end
          end
        end

      assert error.message =~ "must set `temporal_keys {nil, :valid_at}`"
    end

    test "must not be keyed by the foreign key" do
      error =
        assert_dsl_error %Spark.Error.DslError{path: [:relationships, :notes]} do
          defmodule AttributeKeyedRelationship do
            @moduledoc false
            use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

            attributes do
              attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
            end

            relationships do
              has_many :notes, Ash.Test.Temporal.VersionedNote do
                destination_attribute :versioned_id
                temporal_keys {nil, :valid_at}
              end
            end
          end
        end

      assert error.message =~ "must set `no_attributes? true`"
    end

    test "are left alone when neither side is temporal" do
      refute_dsl_errors do
        defmodule NonTemporalRelationship do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
          end

          relationships do
            has_many :others, Ash.Test.Temporal.Thing do
              destination_attribute :id
            end
          end
        end
      end
    end
  end

  test "a temporal resource on both sides keys each side's period" do
    assert Ash.Resource.Info.relationship(VersionedNote, :versioned).temporal_keys ==
             {:valid_at, :valid_at}
  end
end
