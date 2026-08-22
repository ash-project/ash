# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.AddPeriodAttributeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Temporal.Versioned
  alias Ash.Test.Temporal.VersionedNote

  describe "a resource that names its period and declares nothing" do
    test "gets the attribute, as an inclusive-exclusive range over datetimes" do
      attribute = Ash.Resource.Info.temporal_period(VersionedNote)

      assert attribute.name == :valid_at
      assert attribute.type == Ash.Type.Range
      refute attribute.allow_nil?
      assert attribute.constraints[:inner_type] == Ash.Type.DateTime
      assert attribute.constraints[:lower][:inclusive?]
      refute attribute.constraints[:upper][:inclusive?]
    end

    test "gets it marked generated?, so a create need not supply it" do
      assert Ash.Resource.Info.temporal_period(VersionedNote).generated?
    end

    test "does not get it made public" do
      refute Ash.Resource.Info.temporal_period(VersionedNote).public?
    end

    test "gets it under the name it chose" do
      defmodule NamedPeriod do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.Test.Temporal.StubDataLayer

        temporal do
          attribute :effective_at
        end

        attributes do
          attribute :id, :integer, primary_key?: true, allow_nil?: false
        end
      end

      assert Ash.Resource.Info.temporal_period(NamedPeriod).name == :effective_at
      refute Ash.Resource.Info.attribute(NamedPeriod, :valid_at)
    end
  end

  describe "a resource that declares its own period" do
    test "keeps the type and constraints it declared" do
      assert Ash.Resource.Info.temporal_inner_type(Versioned) == Ash.Type.DateTime
      assert Ash.Resource.Info.temporal_inner_constraints(Versioned)[:precision] == :second
    end

    test "keeps an inner type that is not time at all" do
      defmodule IntegerExtent do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.Test.Temporal.StubDataLayer

        temporal do
          attribute :valid_over
        end

        attributes do
          attribute :id, :integer, primary_key?: true, allow_nil?: false

          attribute :valid_over, Ash.Type.Range,
            allow_nil?: false,
            constraints: [
              inner_type: :integer,
              lower: [inclusive?: true],
              upper: [inclusive?: false]
            ]
        end
      end

      assert Ash.Resource.Info.temporal_inner_type(IntegerExtent) == Ash.Type.Integer
    end

    test "keeps sub-second detail when it asks for it" do
      defmodule MicrosecondPeriod do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.Test.Temporal.StubDataLayer

        temporal do
          attribute :valid_at
        end

        attributes do
          attribute :id, :integer, primary_key?: true, allow_nil?: false

          attribute :valid_at, Ash.Type.Range,
            allow_nil?: false,
            public?: true,
            constraints: [
              inner_type: :datetime,
              inner_constraints: [precision: :microsecond],
              lower: [inclusive?: true],
              upper: [inclusive?: false]
            ]
        end
      end

      instant = ~U[2026-01-01 00:00:00.123456Z]
      constraints = Ash.Resource.Info.temporal_period(MicrosecondPeriod).constraints

      assert {:ok, range} =
               Ash.Type.cast_input(Ash.Type.Range, %{lower: instant, upper: nil}, constraints)

      assert {:ok, %{lower: ^instant}} =
               Ash.Type.apply_constraints(Ash.Type.Range, range, constraints)

      assert Ash.Resource.Info.temporal_period(MicrosecondPeriod).public?
    end

    test "is marked generated? whether or not it said so" do
      assert Ash.Resource.Info.temporal_period(Versioned).generated?

      defmodule PeriodNotMarkedGenerated do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.Test.Temporal.StubDataLayer

        temporal do
          attribute :valid_at
        end

        attributes do
          attribute :id, :integer, primary_key?: true, allow_nil?: false

          attribute :valid_at, Ash.Type.Range,
            allow_nil?: false,
            constraints: [
              inner_type: :datetime,
              lower: [inclusive?: true],
              upper: [inclusive?: false]
            ]
        end
      end

      assert Ash.Resource.Info.temporal_period(PeriodNotMarkedGenerated).generated?
    end
  end

  describe "a declared period is checked" do
    test "must be an Ash.Type.Range" do
      assert_raise Spark.Error.DslError, ~r/to be an `Ash.Type.Range`/, fn ->
        defmodule PeriodIsADatetime do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.Test.Temporal.StubDataLayer

          temporal do
            attribute :valid_at
          end

          attributes do
            attribute :id, :integer, primary_key?: true, allow_nil?: false
            attribute :valid_at, :datetime, allow_nil?: false
          end
        end
      end
    end

    test "must constrain its bounds to inclusive-exclusive" do
      assert_raise Spark.Error.DslError, ~r/It constrains neither bound/, fn ->
        defmodule PeriodWithoutBounds do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.Test.Temporal.StubDataLayer

          temporal do
            attribute :valid_at
          end

          attributes do
            attribute :id, :integer, primary_key?: true, allow_nil?: false

            attribute :valid_at, Ash.Type.Range,
              allow_nil?: false,
              constraints: [inner_type: :datetime]
          end
        end
      end
    end

    test "says which bounds it got when they are the wrong way round" do
      assert_raise Spark.Error.DslError, ~r/Got lower inclusive\? false, upper true/, fn ->
        defmodule PeriodExclusiveInclusive do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.Test.Temporal.StubDataLayer

          temporal do
            attribute :valid_at
          end

          attributes do
            attribute :id, :integer, primary_key?: true, allow_nil?: false

            attribute :valid_at, Ash.Type.Range,
              allow_nil?: false,
              constraints: [
                inner_type: :datetime,
                lower: [inclusive?: false],
                upper: [inclusive?: true]
              ]
          end
        end
      end
    end

    test "must not allow nil" do
      assert_raise Spark.Error.DslError, ~r/not to be `allow_nil\? true`/, fn ->
        defmodule PeriodAllowingNil do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.Test.Temporal.StubDataLayer

          temporal do
            attribute :valid_at
          end

          attributes do
            attribute :id, :integer, primary_key?: true, allow_nil?: false

            attribute :valid_at, Ash.Type.Range,
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

  test "a resource that is not temporal has no period" do
    refute Ash.Resource.Info.temporal_period(Ash.Test.Temporal.Thing)
    refute Ash.Resource.Info.temporal_inner_type(Ash.Test.Temporal.Thing)
    refute Ash.Resource.Info.attribute(Ash.Test.Temporal.Thing, :valid_at)
  end
end
