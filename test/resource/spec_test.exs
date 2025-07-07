defmodule Ash.Test.Resource.SpecTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule TimestampedSpec do
    use Spark.Dsl.Fragment, of: Ash.Resource

    attributes do
      attribute :created_at, :utc_datetime_usec
      attribute :updated_at, :utc_datetime_usec
    end
  end

  defmodule Post do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      specs: [TimestampedSpec]

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :created_at, :utc_datetime_usec
      attribute :updated_at, :utc_datetime_usec
    end

    actions do
      defaults [:read, :create, :update, :destroy]
    end
  end

  test "can define a spec with attributes" do
    attributes = Ash.Resource.Spec.attributes(TimestampedSpec)
    assert length(attributes) == 2
    assert Enum.any?(attributes, &(&1.name == :created_at))
    assert Enum.any?(attributes, &(&1.name == :updated_at))
  end

  test "can use specs option in resource" do
    # Just testing that it compiles for now
    assert Post
  end

  test "verifies spec compliance - missing attribute" do
    assert_raise Spark.Error.DslError, ~r/does not implement required attribute/, fn ->
      defmodule MissingAttributePost do
        use Ash.Resource,
          domain: Ash.Test.Domain,
          specs: [TimestampedSpec]

        attributes do
          uuid_primary_key :id
          attribute :title, :string
          # Missing :created_at and :updated_at required by TimestampedSpec
        end

        actions do
          defaults [:read, :create, :update, :destroy]
        end
      end
    end
  end

  test "verifies spec compliance - type mismatch" do
    assert_raise Spark.Error.DslError, ~r/type mismatch/, fn ->
      defmodule TypeMismatchPost do
        use Ash.Resource,
          domain: Ash.Test.Domain,
          specs: [TimestampedSpec]

        attributes do
          uuid_primary_key :id
          attribute :title, :string
          # Wrong type! Should be :utc_datetime_usec
          attribute :created_at, :string
          attribute :updated_at, :utc_datetime_usec
        end

        actions do
          defaults [:read, :create, :update, :destroy]
        end
      end
    end
  end
end
