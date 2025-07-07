defmodule Ash.Test.Resource.Verifiers.VerifySpecComplianceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  describe "Ash.Resource.Spec compliance verification" do
    defmodule AuditableSpec do
      use Spark.Dsl.Fragment, of: Ash.Resource

      attributes do
        attribute :created_at, :utc_datetime_usec
        attribute :updated_at, :utc_datetime_usec
        attribute :created_by_id, :uuid
      end
    end

    defmodule TimestampSpec do
      use Spark.Dsl.Fragment, of: Ash.Resource

      attributes do
        create_timestamp :inserted_at
        update_timestamp :modified_at
      end
    end

    test "resource implementing spec compiles successfully" do
      defmodule CompliantResource do
        use Ash.Resource,
          domain: Ash.Test.Domain,
          specs: [AuditableSpec]

        attributes do
          uuid_primary_key :id
          attribute :name, :string
          attribute :created_at, :utc_datetime_usec
          attribute :updated_at, :utc_datetime_usec
          attribute :created_by_id, :uuid
        end

        actions do
          defaults [:read, :create, :update, :destroy]
        end
      end

      assert CompliantResource
    end

    test "resource implementing multiple specs compiles successfully" do
      defmodule MultiSpecResource do
        use Ash.Resource,
          domain: Ash.Test.Domain,
          specs: [AuditableSpec, TimestampSpec]

        attributes do
          uuid_primary_key :id
          attribute :name, :string
          # Satisfies AuditableSpec
          attribute :created_at, :utc_datetime_usec
          attribute :updated_at, :utc_datetime_usec
          attribute :created_by_id, :uuid
          # Satisfies TimestampSpec (note: create_timestamp has allow_nil? false by default)
          create_timestamp :inserted_at
          update_timestamp :modified_at
        end

        actions do
          defaults [:read, :create, :update, :destroy]
        end
      end

      assert MultiSpecResource
    end

    test "resource missing required attribute from spec raises error" do
      assert_raise Spark.Error.DslError,
                   ~r/does not implement required attribute :created_by_id/,
                   fn ->
                     defmodule MissingAttributeResource do
                       use Ash.Resource,
                         domain: Ash.Test.Domain,
                         specs: [AuditableSpec]

                       attributes do
                         uuid_primary_key :id
                         attribute :name, :string
                         attribute :created_at, :utc_datetime_usec
                         attribute :updated_at, :utc_datetime_usec
                         # Missing :created_by_id required by AuditableSpec
                       end

                       actions do
                         defaults [:read, :create, :update, :destroy]
                       end
                     end
                   end
    end

    test "resource with wrong attribute type raises error" do
      assert_raise Spark.Error.DslError,
                   ~r/type mismatch.*expected.*Ash\.Type\.UtcDatetimeUsec.*got.*Ash\.Type\.String/,
                   fn ->
                     defmodule WrongTypeResource do
                       use Ash.Resource,
                         domain: Ash.Test.Domain,
                         specs: [AuditableSpec]

                       attributes do
                         uuid_primary_key :id
                         attribute :name, :string
                         # Wrong type! Should be :utc_datetime_usec
                         attribute :created_at, :string
                         attribute :updated_at, :utc_datetime_usec
                         attribute :created_by_id, :uuid
                       end

                       actions do
                         defaults [:read, :create, :update, :destroy]
                       end
                     end
                   end
    end

    test "resource without specs compiles successfully" do
      defmodule NoSpecResource do
        use Ash.Resource,
          domain: Ash.Test.Domain

        attributes do
          uuid_primary_key :id
          attribute :name, :string
        end

        actions do
          defaults [:read, :create, :update, :destroy]
        end
      end

      assert NoSpecResource
    end

    test "can query spec information" do
      attributes = Ash.Resource.Spec.attributes(AuditableSpec)
      assert length(attributes) == 3

      names = Enum.map(attributes, & &1.name)
      assert :created_at in names
      assert :updated_at in names
      assert :created_by_id in names
    end

    defmodule ComplexSpec do
      use Spark.Dsl.Fragment, of: Ash.Resource

      attributes do
        attribute :name, :string
        attribute :status, :string
      end

      relationships do
        belongs_to :author, Author
      end

      actions do
        read :by_name do
          argument :name, :string, allow_nil?: false
        end
      end
    end

    test "verifies compliance for multiple entity types (attributes, relationships, actions)" do
      defmodule CompliantMultiEntityResource do
        use Ash.Resource,
          domain: Ash.Test.Domain,
          specs: [ComplexSpec]

        attributes do
          uuid_primary_key :id
          attribute :name, :string
          attribute :status, :string
        end

        relationships do
          belongs_to :author, Author
        end

        actions do
          defaults [:read, :create, :update, :destroy]
          
          read :by_name do
            argument :name, :string, allow_nil?: false
          end
        end
      end

      assert CompliantMultiEntityResource
    end

    test "raises error when missing required relationship from spec" do
      assert_raise Spark.Error.DslError, ~r/does not implement required relationship :author/, fn ->
        defmodule MissingRelationshipResource do
          use Ash.Resource,
            domain: Ash.Test.Domain,
            specs: [ComplexSpec]

          attributes do
            uuid_primary_key :id
            attribute :name, :string
            attribute :status, :string
          end

          # Missing belongs_to :author relationship required by ComplexSpec

          actions do
            defaults [:read, :create, :update, :destroy]
            
            read :by_name do
              argument :name, :string, allow_nil?: false
            end
          end
        end
      end
    end

    test "raises error when missing required action from spec" do
      assert_raise Spark.Error.DslError, ~r/does not implement required action :by_name/, fn ->
        defmodule MissingActionResource do
          use Ash.Resource,
            domain: Ash.Test.Domain,
            specs: [ComplexSpec]

          attributes do
            uuid_primary_key :id
            attribute :name, :string
            attribute :status, :string
          end

          relationships do
            belongs_to :author, Author
          end

          actions do
            defaults [:read, :create, :update, :destroy]
            # Missing read :by_name action required by ComplexSpec
          end
        end
      end
    end
  end
end
