defmodule Ash.Test.Resource.Actions.ExtendActionsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Info

  defmacrop defresource(name, do: actions_block) do
    quote do
      defmodule unquote(name) do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :name, :string, public?: true
        end

        actions do
          unquote(actions_block)
        end
      end
    end
  end

  describe "list field concatenation" do
    test "concatenates preparations with base first" do
      defresource PrepConcat do
        read :base do
          prepare build(sort: [:id])
        end

        read :extended do
          extends :base
          prepare build(limit: 10)
        end
      end

      base = Info.action(PrepConcat, :base)
      extended = Info.action(PrepConcat, :extended)

      assert length(extended.preparations) == 2
      assert Enum.at(extended.preparations, 0) == Enum.at(base.preparations, 0)
    end

    test "concatenates arguments from base and extending action" do
      defresource ArgConcat do
        read :base do
          argument :from_base, :string
        end

        read :extended do
          extends :base
          argument :from_extended, :integer
        end
      end

      extended = Info.action(ArgConcat, :extended)
      arg_names = Enum.map(extended.arguments, & &1.name)

      assert arg_names == [:from_base, :from_extended]
    end

    test "concatenates changes with base first" do
      defresource ChangeConcat do
        create :base do
          accept [:name]
          change set_attribute(:name, "from_base")
        end

        create :extended do
          extends :base
          change set_attribute(:name, "from_extended")
        end
      end

      base = Info.action(ChangeConcat, :base)
      extended = Info.action(ChangeConcat, :extended)

      assert length(extended.changes) == 2
      assert Enum.at(extended.changes, 0) == Enum.at(base.changes, 0)
    end
  end

  describe "scalar field inheritance" do
    test "inherits scalar fields from base" do
      defresource ScalarInherit do
        read :base do
          get? true
          transaction? true
        end

        read :extended do
          extends :base
        end
      end

      extended = Info.action(ScalarInherit, :extended)

      assert extended.get? == true
      assert extended.transaction? == true
    end

    test "overrides scalar fields when explicitly set" do
      defresource ScalarOverride do
        read :base do
          get? true
        end

        read :extended do
          extends :base
          get? false
        end
      end

      extended = Info.action(ScalarOverride, :extended)

      assert extended.get? == false
    end

    test "inherits accept from base" do
      defresource AcceptInherit do
        update :base do
          accept [:name]
        end

        update :extended do
          extends :base
        end
      end

      extended = Info.action(AcceptInherit, :extended)

      assert extended.accept == [:name]
    end

    test "does not inherit primary?" do
      defresource PrimaryExcluded do
        read :base do
          primary? true
        end

        read :extended do
          extends :base
        end
      end

      assert Info.action(PrimaryExcluded, :base).primary? == true
      refute Info.action(PrimaryExcluded, :extended).primary?
    end
  end

  describe "all action types" do
    test "create" do
      defresource CreateExtend do
        create :base do
          accept [:name]
          argument :token, :string
          change set_attribute(:name, "default")
        end

        create :extended do
          extends :base
          argument :extra, :string
        end
      end

      extended = Info.action(CreateExtend, :extended)

      assert extended.accept == [:name]
      assert length(extended.changes) == 1
      assert Enum.map(extended.arguments, & &1.name) == [:token, :extra]
    end

    test "update" do
      defresource UpdateExtend do
        update :base do
          accept [:name]
          argument :reason, :string
        end

        update :extended do
          extends :base
          argument :extra, :string
        end
      end

      extended = Info.action(UpdateExtend, :extended)

      assert extended.accept == [:name]
      assert Enum.map(extended.arguments, & &1.name) == [:reason, :extra]
    end

    test "destroy" do
      defresource DestroyExtend do
        destroy :base do
          argument :reason, :string
        end

        destroy :extended do
          extends :base
          argument :extra, :string
        end
      end

      extended = Info.action(DestroyExtend, :extended)

      assert Enum.map(extended.arguments, & &1.name) == [:reason, :extra]
    end

    test "generic action" do
      defresource GenericExtend do
        action :base, :string do
          argument :input, :string

          run fn _input, _context ->
            {:ok, "result"}
          end
        end

        action :extended, :string do
          extends :base
          argument :extra, :integer
        end
      end

      extended = Info.action(GenericExtend, :extended)

      assert Enum.map(extended.arguments, & &1.name) == [:input, :extra]
      assert extended.run != nil
    end
  end

  describe "error cases" do
    test "raises when base action does not exist" do
      assert_raise Spark.Error.DslError, ~r/no action named `nonexistent` exists/, fn ->
        defresource MissingBase do
          read :bad do
            extends :nonexistent
          end
        end
      end
    end

    test "raises when action types don't match" do
      assert_raise Spark.Error.DslError, ~r/must be the same type/, fn ->
        defresource TypeMismatch do
          create :base do
            accept []
          end

          read :bad do
            extends :base
          end
        end
      end
    end
  end
end
