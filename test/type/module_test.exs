defmodule Ash.Test.Type.ModuleTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule StinkyBehaviour do
    @moduledoc false
    @callback stinky? :: boolean
  end

  defmodule StinkyModule do
    @moduledoc false
    @behaviour StinkyBehaviour
    def stinky?, do: true
  end

  defprotocol StinkyProtocol do
    @moduledoc false
    def stinky?(_)
  end

  defmodule StinkyStruct do
    @moduledoc false
    defstruct stinky?: true

    defimpl StinkyProtocol do
      def stinky?(stinky), do: stinky.stinky?
    end
  end

  defmodule GenericModule do
    @moduledoc false
  end

  defmodule ModuleAttr do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :module, :module do
        public?(true)
      end
    end
  end

  defmodule ModuleAttrWithBehaviourConstraint do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :module, :module do
        public?(true)
        constraints behaviour: StinkyBehaviour
      end
    end
  end

  defmodule ModuleAttrWithProtocolConstraint do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :module, :module do
        public?(true)
        constraints protocol: StinkyProtocol
      end
    end
  end

  test "module attribute with no constraints" do
    ModuleAttr
    |> Ash.Changeset.for_create(:create, %{module: GenericModule})
    |> Domain.create!()
  end

  test "module attribute with behaviour constraint when the module complies" do
    ModuleAttrWithBehaviourConstraint
    |> Ash.Changeset.for_create(:create, %{module: StinkyModule})
    |> Domain.create!()
  end

  test "module attribute with behaviour constraint when the module is not compliant" do
    assert_raise Ash.Error.Invalid, fn ->
      ModuleAttrWithBehaviourConstraint
      |> Ash.Changeset.for_create(:create, %{module: GenericModule})
      |> Domain.create!()
    end
  end

  test "module attribute with protocol constraint when the module complies" do
    ModuleAttrWithProtocolConstraint
    |> Ash.Changeset.for_create(:create, %{module: StinkyStruct})
    |> Domain.create!()
  end

  test "module attribute with protocol constraint when the module is not compliant" do
    assert_raise Ash.Error.Invalid, fn ->
      ModuleAttrWithProtocolConstraint
      |> Ash.Changeset.for_create(:create, %{module: GenericModule})
      |> Domain.create!()
    end
  end
end
