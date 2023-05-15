defmodule Ash.Test.Type.ModuleTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset
  require Ash.Query

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
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :module, :module
    end
  end

  defmodule ModuleAttrWithBehaviourConstraint do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :module, :module do
        constraints behaviour: StinkyBehaviour
      end
    end
  end

  defmodule ModuleAttrWithProtocolConstraint do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :module, :module do
        constraints protocol: StinkyProtocol
      end
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry ModuleAttr
      entry ModuleAttrWithBehaviourConstraint
      entry ModuleAttrWithProtocolConstraint
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "module attribute with no constraints" do
    ModuleAttr
    |> new(%{module: GenericModule})
    |> Api.create!()
  end

  test "module attribute with behaviour constraint when the module complies" do
    ModuleAttrWithBehaviourConstraint
    |> new(%{module: StinkyModule})
    |> Api.create!()
  end

  test "module attribute with behaviour constraint when the module is not compliant" do
    assert_raise Ash.Error.Invalid, fn ->
      ModuleAttrWithBehaviourConstraint
      |> new(%{module: GenericModule})
      |> Api.create!()
    end
  end

  test "module attribute with protocol constraint when the module complies" do
    ModuleAttrWithProtocolConstraint
    |> new(%{module: StinkyStruct})
    |> Api.create!()
  end

  test "module attribute with protocol constraint when the module is not compliant" do
    assert_raise Ash.Error.Invalid, fn ->
      ModuleAttrWithProtocolConstraint
      |> new(%{module: GenericModule})
      |> Api.create!()
    end
  end
end
