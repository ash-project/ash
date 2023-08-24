defmodule Ash.Test.Type.Binary do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset
  require Ash.Query

  @binary <<255, 216, 255, 224, 0, 16, 74, 70, 73, 70, 0, 1>>

  defmodule Embedded do
    @moduledoc false
    use Ash.Resource, data_layer: :embedded

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :value, :binary
    end
  end

  defmodule Normal do
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
      attribute :value, :binary
      attribute :embedded, Embedded
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Normal
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "it handles normal resources" do
    normal =
      Normal
      |> new(%{value: @binary})
      |> Api.create!()

    assert normal.value == @binary
  end

  test "it handles embedded resources" do
    embedded =
      Normal
      |> new(%{embedded: %{value: @binary}})
      |> Api.create!()

    assert embedded.embedded.value == @binary
  end
end
