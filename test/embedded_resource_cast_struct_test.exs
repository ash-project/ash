defmodule Ash.Test.EmbeddedResourceCastStructTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Vector3 do
    @moduledoc false
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :x, :decimal
      attribute :y, :decimal
      attribute :z, :decimal
    end
  end

  defmodule Transform do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id

      attribute :position, Vector3
      attribute :rotation, Vector3
      attribute :scale, Vector3
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Transform
    end
  end

  import Ash.Changeset

  test "embedded resource is casted if passed as a struct" do
    assert transform =
             %Transform{position: %Vector3{x: %Decimal{}, y: %Decimal{}, z: %Decimal{}}} =
             Transform
             |> new(%{position: %Vector3{x: 0, y: 0, z: 0}})
             |> Api.create!()

    assert [_] = Api.read!(Transform)

    assert transform == Api.get!(Transform, transform.id)

    assert transform_updated =
             %Transform{position: %Vector3{x: %Decimal{}, y: %Decimal{}, z: %Decimal{}}} =
             transform
             |> new(position: %Vector3{x: 1.1, y: 1.1, z: 1.1})
             |> Api.update!()

    assert Decimal.new("1.1") == transform_updated.position.x
    assert Decimal.new("1.1") == transform_updated.position.y
    assert Decimal.new("1.1") == transform_updated.position.z

    assert :ok = Api.destroy!(transform)
  end
end
