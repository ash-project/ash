defmodule Ash.Test.Type.DecimalTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Vector2 do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id

      attribute :x, :decimal
      attribute :y, :decimal
    end
  end

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
      resource Vector2
    end
  end

  import Ash.Changeset

  test "decimal type works on regular resource" do
    assert vector2 = %Vector2{x: %Decimal{}, y: %Decimal{}} =
      Vector2
      |> new(%{x: 0, y: 0})
      |> Api.create!()

    assert vector2 == Api.get!(Vector2, vector2.id)

    assert vector2_updated = %Vector2{x: %Decimal{}, y: %Decimal{}} = %Vector2{x: %Decimal{}, y: %Decimal{}} =
      vector2
      |> new(%{x: 1.1, y: 1.1})
      |> Api.update!()

    assert Decimal.new("1.1") == vector2_updated.x
    assert Decimal.new("1.1") == vector2_updated.y

    assert [_] = Api.read!(Vector2)
    assert :ok = Api.destroy!(vector2_updated)
  end

  test "decimal type works on embedded resources" do
    assert transform = %Transform{position: %Vector3{x: %Decimal{}, y: %Decimal{}, z: %Decimal{}}} =
      Transform
      |> new(%{position: %Vector3{x: 0, y: 0, z: 0}})
      |> IO.inspect()
      |> Api.create!()

    assert [_] = Api.read!(Transform)

    assert transform == Api.get!(Transform, transform.id)

    assert transform_updated = %Transform{position: %Vector3{x: %Decimal{}, y: %Decimal{}, z: %Decimal{}}} =
      transform
      |> new(position: %Vector3{x: 1.1, y: 1.1, z: 1.1})
      |> Api.update!()

    assert Decimal.new("1.1") == transform_updated.position.x
    assert Decimal.new("1.1") == transform_updated.position.y
    assert Decimal.new("1.1") == transform_updated.position.z

    assert :ok = Api.destroy!(transform)
  end
end
