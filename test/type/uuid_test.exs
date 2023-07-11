defmodule Ash.Test.Type.UUIDTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "it casts binary UUIDs to string" do
    uuid = "a7cec9ba-15de-4c56-99e4-c2abc91a2209"

    assert {:ok, binary_uuid} = Ash.Type.dump_to_native(Ash.Type.UUID, uuid)
    assert {:ok, ^uuid} = Ash.Type.cast_input(Ash.Type.UUID, binary_uuid)
  end
end
