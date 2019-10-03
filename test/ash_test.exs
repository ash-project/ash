defmodule AshTest do
  use ExUnit.Case
  doctest Ash

  test "greets the world" do
    assert Ash.hello() == :world
  end
end
