defmodule Ash.Test.ReactorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "it can be used directly" do
    defmodule DirectReactor do
      @moduledoc false
      use Ash.Reactor

      input :whom

      step :greet do
        argument :whom, input(:whom)
        run fn %{whom: whom} -> {:ok, "Hello, #{whom}!"} end
      end
    end

    assert {:ok, "Hello, Marty!"} = Reactor.run(DirectReactor, %{whom: "Marty"})
  end
end
