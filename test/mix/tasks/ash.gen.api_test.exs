defmodule Mix.Tasks.Ash.Gen.ApiTest do
  use ExUnit.Case, async: true

  alias Mix.Tasks.Ash.Gen.Api

  
  test "api without context" do
    assert Api.generate_file("api", false) == """
    defmodule Ash.Api do
      use Ash.Api

      resources do
        # add your resources here
        # resource Ash.ResourceName
      end
    end
    """
  end

  test "api with context" do
    assert Api.generate_file("context", true) == """
    defmodule Ash.Context.Context do
      use Ash.Api

      resources do
        # add your resources here
        # resource Ash.Context.ResourceName
      end
    end
    """
  end

end
