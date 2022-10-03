defmodule Ash.Test.Resource.ValidateAcceptTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers
  alias Spark.Error.DslError

  test "Accepting an attribute that does not exist raises an error" do
    assert_raise DslError, ~r/invalid is not an attribute/, fn ->
      defposts do
        actions do
          create :example_action, accept: [:invalid]
        end
      end
    end
  end

  test "Accepting an attribute that is private raises an error" do
    assert_raise DslError, ~r/secret is a private attribute/, fn ->
      defposts do
        attributes do
          attribute :secret, :string, private?: true
        end

        actions do
          create :example_action, accept: [:secret]
        end
      end
    end
  end

  test "Rejecting an attribute that does not exist raises an error" do
    assert_raise DslError, ~r/invalid is not an attribute/, fn ->
      defposts do
        actions do
          create :example_action, reject: [:invalid]
        end
      end
    end
  end

  test "Rejecting an attribute that is private raises an error" do
    assert_raise DslError, ~r/secret is a private attribute/, fn ->
      defposts do
        attributes do
          attribute :secret, :string, private?: true
        end

        actions do
          create :example_action, reject: [:secret]
        end
      end
    end
  end
end
