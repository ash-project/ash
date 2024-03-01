defmodule Ash.Test.Resource.ValidateAcceptTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers
  alias Spark.Error.DslError

  test "Accepting an attribute that does not exist raises an error" do
    assert_raise DslError, ~r/\[:invalid\] are not attributes/, fn ->
      defposts do
        actions do
          create :example_action, accept: [:invalid]
        end
      end
    end
  end

  test "Accepting an attribute that is private raises an error" do
    assert_raise DslError, ~r/\[:secret\] are private attributes/, fn ->
      defposts do
        attributes do
          attribute :secret, :string
        end

        actions do
          create :example_action, accept: [:secret]
        end
      end
    end
  end

  test "Rejecting an attribute that does not exist raises an error" do
    assert_raise DslError, ~r/\[:invalid\] are not attributes/, fn ->
      defposts do
        actions do
          create :example_action, reject: [:invalid]
        end
      end
    end
  end

  test "Rejecting an attribute that is private raises an error" do
    assert_raise DslError, ~r/\[:secret\] are private attributes/, fn ->
      defposts do
        attributes do
          attribute :secret, :string
        end

        actions do
          create :example_action, reject: [:secret]
        end
      end
    end
  end

  test "Accepting and rejecting attributes that does not exist raises an error" do
    assert_raise DslError, ~r/\[:invalid, :invalid_2\] are not attributes/, fn ->
      defposts do
        actions do
          create :example_action, accept: [:invalid, :invalid_2], reject: [:invalid_reject]
        end
      end
    end
  end

  test "Validating all actions accept and reject attributes that does not exist raises an error" do
    assert_raise DslError, ~r/\[:update_invalid, :update_invalid_2\] are not attributes/, fn ->
      defposts do
        actions do
          create :example_action, accept: [:invalid, :invalid_2], reject: [:invalid_reject]
          update :update_example, accept: [:update_invalid, :update_invalid_2]
        end
      end
    end
  end
end
