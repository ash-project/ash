# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.InfoTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Info

  describe "extensions_in_use/1" do
    test "returns extensions in use by all the domains and resources" do
      assert Ash.Domain.Dsl in Info.extensions_in_use(:ash)
      assert Ash.Resource.Dsl in Info.extensions_in_use(:ash)
    end
  end

  describe "defined_extensions/1" do
    test "returns all defined extensions in the application" do
      assert Ash.Domain.Dsl in Info.defined_extensions(:ash)
      assert Ash.Resource.Dsl in Info.defined_extensions(:ash)
    end
  end
end
