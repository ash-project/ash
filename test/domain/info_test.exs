# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Domain.InfoTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Domain.Info
  alias Ash.Test.Flow.Domain

  describe "extensions/1" do
    test "returns extensions in use by the domain" do
      assert Ash.Domain.Dsl in Info.extensions(Domain)
      refute Ash.DataLayer.Mnesia in Info.extensions(Domain)
      assert Ash.DataLayer.Mnesia in Info.extensions(Domain, include_resource_extensions?: true)
    end
  end
end
