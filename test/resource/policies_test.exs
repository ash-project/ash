# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.PoliciesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "records can belong to other resources" do
    assert_raise Spark.Error.DslError,
                 ~r/Bypass policies that can only ever forbid have no effect/,
                 fn ->
                   defmodule HasBadBypassPolicy do
                     use Ash.Resource,
                       domain: Ash.Test.Domain,
                       authorizers: [Ash.Policy.Authorizer]

                     attributes do
                       uuid_primary_key :id
                     end

                     policies do
                       bypass always() do
                         forbid_if always()
                         forbid_unless always()
                       end
                     end
                   end
                 end
  end
end
