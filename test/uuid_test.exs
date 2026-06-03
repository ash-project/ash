# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.UUIDTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "generate/0 returns a version 4 UUID (backwards compatible)" do
    uuid = Ash.UUID.generate()

    assert <<_::64, ?-, _::32, ?-, "4", _::24, ?-, _::32, ?-, _::96>> = uuid
  end

  test "generate/1 accepts opts and can produce a monotonic version 7 UUID" do
    uuid = Ash.UUID.generate(version: 7, precision: :monotonic)

    assert <<_::64, ?-, _::32, ?-, "7", _::24, ?-, _::32, ?-, _::96>> = uuid
  end

  test "&Ash.UUID.generate/0 still resolves as a 0-arity capture" do
    # AshPostgres' migration generator matches UUID defaults by capturing
    # `&Ash.UUID.generate/0`; the `\\ []` default must keep that capture valid.
    fun = &Ash.UUID.generate/0

    assert is_function(fun, 0)
    assert <<_::64, ?-, _::32, ?-, "4", _::24, ?-, _::32, ?-, _::96>> = fun.()
  end
end
