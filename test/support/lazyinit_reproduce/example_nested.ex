# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Type.LazyInitTest.ExampleNested do
  @moduledoc false
  use Ash.Type.NewType, subtype_of: Type.LazyInitTest.Example, lazy_init?: true
end
