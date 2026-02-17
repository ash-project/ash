# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Template.Value do
  @moduledoc """
  A statically `value` template.
  """

  defstruct value: nil, sub_path: []

  @type t :: %__MODULE__{value: any, sub_path: [any]}
end
