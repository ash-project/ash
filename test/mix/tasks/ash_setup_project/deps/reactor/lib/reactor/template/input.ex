# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Template.Input do
  @moduledoc """
  The `input` template.
  """

  defstruct name: nil, sub_path: []

  @type t :: %__MODULE__{name: atom, sub_path: [atom]}
end
