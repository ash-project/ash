# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Input do
  @moduledoc """
  Reactor's internal representation for inputs.
  """
  defstruct [:name, :description]

  @type t :: %__MODULE__{
          name: atom,
          description: nil | String.t()
        }
end
