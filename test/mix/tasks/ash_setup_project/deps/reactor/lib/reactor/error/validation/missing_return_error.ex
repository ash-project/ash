# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Validation.MissingReturnError do
  @moduledoc """
  An error returned when a Reactor cannot be validated because of a missing
  return value.
  """

  use Reactor.Error,
    fields: [:reactor],
    class: :validation

  @doc false
  @impl true
  def message(_error) do
    """
    # Missing Return Error

    The Reactor does not have a named return value.

    You can set one using `Reactor.Builder.return/2` or by setting the `d:Reactor.Dsl.reactor.return` DSL option.
    """
  end
end
