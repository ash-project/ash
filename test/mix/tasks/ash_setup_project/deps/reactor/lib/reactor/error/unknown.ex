# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Unknown do
  @moduledoc """
  The [Splode error class](e:splode:get-started-with-splode.html#error-classes)
  for unknown errors.
  """

  use Reactor.Error, fields: [:errors], class: :unknown

  @doc false
  @impl true
  def message(%{errors: errors}) do
    Splode.ErrorClass.error_messages(errors)
  end
end
