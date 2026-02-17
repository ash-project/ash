# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid do
  @moduledoc """
  The [Splode error class](e:splode:get-started-with-splode.html#error-classes)
  for user-caused errors.
  """

  use Reactor.Error, fields: [:errors], class: :invalid

  @doc false
  @impl true
  def message(%{errors: errors}) do
    Splode.ErrorClass.error_messages(errors)
  end
end
