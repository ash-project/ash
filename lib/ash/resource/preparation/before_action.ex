# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Preparation.BeforeAction do
  @moduledoc false

  use Ash.Resource.Preparation

  def supports(_opts), do: [Ash.Query, Ash.ActionInput]

  @doc false
  @spec prepare(Ash.Query.t() | Ash.ActionInput.t(), keyword, map) ::
          Ash.Query.t() | Ash.ActionInput.t()
  def prepare(subject, opts, context) do
    Ash.Subject.before_action(subject, fn subject -> opts[:callback].(subject, context) end)
  end
end
