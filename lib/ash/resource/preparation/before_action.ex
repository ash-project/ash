# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Preparation.BeforeAction do
  @moduledoc false

  use Ash.Resource.Preparation

  # Wraps an arbitrary function, whose temporal safety cannot be known.
  @impl true
  def temporal_safe?(_opts), do: false

  @impl true
  def supports(_opts), do: [Ash.Query, Ash.ActionInput]

  @doc false
  @impl true
  @spec prepare(Ash.Query.t() | Ash.ActionInput.t(), keyword, map) ::
          Ash.Query.t() | Ash.ActionInput.t()
  def prepare(subject, opts, context) do
    Ash.Subject.before_action(subject, fn subject -> opts[:callback].(subject, context) end)
  end
end
