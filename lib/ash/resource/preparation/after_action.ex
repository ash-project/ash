# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Preparation.AfterAction do
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
    Ash.Subject.after_action(subject, fn subject, result ->
      opts[:callback].(subject, result, context)
    end)
  end
end
