# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.PrivateAction do
  @moduledoc """
  This check is true when the current action is private (`public?: false`).

  Use it to bypass authorization for internal-only actions that must never be
  exposed by API extensions (e.g. AshGraphql, AshJsonApi):

  ```elixir
  policy bypass private_action?() do
    authorize_if always()
  end
  ```
  """
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(_) do
    "action is private (public?: false)"
  end

  @impl true
  def match?(_actor, %{action: action}, _opts) do
    # Default to true (public) when key is missing for backwards compatibility
    !Map.get(action, :public?, true)
  end
end
