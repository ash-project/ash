# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions.Implementation.Builtins do
  @moduledoc "Builtin generic action implementations"

  @doc """
  Runs the given `Reactor` as the generic action.

  The action's arguments are passed as the Reactor's inputs, and the action's context
  (actor, tenant, etc.) as its context. Every Reactor input needs a corresponding action
  argument.

  Any options are passed to `Reactor.run/4`. If the action sets `transaction? true`, the
  Reactor is run synchronously regardless of the `async?` option.

  ## Examples

      run reactor(MyApp.Reactors.CreatePost)
      run reactor(MyApp.Reactors.CreatePost, max_concurrency: 4)
  """
  @spec reactor(reactor :: module, opts :: Keyword.t()) ::
          {Ash.Resource.Actions.RunReactor, Keyword.t()}
  def reactor(reactor, opts \\ []) do
    {Ash.Resource.Actions.RunReactor, Keyword.put(opts, :reactor, reactor)}
  end
end
