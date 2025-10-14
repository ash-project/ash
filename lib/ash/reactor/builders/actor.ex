# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defimpl Reactor.Argument.Build, for: Ash.Reactor.Dsl.Actor do
  @doc false
  @impl true
  def build(actor),
    do: {:ok, [%Reactor.Argument{name: :actor, source: actor.source, transform: actor.transform}]}
end
