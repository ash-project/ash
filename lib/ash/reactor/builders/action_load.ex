# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defimpl Reactor.Argument.Build, for: Ash.Reactor.Dsl.ActionLoad do
  @doc false
  @impl true
  def build(load),
    do: {:ok, [%Reactor.Argument{name: :load, source: load.source, transform: load.transform}]}
end
