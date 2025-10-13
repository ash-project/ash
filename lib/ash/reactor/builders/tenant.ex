# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defimpl Reactor.Argument.Build, for: Ash.Reactor.Dsl.Tenant do
  @doc false
  @impl true
  def build(tenant),
    do:
      {:ok,
       [%Reactor.Argument{name: :tenant, source: tenant.source, transform: tenant.transform}]}
end
