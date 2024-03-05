defimpl Reactor.Argument.Build, for: Ash.Reactor.Dsl.Tenant do
  @doc false
  @impl true
  def build(tenant),
    do:
      {:ok,
       [%Reactor.Argument{name: :tenant, source: tenant.source, transform: tenant.transform}]}
end
