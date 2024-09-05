defimpl Reactor.Argument.Build, for: Ash.Reactor.Dsl.ActionLoad do
  @doc false
  @impl true
  def build(load),
    do: {:ok, [%Reactor.Argument{name: :load, source: load.source, transform: load.transform}]}
end
