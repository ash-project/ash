defimpl Reactor.Argument.Build, for: Ash.Reactor.Dsl.Actor do
  def build(actor),
    do: {:ok, [%Reactor.Argument{name: :actor, source: actor.source, transform: actor.transform}]}
end
