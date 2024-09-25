defimpl Reactor.Argument.Build, for: Ash.Reactor.Dsl.Context do
  import Reactor.Template, only: [is_template: 1]

  @doc false
  @impl true
  def build(context) when is_template(context.context),
    do: {:ok, [Reactor.Argument.from_template(:context, context.context, context.transform)]}

  def build(context) when is_map(context.context),
    do: {:ok, [Reactor.Argument.from_value(:context, context.context, context.transform)]}

  def build(context) when is_nil(context.context),
    do: {:ok, []}
end
