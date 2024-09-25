defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Read do
  alias Ash.Reactor.ReadStep
  alias Reactor.Builder
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(read, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, read) do
      arguments =
        arguments
        |> maybe_append(read.actor)
        |> maybe_append(read.tenant)
        |> maybe_append(read.load)
        |> maybe_append(read.context)
        |> Enum.concat(read.wait_for)

      action_options =
        read
        |> Map.take([:action, :authorize?, :domain, :resource])
        |> Enum.to_list()

      step_options =
        read
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        read.name,
        {ReadStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def verify(_create, _dsl_state), do: :ok
end
