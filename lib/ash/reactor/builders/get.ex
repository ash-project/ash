defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Get do
  alias Ash.Reactor.GetStep
  alias Reactor.Builder
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(get, reactor) do
    with {:ok, reactor, arguments} <- build_input_arguments(reactor, get) do
      arguments =
        arguments
        |> maybe_append(get.actor)
        |> maybe_append(get.tenant)
        |> maybe_append(get.load)
        |> maybe_append(get.context)
        |> Enum.concat(get.wait_for)

      action_options =
        get
        |> Map.take([:action, :authorize?, :by, :domain, :fail_on_not_found?, :resource])
        |> Enum.to_list()

      step_options =
        get
        |> Map.take([:async?, :guards])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        get.name,
        {GetStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def verify(_create, _dsl_state), do: :ok
end
