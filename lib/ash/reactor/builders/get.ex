defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.ReadOne do
  alias Ash.Reactor.ReadOneStep
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
        |> Enum.concat(get.wait_for)

      action_options =
        get
        |> Map.take([:action, :api, :authorize?, :resource, :fail_on_not_found?])
        |> Enum.to_list()

      step_options =
        get
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        get.name,
        {ReadOneStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def transform(_create, dsl_state), do: {:ok, dsl_state}

  @doc false
  @impl true
  def verify(_create, _dsl_state), do: :ok
end
