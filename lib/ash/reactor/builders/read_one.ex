defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.ReadOne do
  alias Ash.Reactor.ReadOneStep
  alias Reactor.Builder
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(read_one, reactor) do
    with {:ok, reactor, arguments} <- build_input_arguments(reactor, read_one) do
      arguments =
        arguments
        |> maybe_append(read_one.actor)
        |> maybe_append(read_one.tenant)
        |> Enum.concat(read_one.wait_for)

      action_options =
        read_one
        |> Map.take([:action, :authorize?, :domain, :fail_on_not_found?, :resource])
        |> Enum.to_list()

      step_options =
        read_one
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        read_one.name,
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
