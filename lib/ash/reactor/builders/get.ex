defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Get do
  alias Ash.Reactor.GetStep
  alias Reactor.Builder
  import Ash.Reactor.BuilderUtils

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
        {GetStep, action_options},
        arguments,
        step_options
      )
    end
  end

  def transform(_create, dsl_state), do: {:ok, dsl_state}

  def verify(_create, _dsl_state), do: :ok
end
