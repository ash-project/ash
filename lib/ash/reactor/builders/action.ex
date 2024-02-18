defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Action do
  @moduledoc false

  alias Ash.Reactor.ActionStep
  alias Reactor.Builder
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(create, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, create) do
      arguments =
        arguments
        |> maybe_append(create.actor)
        |> maybe_append(create.tenant)
        |> Enum.concat(create.wait_for)

      action_options =
        create
        |> Map.take([:action, :api, :authorize?, :resource])
        |> Enum.to_list()

      step_options =
        create
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        create.name,
        {ActionStep, action_options},
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
