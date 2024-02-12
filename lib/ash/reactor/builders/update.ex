defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Update do
  @moduledoc false

  alias Ash.Reactor.UpdateStep
  alias Reactor.{Argument, Builder}
  import Ash.Reactor.BuilderUtils

  def build(update, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, update) do
      arguments =
        arguments
        |> maybe_append(update.actor)
        |> maybe_append(update.tenant)
        |> Enum.concat(update.wait_for)
        |> Enum.concat([%Argument{name: :initial, source: update.initial}])

      action_options =
        update
        |> Map.take([:action, :api, :authorize?, :resource])
        |> Enum.to_list()

      step_options =
        update
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        update.name,
        {UpdateStep, action_options},
        arguments,
        step_options
      )
    end
  end

  def transform(_update, dsl_state), do: {:ok, dsl_state}

  def verify(_update, _dsl_state), do: :ok
end
