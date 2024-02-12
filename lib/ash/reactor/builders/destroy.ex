defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Destroy do
  @moduledoc false

  alias Ash.Reactor.DestroyStep
  alias Reactor.{Argument, Builder}
  import Ash.Reactor.BuilderUtils

  def build(destroy, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, destroy) do
      arguments =
        arguments
        |> maybe_append(destroy.actor)
        |> maybe_append(destroy.tenant)
        |> Enum.concat(destroy.wait_for)
        |> Enum.concat([%Argument{name: :initial, source: destroy.initial}])

      action_options =
        destroy
        |> Map.take([:action, :api, :authorize?, :resource, :return_destroyed?])
        |> Enum.to_list()

      step_options =
        destroy
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        destroy.name,
        {DestroyStep, action_options},
        arguments,
        step_options
      )
    end
  end

  def transform(_destroy, dsl_state), do: {:ok, dsl_state}

  def verify(_destroy, _dsl_state), do: :ok
end
