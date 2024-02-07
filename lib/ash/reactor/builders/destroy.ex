defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Destroy do
  @moduledoc false

  alias Ash.Reactor.{DestroyStep, Dsl.Destroy}
  alias Reactor.{Argument, Builder}
  alias Spark.Dsl
  import Ash.Reactor.BuilderUtils

  @spec build(Destroy.t(), Reactor.t()) :: {:ok, Reactor.t()} | {:error, any}
  def build(destroy, reactor) do
    with {:ok, reactor, arguments} <- build_input_arguments(reactor, destroy) do
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

  @spec transform(Destroy.t(), Dsl.t()) :: {:ok, Dsl.t()} | {:error, any}
  def transform(_destroy, dsl_state), do: {:ok, dsl_state}

  @spec verify(Destroy.t(), Dsl.t()) :: :ok | {:error, any}
  def verify(_destroy, _dsl_state), do: :ok
end
