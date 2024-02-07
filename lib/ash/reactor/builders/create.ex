defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Create do
  @moduledoc false

  alias Ash.Reactor.{CreateStep, Dsl.Create}
  alias Reactor.Builder
  alias Spark.Dsl
  import Ash.Reactor.BuilderUtils

  @spec build(Create.t(), Reactor.t()) :: {:ok, Reactor.t()} | {:error, any}
  def build(create, reactor) do
    with {:ok, reactor, arguments} <- build_input_arguments(reactor, create) do
      arguments =
        arguments
        |> maybe_append(create.actor)
        |> maybe_append(create.tenant)
        |> Enum.concat(create.wait_for)

      action_options =
        create
        |> Map.take([:action, :api, :authorize?, :resource, :upsert_identity, :upsert?])
        |> Enum.to_list()

      step_options =
        create
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        create.name,
        {CreateStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @spec transform(Create.t(), Dsl.t()) :: {:ok, Dsl.t()} | {:error, any}
  def transform(_create, dsl_state), do: {:ok, dsl_state}

  @spec verify(Create.t(), Dsl.t()) :: :ok | {:error, any}
  def verify(_create, _dsl_state), do: :ok
end
