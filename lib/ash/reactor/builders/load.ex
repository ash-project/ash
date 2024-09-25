defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Load do
  @moduledoc false

  alias Ash.Reactor.LoadStep
  alias Reactor.{Argument, Builder}
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(load, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor) do
      arguments =
        [
          Argument.from_template(:records, load.records),
          Argument.from_template(:load, load.load, load.transform)
        ]
        |> maybe_append(load.actor)
        |> maybe_append(load.tenant)
        |> maybe_append(load.context)
        |> Enum.concat(load.wait_for)

      load_options =
        load
        |> Map.take([:authorize?, :domain, :lazy?, :reuse_values?, :strict?])
        |> Enum.reject(&is_nil(elem(&1, 1)))

      step_options =
        load
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        load.name,
        {LoadStep, load_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def verify(_load, _dsl_state), do: :ok
end
