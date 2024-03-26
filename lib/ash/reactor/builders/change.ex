defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Change do
  @moduledoc false

  alias Ash.Reactor.ChangeStep
  alias Ash.Resource.Info
  alias Reactor.{Argument, Builder, Step.ReturnAllArguments}
  alias Spark.{Dsl.Transformer, Error.DslError}
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(change, reactor) do
    argument_step_name = {:__arguments__, change.name, Enum.map(change.arguments, & &1.name)}

    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor} <-
           Builder.add_step(reactor, argument_step_name, ReturnAllArguments, change.arguments,
             async?: change.async?,
             ref: :step_name
           ) do
      initial =
        case change.initial do
          module when is_atom(module) and not is_nil(module) ->
            Argument.from_value(:initial, module)

          template
          when is_struct(template, Reactor.Template.Input) or
                 is_struct(template, Reactor.Template.Result) or
                 is_struct(template, Reactor.Template.Value) ->
            %Argument{name: :initial, source: template}
        end

      arguments = Argument.from_result(:arguments, argument_step_name)

      step_options =
        change
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      change_options =
        change
        |> Map.take([:always_atomic?, :change, :only_when_valid?, :where])
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        change.name,
        {ChangeStep, change_options},
        [arguments, initial],
        step_options
      )
    end
  end

  @doc false
  @impl true
  def transform(_change, dsl_state), do: {:ok, dsl_state}

  @doc false
  @impl true
  def verify(_change, _dsl_state), do: :ok
end
