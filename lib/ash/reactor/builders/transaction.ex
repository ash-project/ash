defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Transaction do
  @moduledoc false

  alias Reactor.{Builder, Dsl.Build}
  import Ash.Reactor.BuilderUtils
  import Reactor.Utils

  @doc false
  @impl true
  def build(transaction, reactor) do
    sub_reactor = Builder.new({Ash.Reactor.TransactionStep, transaction.name})

    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, sub_reactor} <- build_nested_steps(sub_reactor, transaction.steps),
         {:ok, sources} <- extract_inner_sources(sub_reactor),
         {:ok, arguments} <- build_transaction_arguments(sources),
         {:ok, sub_reactor} <- build_sub_reactor_inputs(sub_reactor, arguments),
         {:ok, sub_reactor} <- maybe_add_return(sub_reactor, transaction),
         {:ok, sub_reactor} <- Reactor.Planner.plan(sub_reactor) do
      arguments =
        transaction.wait_for
        |> Enum.concat(arguments)

      transaction_options =
        [
          sub_reactor: sub_reactor,
          resources: transaction.resources,
          timeout: transaction.timeout
        ]

      Builder.add_step(
        reactor,
        transaction.name,
        {Ash.Reactor.TransactionStep, transaction_options},
        arguments,
        async?: false,
        ref: :step_name,
        max_retries: 0
      )
    end
  end

  @doc false
  @impl true
  def verify(transaction, _dsl_state) do
    transaction.resources
    |> Enum.reject(&Ash.DataLayer.data_layer_can?(&1, :transact))
    |> case do
      [] ->
        :ok

      [resource] ->
        raise ArgumentError, "The `#{inspect(resource)}` resource does not support transactions."

      resources ->
        resources = Enum.map_join(resources, ", ", &"`#{inspect(&1)}`")

        raise ArgumentError, "The following resources do not support transactions: #{resources}."
    end
  end

  defp build_nested_steps(sub_reactor, steps),
    do: reduce_while_ok(steps, sub_reactor, &Build.build/2)

  # Iterates the nested steps and returns any argument sources which are not
  # transaction local.
  defp extract_inner_sources(sub_reactor) do
    inside_step_names =
      sub_reactor.steps
      |> Enum.map(& &1.name)
      |> MapSet.new()

    sources_to_raise =
      sub_reactor.steps
      |> Enum.flat_map(& &1.arguments)
      |> Enum.map(& &1.source)
      |> Enum.filter(fn
        source when is_struct(source, Reactor.Template.Input) ->
          true

        source when is_struct(source, Reactor.Template.Result) ->
          !MapSet.member?(inside_step_names, source.name)

        _ ->
          false
      end)
      |> Enum.map(fn
        source when is_map_key(source, :sub_path) -> %{source | sub_path: []}
        source -> source
      end)
      |> Enum.uniq()

    {:ok, sources_to_raise}
  end

  # sobelow_skip ["DOS.StringToAtom"]
  defp build_transaction_arguments(sources) do
    transaction_arguments =
      sources
      |> Enum.with_index()
      |> Enum.map(fn
        {source, _idx} when is_struct(source, Reactor.Template.Input) ->
          %Reactor.Argument{name: source.name, source: source}

        {source, idx} when is_struct(source, Reactor.Template.Result) ->
          %Reactor.Argument{name: String.to_atom("result_#{idx}"), source: source}
      end)

    {:ok, transaction_arguments}
  end

  defp build_sub_reactor_inputs(sub_reactor, arguments) do
    arguments
    |> reduce_while_ok(sub_reactor, fn
      argument, sub_reactor when is_struct(argument.source, Reactor.Template.Input) ->
        Builder.add_input(sub_reactor, argument.name)

      argument, sub_reactor when is_struct(argument.source, Reactor.Template.Result) ->
        with {:ok, sub_reactor} <- Builder.add_input(sub_reactor, argument.name) do
          Builder.add_step(
            sub_reactor,
            argument.source.name,
            {Reactor.Step.ReturnArgument, argument: argument.name},
            [argument],
            ref: :step_name,
            async?: false
          )
        end
    end)
  end

  defp maybe_add_return(sub_reactor, transaction) when is_nil(transaction.return) do
    last_step_name =
      transaction.steps
      |> Enum.map(& &1.name)
      |> List.last()

    Builder.return(sub_reactor, last_step_name)
  end

  defp maybe_add_return(sub_reactor, transaction),
    do: Builder.return(sub_reactor, transaction.return)
end
