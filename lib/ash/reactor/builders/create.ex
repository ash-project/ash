defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Create do
  @moduledoc false

  alias Ash.Reactor.{CreateStep, Dsl.Create, MergeInputsStep}
  alias Reactor.{Argument, Builder, Step.ReturnAllArguments}
  alias Spark.Dsl

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

  defp build_input_arguments(reactor, create) when create.inputs == [],
    do: {:ok, reactor, [input: {:value, %{}}]}

  defp build_input_arguments(reactor, create) do
    Enum.reduce_while(create.inputs, {:ok, reactor, []}, fn input, {:ok, reactor, result_names} ->
      arguments = Enum.map(input.template, &%Argument{name: elem(&1, 0), source: elem(&1, 1)})
      name = {:__input__, create.name, Map.keys(input.template)}

      case Builder.add_step(reactor, name, ReturnAllArguments, arguments,
             transform: input.transform,
             ref: :step_name
           ) do
        {:ok, reactor} -> {:cont, {:ok, reactor, [name | result_names]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:error, reason} ->
        {:error, reason}

      {:ok, reactor, []} ->
        {:ok, reactor, []}

      {:ok, reactor, [result_name]} ->
        {:ok, reactor, [input: {:result, result_name}]}

      {:ok, reactor, result_names} ->
        arguments =
          result_names
          |> Enum.with_index()
          |> Enum.map(&{:"input_#{elem(&1, 1)}", {:result, elem(&1, 0)}})

        name = {:__input__, create.name, :__merge__, result_names}

        case Builder.add_step(reactor, name, MergeInputsStep, arguments, ref: :step_name) do
          {:ok, reactor} -> {:ok, reactor, [input: {:result, name}]}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp maybe_append(lhs, nil), do: lhs
  defp maybe_append(lhs, rhs), do: Enum.concat(lhs, [rhs])
end
