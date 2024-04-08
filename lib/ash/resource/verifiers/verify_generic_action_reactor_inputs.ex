defmodule Ash.Resource.Verifiers.VerifyGenericActionReactorInputs do
  @moduledoc """
  Returns an error if a generic action calls a Reactor module without specifying
  an argument for all expected inputs.
  """
  use Spark.Dsl.Verifier

  def verify(dsl) do
    dsl
    |> Ash.Resource.Info.actions()
    |> Enum.filter(&(&1.type == :action))
    |> Enum.reduce_while(:ok, fn action, :ok ->
      case verify_action(action, dsl) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp verify_action(%{run: module} = action, dsl) when is_atom(module) do
    if is_reactor?(module) do
      verify_reactor_action(action, module, dsl)
    else
      :ok
    end
  end

  defp verify_action(%{run: {module, _}} = action, dsl) do
    if is_reactor?(module) do
      verify_reactor_action(action, module, dsl)
    else
      :ok
    end
  end

  defp verify_reactor_action(action, module, dsl) do
    reactor = module.reactor()

    required_inputs = MapSet.new(reactor.inputs)
    provided_arguments = MapSet.new(action.arguments, & &1.name)

    required_inputs
    |> MapSet.difference(provided_arguments)
    |> Enum.sort()
    |> case do
      [] ->
        :ok

      missing ->
        missing =
          missing
          |> Enum.map_join(", ", &"`#{inspect(&1)}`")

        {:error,
         Spark.Error.DslError.exception(
           module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
           path: [:actions, :action, action.name],
           message:
             "You need to provide arguments for all the Reactor's inputs.  Missing #{missing}"
         )}
    end
  end

  defp is_reactor?(module) when is_atom(module) do
    module.spark_is() == Reactor
  rescue
    UndefinedFunctionError -> false
  end
end
