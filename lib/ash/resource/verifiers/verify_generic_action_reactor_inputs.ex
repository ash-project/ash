# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.VerifyGenericActionReactorInputs do
  @moduledoc """
  Returns an error if a generic action calls a Reactor module without specifying
  an argument for all expected inputs. Does nothing when `infer_generic_action_reactors?` is disabled.
  """
  use Spark.Dsl.Verifier

  @infer_generic_action_reactors? Application.compile_env(
                                    :ash,
                                    :infer_generic_action_reactors?,
                                    true
                                  )

  if @infer_generic_action_reactors? do
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
  else
    # Resources don't depend at compile time on their `run` modules in this mode,
    # so anything checked here could go stale.
    def verify(_dsl), do: :ok
  end

  defp verify_action(%{run: module} = action, dsl) when is_atom(module) do
    verify_action(%{action | run: {module, []}}, dsl)
  end

  defp verify_action(%{run: {Ash.Resource.Actions.RunReactor, opts}} = action, dsl) do
    reactor = opts[:reactor]

    if is_atom(reactor) and Ash.Resource.Actions.RunReactor.reactor?(reactor) do
      verify_reactor_action(action, reactor, dsl)
    else
      {:error,
       Spark.Error.DslError.exception(
         module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
         path: [:actions, :action, action.name, :run],
         message: "`reactor/2` expects a Reactor module, got: #{inspect(reactor)}"
       )}
    end
  end

  defp verify_action(%{run: {module, _}} = action, dsl) do
    if Ash.Resource.Actions.RunReactor.reactor?(module) do
      verify_reactor_action(action, module, dsl)
    else
      :ok
    end
  end

  defp verify_reactor_action(action, module, dsl) do
    reactor = module.reactor()

    required_inputs = MapSet.new(reactor.inputs, & &1.name)
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
end
