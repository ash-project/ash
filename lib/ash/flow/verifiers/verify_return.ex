defmodule Ash.Flow.Verifiers.VerifyReturn do
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier

  def verify(dsl_state) do
    steps = Ash.Flow.Info.steps(dsl_state)

    return_step_names =
      case Ash.Flow.Info.returns(dsl_state) do
        value when is_list(value) ->
          if Keyword.keyword?(value) do
            Keyword.keys(value)
          else
            value
          end

        value when is_map(value) ->
          Map.keys(value)

        value ->
          value
      end
      |> List.wrap()

    if return_step_names do
      case Enum.find(return_step_names, fn step_name ->
             is_nil(returnable_step(steps, step_name))
           end) do
        nil ->
          :ok

        invalid_step_name ->
          module = Verifier.get_persisted(dsl_state, :module)

          {:error,
           Spark.Error.DslError.exception(
             module: module,
             path: [:flow, :returns, invalid_step_name],
             message: """
             #{inspect(invalid_step_name)} is not a returnable step in #{inspect(module)}.
             """
           )}
      end
    else
      :ok
    end
  end

  defp returnable_step(steps, name) do
    Enum.find(steps, fn
      %{name: ^name} = step ->
        step

      %Ash.Flow.Step.Map{} ->
        nil

      %{steps: steps} ->
        returnable_step(steps, name)

      %{name: ^name} = step ->
        step

      _ ->
        nil
    end)
  end
end
