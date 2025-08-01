defmodule Ash.Resource.Verifiers.VerifyFilterExpressions do
  @moduledoc """
  Raises an error if a filter expression references an undefined argument.
  """
  use Spark.Dsl.Verifier

  alias Spark.Error.DslError

  @impl true
  def verify(dsl) do
    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    dsl
    |> Ash.Resource.Info.actions()
    |> Enum.each(fn action ->
      verify_action_filter!(module, action)
    end)

    :ok
  end

  defp verify_action_filter!(module, %{filter: filter} = action) when not is_nil(filter) do
    argument_keys = action.arguments |> Enum.map(& &1.name) |> MapSet.new()

    Ash.Expr.walk_template(filter, fn
      {:_arg, field} = expr ->
        field = if is_binary(field), do: String.to_atom(field), else: field

        if MapSet.member?(argument_keys, field) do
          expr
        else
          raise DslError,
            module: module,
            message:
              "Filter expression references undefined argument `#{field}`. Available arguments: #{inspect(MapSet.to_list(argument_keys))}",
            path: [:actions, action.name, :filter]
        end

      other ->
        other
    end)
  end

  defp verify_action_filter!(_module, _action), do: :ok
end
