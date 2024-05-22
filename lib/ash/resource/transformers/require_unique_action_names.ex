defmodule Ash.Resource.Transformers.RequireUniqueActionNames do
  @moduledoc """
  Ensures that all actions have unique names.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.group_by(& &1.name)
    |> Enum.each(fn {name, actions} ->
      unless Enum.count(actions) == 1 do
        raise DslError.exception(
                message: """
                Multiple actions (#{Enum.count(actions)}) with the name `#{name}` defined.

                In the past, the pattern of having multiple actions called `:default`
                was promoted in the documentation, but that is no longer valid. All
                actions must have unique names.
                """,
                path: [:actions, name]
              )
      end
    end)

    {:ok, dsl_state}
  end

  def after?(Ash.Resource.Transformers.SetPrimaryActions), do: true
  def after?(_), do: false
end
