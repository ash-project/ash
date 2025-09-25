defmodule Ash.Resource.Transformers.RequireUniqueActionNames do
  @moduledoc """
  Ensures that all actions have unique names.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Dsl.Extension
  alias Spark.Dsl.Entity
  alias Spark.Error.DslError

  def transform(dsl_state) do
    dsl_state
    |> Ash.Resource.Info.actions()
    |> Enum.group_by(& &1.name)
    |> Enum.each(fn {name, actions} ->
      if Enum.count(actions) != 1 do
        # Find the first action with this name to get location info
        second_action = Enum.at(actions, 1)
        location = Entity.anno(second_action)

        raise DslError.exception(
                module: Transformer.get_persisted(dsl_state, :module),
                message: """
                Multiple actions (#{Enum.count(actions)}) with the name `#{name}` defined.

                In the past, the pattern of having multiple actions called `:default`
                was promoted in the documentation, but that is no longer valid. All
                actions must have unique names.
                """,
                path: [:actions, name],
                location: location
              )
      end
    end)

    {:ok, dsl_state}
  end

  def after?(Ash.Resource.Transformers.SetPrimaryActions), do: true
  def after?(_), do: false
end
