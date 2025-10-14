# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.RequireUniqueActionNames do
  @moduledoc """
  Ensures that all actions have unique names.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Entity
  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    dsl_state
    |> Ash.Resource.Info.actions()
    |> Enum.group_by(& &1.name)
    |> Enum.each(fn {name, actions} ->
      if Enum.count(actions) != 1 do
        # Find the second action with this name and get location of its name property
        second_action = Enum.at(actions, 1)

        location =
          case Entity.property_anno(second_action, :name) do
            nil -> Entity.anno(second_action)
            other -> other
          end

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
