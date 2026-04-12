# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateThroughRelationships do
  @moduledoc """
  Validates that all `through` relationship paths reference existing relationships
  and resolve to the declared destination.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Entity
  alias Spark.Dsl.Verifier

  @impl true
  def verify(dsl) do
    module = Verifier.get_persisted(dsl, :module)

    through_relationships =
      dsl
      |> Ash.Resource.Info.relationships()
      |> Enum.filter(&is_non_empty_list(&1))

    if through_relationships != [] &&
         not Ash.DataLayer.data_layer_can?(dsl, :through_relationship) do
      raise Spark.Error.DslError,
        module: module,
        path: [:relationships],
        message: """
        The data layer for `#{inspect(module)}` does not support `through` relationships.

        Through relationships require data layer support. If you are using `ash_postgres`,
        make sure you have a version that includes through relationship support.
        """
    end

    Enum.each(through_relationships, &validate_through_path(&1.source, &1.through, &1, module))
  end

  defp validate_through_path(resource, [step | rest], relationship, module) do
    step_relationship = Ash.Resource.Info.relationship(resource, step)

    if is_nil(step_relationship) do
      raise Spark.Error.DslError,
        module: module,
        location: Entity.anno(relationship),
        path: [:relationships, relationship.name],
        message:
          "Relationship `#{relationship.name}` has through path #{inspect(relationship.through)}, but relationship `#{step}` does not exist on #{inspect(resource)}"
    end

    if rest == [] do
      if step_relationship.destination != relationship.destination do
        raise Spark.Error.DslError,
          module: module,
          location: Entity.anno(relationship),
          path: [:relationships, relationship.name],
          message:
            "Relationship `#{relationship.name}` has through path #{inspect(relationship.through)} which resolves to #{inspect(step_relationship.destination)}, but the declared destination is #{inspect(relationship.destination)}"
      end
    else
      validate_through_path(step_relationship.destination, rest, relationship, module)
    end
  end

  defp is_non_empty_list(%{through: through}) when is_list(through), do: through != []
  defp is_non_empty_list(_relationship), do: false
end
