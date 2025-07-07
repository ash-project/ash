defmodule Ash.Resource.Verifiers.VerifySpecCompliance do
  @moduledoc """
  Verifies that a resource implements all required components from its declared specs.

  This verifier looks at the `specs` option on a resource and ensures that the resource
  implements all the attributes, relationships, etc. defined in those spec fragments.

  Specs are Spark DSL fragments that define interface contracts. Only fragments listed
  in the `specs` option are verified for compliance - regular fragments are ignored.
  """

  use Spark.Dsl.Verifier

  @impl true
  def verify(dsl_state) do
    resource = Spark.Dsl.Verifier.get_persisted(dsl_state, :module)
    specs = Spark.Dsl.Verifier.get_persisted(dsl_state, :specs, [])

    # Skip verification if no specs are declared
    if specs == [] do
      :ok
    else
      try do
        verify_spec_compliance(resource, dsl_state, specs)
      rescue
        error ->
          # If there's an error in verification, wrap it properly
          {:error,
           Spark.Error.DslError.exception(
             module: resource,
             message: "Error verifying spec compliance: #{inspect(error)}",
             path: [:specs]
           )}
      end
    end
  end

  defp verify_spec_compliance(resource, dsl_state, specs) do
    # Ensure specs is a list and iterate through each spec
    specs_list = if is_list(specs), do: specs, else: []

    Enum.reduce_while(specs_list, :ok, fn spec, :ok ->
      case verify_single_spec(resource, dsl_state, spec) do
        :ok ->
          {:cont, :ok}

        error ->
          {:halt, error}
      end
    end)
  end

  defp verify_single_spec(resource, dsl_state, spec) do
    try do
      # Get all DSL sections from the spec that contain entities
      spec_config = spec.spark_dsl_config()

      # Extract all sections that have entities (attributes, relationships, actions, etc.)
      spec_sections =
        spec_config
        |> Enum.filter(fn
          {[_section_name], %{entities: entities}} when is_list(entities) and entities != [] ->
            true

          _ ->
            false
        end)
        |> Enum.map(fn {[section_name], %{entities: entities}} ->
          {section_name, entities}
        end)

      # Verify each section
      Enum.reduce_while(spec_sections, :ok, fn {section_name, spec_entities}, :ok ->
        resource_entities = Spark.Dsl.Verifier.get_entities(dsl_state, [section_name])

        case verify_entities(resource, spec, section_name, spec_entities, resource_entities) do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end
      end)
    rescue
      error ->
        {:error,
         Spark.Error.DslError.exception(
           module: resource,
           message: "Error accessing spec #{inspect(spec)}: #{inspect(error)}",
           path: [:specs]
         )}
    end
  end

  defp verify_entities(resource, spec, section_name, spec_entities, resource_entities) do
    check_entities_individually(spec_entities, resource_entities, resource, spec, section_name)
  end

  defp check_entities_individually([], _resource_entities, _resource, _spec, _section), do: :ok

  defp check_entities_individually(
         [spec_entity | rest],
         resource_entities,
         resource,
         spec,
         section_name
       ) do
    case find_matching_entity(resource_entities, spec_entity) do
      nil ->
        {:error,
         Spark.Error.DslError.exception(
           module: resource,
           message: """
           Resource #{inspect(resource)} does not implement required #{String.trim_trailing("#{section_name}", "s")} #{inspect(spec_entity.name)} from spec #{inspect(spec)}.

           The spec requires:
             #{format_entity(spec_entity, section_name)}

           Add this #{String.trim_trailing("#{section_name}", "s")} to your resource to satisfy the spec.
           """,
           path: [section_name, spec_entity.name]
         )}

      matching_entity ->
        case verify_entity_compatibility(spec_entity, matching_entity, section_name) do
          :ok ->
            check_entities_individually(rest, resource_entities, resource, spec, section_name)

          {:error, reason} ->
            {:error,
             Spark.Error.DslError.exception(
               module: resource,
               message: """
               Resource #{inspect(resource)} #{section_name} #{inspect(spec_entity.name)} does not match spec #{inspect(spec)} requirements.

               Spec requires: #{format_entity(spec_entity, section_name)}
               Resource has: #{format_entity(matching_entity, section_name)}

               Error: #{reason}
               """,
               path: [section_name, spec_entity.name]
             )}
        end
    end
  end

  defp find_matching_entity(resource_entities, spec_entity) do
    Enum.find(resource_entities, fn entity ->
      entity.name == spec_entity.name
    end)
  end

  defp verify_entity_compatibility(spec_entity, resource_entity, section_name) do
    case section_name do
      :attributes ->
        verify_attribute_compatibility(spec_entity, resource_entity)

      # For other entity types, we can add specific validation later
      # For now, just check that the entity exists (which we already did)
      _ ->
        :ok
    end
  end

  defp verify_attribute_compatibility(spec_attribute, resource_attribute) do
    cond do
      spec_attribute.type != resource_attribute.type ->
        {:error,
         "type mismatch: expected #{inspect(spec_attribute.type)}, got #{inspect(resource_attribute.type)}"}

      spec_attribute.allow_nil? == false and resource_attribute.allow_nil? == true ->
        {:error,
         "allow_nil? mismatch: spec requires allow_nil? false, but resource has allow_nil? true"}

      true ->
        :ok
    end
  end

  defp format_entity(entity, section_name) do
    case section_name do
      :attributes ->
        "attribute #{inspect(entity.name)}, #{inspect(entity.type)}"

      :relationships ->
        "#{entity.type} #{inspect(entity.name)}"

      :actions ->
        "#{entity.type} #{inspect(entity.name)}"

      _ ->
        "#{section_name} #{inspect(entity.name)}"
    end
  end
end
