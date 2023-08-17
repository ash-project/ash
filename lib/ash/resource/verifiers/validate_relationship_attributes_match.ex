defmodule Ash.Resource.Verifiers.ValidateRelationshipAttributesMatch do
  @moduledoc """
  Shows a warning on potentially incompatible relationship attributes.
  """
  use Spark.Dsl.Verifier

  def verify(dsl) do
    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    dsl
    |> Ash.Resource.Info.relationships()
    |> Enum.reject(&(&1.type == :many_to_many))
    |> Enum.flat_map(fn
      %{
        source_attribute: source_attribute,
        destination_attribute: destination_attribute,
        destination: destination,
        validate_destination_attribute?: true
      } = rel ->
        case Code.ensure_compiled(destination) do
          {:module, destination} ->
            [{rel, source_attribute, destination_attribute, destination}]

          _ ->
            # older versions of ash/spark/elixir can get here, but not newer ones
            []
        end

      _ ->
        []
    end)
    |> Enum.flat_map(fn {rel, source, dest, resource} ->
      source = Ash.Resource.Info.attribute(dsl, source)
      dest = Ash.Resource.Info.attribute(resource, dest)

      if source && dest && !compatible_types?(source, dest) do
        [warning(rel, module, source, dest, resource)]
      else
        []
      end
    end)
    |> case do
      [] ->
        :ok

      warnings ->
        {:warn, warnings}
    end
  end

  defp warning(rel, module, source, dest, resource) do
    """
    Source attribute may not be compatible with destination attribute for #{inspect(module)}.#{rel.name} to #{inspect(resource)}

    `source_attribute` type: `#{inspect(source.type)}`
    `destination_attribute` type: `#{inspect(dest.type)}`

    Types are considered compatible if:

    1. They are exactly the same
    2. Their `storage_type/1` callback returns the same value
    3. The storage types are `:text` and `:string`
    4. The relationship has `validate_destination_attribute?` set to `false`.
    5. They are explicitly configured as compatible. To do so in this instance, add it to your config like so:


    config :ash, :compatible_foreign_key_types,
      [
        {#{inspect(source.type)}, #{inspect(dest.type)}}
      ]
    """
  end

  defp compatible_types?(%{type: source, constraints: source_constraints}, %{
         type: dest,
         constraints: dest_constraints
       }) do
    left_storage_type = Ash.Type.storage_type(source, source_constraints)
    right_storage_type = Ash.Type.storage_type(dest, dest_constraints)

    cond do
      source == dest ->
        true

      left_storage_type == right_storage_type ->
        true

      left_storage_type == :text and right_storage_type == :string ->
        true

      left_storage_type == :string and right_storage_type == :text ->
        true

      true ->
        configured_as_compatible?(source, dest)
    end
  end

  @compatible_foreign_key_types Application.compile_env(:ash, :compatible_foreign_key_types, [])

  defp configured_as_compatible?(source, dest) do
    Enum.find(@compatible_foreign_key_types, fn {left, right} ->
      (source == left and dest == right) or (source == right and dest == left)
    end)
  end
end
