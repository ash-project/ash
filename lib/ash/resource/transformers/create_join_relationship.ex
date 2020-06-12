defmodule Ash.Resource.Transformers.CreateJoinRelationship do
  @moduledoc """
  Creates an automatically named `has_many` relationship for each many_to_many.

  This will likely not be around for long, as our logic around many to many relationships
  will update soon.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  @extension Ash.Dsl

  # sobelow_skip ["DOS.StringToAtom"]
  def transform(_resource, dsl_state) do
    dsl_state
    |> Transformer.get_entities([:relationships], @extension)
    |> Enum.filter(&(&1.type == :many_to_many))
    |> Enum.reject(fn relationship ->
      has_many_name = to_string(relationship.name) <> "_join_assoc"

      dsl_state
      |> Transformer.get_entities([:relationships], @extension)
      |> Enum.find(&(to_string(&1.name) == has_many_name))
    end)
    |> Enum.reduce({:ok, dsl_state}, fn relationship, {:ok, dsl_state} ->
      has_many_name = String.to_atom(to_string(relationship.name) <> "_join_assoc")

      {:ok, relationship} =
        Transformer.build_entity(@extension, [:relationships], :has_many,
          name: has_many_name,
          destination: relationship.through,
          destination_field: relationship.source_field_on_join_table,
          source_field: relationship.source_field
        )

      {:ok, Transformer.add_entity(dsl_state, [:relationships], @extension, relationship)}
    end)
  end

  def before?(Ash.Resource.Transformers.SetRelationshipSource), do: true
  def before?(_), do: false
end
