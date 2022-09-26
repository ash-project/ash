defmodule Ash.Resource.Transformers.BelongsToAttribute do
  @moduledoc """
  Creates the attribute for belongs_to relationships that have `define_attribute?: true`
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  @extension Ash.Resource.Dsl

  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:relationships])
    |> Enum.filter(&(&1.type == :belongs_to && &1.define_attribute?))
    |> Enum.reject(fn relationship ->
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.find(&(Map.get(&1, :name) == relationship.source_attribute))
    end)
    |> Enum.reduce_while({:ok, dsl_state}, fn relationship, {:ok, dsl_state} ->
      entity =
        Transformer.build_entity(@extension, [:attributes], :attribute,
          name: relationship.source_attribute,
          type: relationship.attribute_type || :uuid,
          allow_nil?:
            if relationship.primary_key? do
              false
            else
              relationship.allow_nil?
            end,
          writable?: relationship.attribute_writable?,
          private?: !relationship.attribute_writable?,
          primary_key?: relationship.primary_key?
        )

      valid_opts? = not (relationship.primary_key? && relationship.allow_nil?)

      entity_or_error =
        if valid_opts? do
          entity
        else
          {:error,
           "Relationship cannot be a primary key unless it is also marked as `allow_nil? false`"}
        end

      add_entity(entity_or_error, dsl_state, relationship)
    end)
  end

  defp add_entity({:ok, attribute}, dsl_state, _relationship),
    do: {:cont, {:ok, Transformer.add_entity(dsl_state, [:attributes], attribute, type: :append)}}

  defp add_entity({:error, error}, _dsl_state, relationship),
    do:
      {:halt,
       {:error,
        DslError.exception(
          message:
            "Could not create attribute for belongs_to #{relationship.name}: #{inspect(error)}",
          path: [:relationships, relationship.name]
        )}}

  def after?(Ash.Resource.Transformers.BelongsToSourceField), do: true
  def after?(_), do: false

  def before?(Ash.Resource.Transformers.SetTypes), do: true
  def before?(_), do: false
end
