defmodule Ash.Resource.Transformers.BelongsToAttribute do
  @moduledoc """
  Creates the attribute for belongs_to relationships that have `define_field?: true`
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  alias Ash.Error.Dsl.DslError

  @extension Ash.Resource.Dsl

  def transform(_resource, dsl_state) do
    dsl_state
    |> Transformer.get_entities([:relationships])
    |> Enum.filter(&(&1.type == :belongs_to))
    |> Enum.filter(& &1.define_field?)
    |> Enum.reject(fn relationship ->
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.find(&(Map.get(&1, :name) == relationship.source_field))
    end)
    |> Enum.reduce_while({:ok, dsl_state}, fn relationship, {:ok, dsl_state} ->
      case Transformer.build_entity(@extension, [:attributes], :attribute,
             name: relationship.source_field,
             type: relationship.field_type,
             writable?: false,
             primary_key?: relationship.primary_key?
           ) do
        {:ok, attribute} ->
          {:cont, {:ok, Transformer.add_entity(dsl_state, [:attributes], attribute)}}

        {:error, error} ->
          {:halt,
           {:error,
            DslError.exception(
              module: __MODULE__,
              message:
                "Could not create attribute for belongs_to #{relationship.name}: #{inspect(error)}",
              path: [:relationships, relationship.name]
            )}}
      end
    end)
  end

  def after?(Ash.Resource.Transformers.BelongsToSourceField), do: true
  def after?(_), do: false
end
