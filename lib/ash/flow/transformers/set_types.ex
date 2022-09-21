defmodule Ash.Flow.Transformers.SetTypes do
  @moduledoc "Sets the actual types and transforms the type constraints"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    set_argument_types(dsl_state)
  end

  defp set_argument_types(dsl_state) do
    arguments = Transformer.get_entities(dsl_state, [:flow])

    new_arguments =
      arguments
      |> Enum.reduce_while({:ok, []}, fn argument, {:ok, args} ->
        type = Ash.Type.get_type(argument.type)

        case validate_constraints(type, argument.constraints) do
          {:ok, constraints} ->
            {:cont, {:ok, [%{argument | type: type, constraints: constraints} | args]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)

    case new_arguments do
      {:ok, new_args} ->
        {:ok,
         Enum.reduce(new_args, dsl_state, fn new_arg, dsl_state ->
           Transformer.replace_entity(
             dsl_state,
             [:flow],
             new_arg,
             fn replacing ->
               replacing.name == new_arg.name
             end
           )
         end)}

      {:error, error} ->
        {:error, error}
    end
  end

  def validate_constraints(type, constraints) do
    case type do
      {:array, type} ->
        with {:ok, new_constraints} <-
               Spark.OptionsHelpers.validate(
                 Keyword.delete(constraints, :items),
                 Ash.Type.array_constraints(type)
               ),
             {:ok, item_constraints} <- validate_item_constraints(type, constraints) do
          {:ok, Keyword.put(new_constraints, :items, item_constraints)}
        end

      type ->
        schema = Ash.Type.constraints(type)

        case Spark.OptionsHelpers.validate(constraints, schema) do
          {:ok, constraints} ->
            validate_none_reserved(constraints, type)

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp validate_item_constraints(type, constraints) do
    schema = Ash.Type.constraints(type)

    case Spark.OptionsHelpers.validate(constraints[:items] || [], schema) do
      {:ok, item_constraints} ->
        validate_none_reserved(item_constraints, type)

      {:error, error} ->
        {:error, error}
    end
  end

  @reserved ~w(default source autogenerate read_after_writes virtual primary_key load_in_query redact)a

  defp validate_none_reserved(constraints, type) do
    case Enum.find(@reserved, &Keyword.has_key?(constraints, &1)) do
      nil ->
        {:ok, constraints}

      key ->
        {:error,
         "Invalid constraint key #{key} in type #{inspect(type)}. This name is reserved due to the underlying ecto implementation."}
    end
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(_), do: false
end
