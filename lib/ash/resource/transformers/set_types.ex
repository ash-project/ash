defmodule Ash.Resource.Transformers.SetTypes do
  @moduledoc "Sets the `source` key on relationships to be the resource they were defined on"
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(_resource, dsl_state) do
    with {:ok, dsl_state} <- set_attribute_types(dsl_state),
         {:ok, dsl_state} <- set_argument_types(dsl_state),
         {:ok, dsl_state} <- set_metadata_types(dsl_state),
         {:ok, dsl_state} <- set_calculation_types(dsl_state) do
      {:ok, dsl_state}
    end
  end

  defp set_attribute_types(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:attributes])
    |> Enum.reduce_while({:ok, dsl_state}, fn attribute, {:ok, dsl_state} ->
      type = Ash.Type.get_type(attribute.type)

      case validate_constraints(type, attribute.constraints) do
        {:ok, constraints} ->
          {:cont,
           {:ok,
            Transformer.replace_entity(
              dsl_state,
              [:attributes],
              %{attribute | type: type, constraints: constraints},
              fn replacing ->
                replacing.name == attribute.name
              end
            )}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp set_argument_types(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reduce_while({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      new_arguments =
        action.arguments
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
          {:cont,
           {:ok,
            Transformer.replace_entity(
              dsl_state,
              [:actions],
              %{action | arguments: Enum.reverse(new_args)},
              fn replacing ->
                replacing.name == action.name && replacing.type == action.type
              end
            )}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp set_metadata_types(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.filter(&Map.has_key?(&1, :metadata))
    |> Enum.reduce_while({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      new_metadatas =
        action.metadata
        |> Enum.reduce_while({:ok, []}, fn metadata, {:ok, metadatas} ->
          type = Ash.Type.get_type(metadata.type)

          case validate_constraints(type, metadata.constraints) do
            {:ok, constraints} ->
              {:cont, {:ok, [%{metadata | type: type, constraints: constraints} | metadatas]}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)

      case new_metadatas do
        {:ok, new_metadatas} ->
          {:cont,
           {:ok,
            Transformer.replace_entity(
              dsl_state,
              [:actions],
              %{action | metadata: Enum.reverse(new_metadatas)},
              fn replacing ->
                replacing.name == action.name && replacing.type == action.type
              end
            )}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp set_calculation_types(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:calculations])
    |> Enum.reduce_while({:ok, dsl_state}, fn calculation, {:ok, dsl_state} ->
      type = Ash.Type.get_type(calculation.type)

      case validate_constraints(type, calculation.constraints) do
        {:ok, constraints} ->
          calculation = %{calculation | type: type, constraints: constraints}

          new_arguments =
            calculation.arguments
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
              {:cont,
               {:ok,
                Transformer.replace_entity(
                  dsl_state,
                  [:calculations],
                  %{calculation | arguments: Enum.reverse(new_args)},
                  fn replacing ->
                    replacing.name == calculation.name
                  end
                )}}

            {:error, error} ->
              {:halt, {:error, error}}
          end

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  def validate_constraints(type, constraints) do
    case type do
      {:array, type} ->
        with {:ok, new_constraints} <-
               Ash.OptionsHelpers.validate(
                 Keyword.delete(constraints, :items),
                 Ash.Type.array_constraints(type)
               ),
             {:ok, item_constraints} <- validate_item_constraints(type, constraints) do
          {:ok, Keyword.put(new_constraints, :items, item_constraints)}
        end

      type ->
        schema = Ash.Type.constraints(type)

        case Ash.OptionsHelpers.validate(constraints, schema) do
          {:ok, constraints} ->
            {:ok, constraints}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp validate_item_constraints(type, constraints) do
    schema = Ash.Type.constraints(type)

    case Ash.OptionsHelpers.validate(constraints[:items] || [], schema) do
      {:ok, item_constraints} ->
        {:ok, item_constraints}

      {:error, error} ->
        {:error, error}
    end
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(_), do: false
end
