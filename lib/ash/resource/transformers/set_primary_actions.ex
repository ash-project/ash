defmodule Ash.Resource.Transformers.ValidatePrimaryActions do
  @moduledoc """
  Validates the primary action configuration

  If multiple primary actions exist this results in an error.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  @extension Ash.Resource.Dsl

  def transform(dsl_state) do
    dsl_state = add_defaults(dsl_state)

    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.group_by(& &1.type)
    |> Map.put_new(:read, [])
    |> Map.put_new(:update, [])
    |> Map.put_new(:create, [])
    |> Map.put_new(:destroy, [])
    |> Enum.reduce_while({:ok, dsl_state}, fn
      {_type, []}, {:ok, dsl_state} ->
        {:cont, {:ok, dsl_state}}

      {type, actions}, {:ok, dsl_state} ->
        case Enum.count_until(actions, & &1.primary?, 2) do
          2 ->
            {:halt,
             {:error,
              DslError.exception(
                message:
                  "Multiple actions of type #{type} configured as `primary?: true`, but only one action per type can be the primary",
                path: [:actions, type]
              )}}

          _ ->
            {:cont, {:ok, dsl_state}}
        end
    end)
  end

  defp add_defaults(dsl_state) do
    actions = Transformer.get_entities(dsl_state, [:actions])

    default_defaults =
      if Transformer.get_persisted(dsl_state, :embedded?) do
        [:create, :read, :update, :destroy]
        |> Enum.reject(fn action_name ->
          Enum.any?(actions, &(&1.name == action_name))
        end)
      else
        []
      end

    dsl_state
    |> Transformer.get_option([:actions], :defaults)
    |> Kernel.||(default_defaults)
    |> Enum.with_index()
    |> Enum.reduce(dsl_state, fn {type, i}, dsl_state ->
      unless type in [:create, :update, :read, :destroy] do
        raise Spark.Error.DslError,
          path: [:actions, :default_actions, i],
          message: "#{type} is not a valid action type"
      end

      primary? = !Enum.any?(actions, &(&1.type == type && &1.primary?))

      {:ok, action} =
        Transformer.build_entity(@extension, [:actions], type,
          name: type,
          primary?: primary?
        )

      Transformer.add_entity(dsl_state, [:actions], action)
    end)
  end
end
