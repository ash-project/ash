defmodule Ash.Resource.Transformers.SetPrimaryActions do
  @moduledoc """
  Creates/validates the primary action configuration

  If only one action of a given type is defined, it is marked
  as primary. If multiple exist, and one is not primary,
  this results in an error.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  alias Ash.Error.ResourceDslError

  @extension Ash.Dsl

  def transform(_resource, dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions], @extension)
    |> Enum.group_by(& &1.type)
    |> Enum.reduce_while({:ok, dsl_state}, fn
      {type, [action]}, {:ok, dsl_state} ->
        {:cont,
         {:ok,
          Transformer.replace_entity(
            dsl_state,
            [:actions],
            @extension,
            %{action | primary?: true},
            fn replacing_action ->
              replacing_action.name == action.name && replacing_action.type == type
            end
          )}}

      {type, actions}, {:ok, dsl_state} ->
        case Enum.count(actions, & &1.primary?) do
          0 ->
            {:halt,
             {:error,
              ResourceDslError.exception(
                message:
                  "Multiple actions of type create defined, one must be designated as `primary?: true`",
                path: [:actions, type]
              )}}

          1 ->
            {:cont, {:ok, dsl_state}}

          2 ->
            {:halt,
             {:error,
              ResourceDslError.exception(
                message:
                  "Multiple actions of type #{type} configured as `primary?: true`, but only one action per type can be the primary",
                path: [:actions, type]
              )}}
        end
    end)
  end
end
