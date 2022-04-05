defmodule Ash.Resource.Transformers.ValidatePrimaryActions do
  @moduledoc """
  Validates the primary action configuration

  If multiple primary actions exist this results in an error.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  alias Ash.Error.Dsl.DslError

  @extension Ash.Resource.Dsl

  def transform(resource, dsl_state) do
    dsl_state = add_defaults(dsl_state, resource)

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

      {type, [action]}, {:ok, dsl_state} ->
        {:cont,
         {:ok,
          Transformer.replace_entity(
            dsl_state,
            [:actions],
            %{action | primary?: true},
            fn replacing_action ->
              replacing_action.name == action.name && replacing_action.type == type
            end
          )}}

      {type, actions}, {:ok, dsl_state} ->
        case Enum.count_until(actions, & &1.primary?, 2) do
          2 ->
            {:halt,
             {:error,
              DslError.exception(
                module: resource,
                message:
                  "Multiple actions of type #{type} configured as `primary?: true`, but only one action per type can be the primary",
                path: [:actions, type]
              )}}

          _ ->
            {:cont, {:ok, dsl_state}}
        end
    end)
  end

  defp add_defaults(dsl_state, resource) do
    actions = Transformer.get_entities(dsl_state, [:actions])

    resource
    |> Ash.Resource.Info.default_actions()
    |> Enum.with_index()
    |> Enum.reduce(dsl_state, fn {type, i}, dsl_state ->
      unless type in [:create, :update, :read, :destroy] do
        raise Ash.Error.Dsl.DslError,
          path: [:actions, :default_actions, i],
          module: resource,
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
