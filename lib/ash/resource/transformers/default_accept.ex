defmodule Ash.Resource.Transformers.DefaultAccept do
  @moduledoc "Sets the default `accept` for each action"

  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(resource, dsl_state) do
    default_accept =
      resource
      |> Ash.Resource.Info.public_attributes()
      |> Enum.map(& &1.name)

    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reject(&(&1.type == :read))
    |> Enum.reduce({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      if action.accept do
        if action.reject do
          new_dsl_state =
            Transformer.replace_entity(
              dsl_state,
              [:actions],
              %{action | accept: Enum.reject(action.accept, &(&1 in action.reject))},
              &(&1.name == action.name && &1.type == action.type)
            )

          {:ok, new_dsl_state}
        else
          {:ok, dsl_state}
        end
      else
        if action.reject do
          new_dsl_state =
            Transformer.replace_entity(
              dsl_state,
              [:actions],
              %{action | accept: Enum.reject(default_accept, &(&1 in action.reject))},
              &(&1.name == action.name && &1.type == action.type)
            )

          {:ok, new_dsl_state}
        else
          new_dsl_state =
            Transformer.replace_entity(
              dsl_state,
              [:actions],
              %{action | accept: default_accept},
              &(&1.name == action.name && &1.type == action.type)
            )

          {:ok, new_dsl_state}
        end
      end
    end)
  end

  def after?(Ash.Resource.Transformers.BelongsToSourceField), do: true
  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(Ash.Resource.Transformers.CreateJoinRelationship), do: true
  def after?(_), do: false
end
