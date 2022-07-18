defmodule Ash.Resource.Transformers.DefaultAccept do
  @moduledoc "Sets the default `accept` for each action"

  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(resource, dsl_state) do
    public_attribute_names =
      resource
      |> Ash.Resource.Info.public_attributes()
      |> Enum.map(& &1.name)

    default_accept =
      Transformer.get_option(
        dsl_state,
        [:actions],
        :default_accept
      ) || public_attribute_names

    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reject(&(&1.type == :read))
    |> Enum.reduce({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      accept =
        case {action.accept, action.reject} do
          {_, :all} -> []
          {nil, reject} -> reject(default_accept, reject)
          {:all, reject} -> reject(public_attribute_names, reject)
          {accept, reject} -> reject(accept, reject)
        end

      new_dsl_state =
        Transformer.replace_entity(
          dsl_state,
          [:actions],
          %{action | accept: accept},
          &(&1.name == action.name && &1.type == action.type)
        )

      {:ok, new_dsl_state}
    end)
  end

  defp reject(list, reject) do
    Enum.reject(list, &(&1 in reject))
  end

  def after?(Ash.Resource.Transformers.BelongsToSourceField), do: true
  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(Ash.Resource.Transformers.CreateJoinRelationship), do: true
  def after?(_), do: false
end
