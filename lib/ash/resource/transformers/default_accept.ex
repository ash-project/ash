defmodule Ash.Resource.Transformers.DefaultAccept do
  @moduledoc "Sets the default `accept` for each action"

  use Ash.Dsl.Transformer

  alias Ash.Dsl.Extension
  alias Ash.Dsl.Transformer

  def transform(resource, dsl_state) do
    resource_attribute_names =
      resource
      |> Ash.Resource.Info.public_attributes()
      |> Enum.map(& &1.name)

    # TODO: This needs to validate the given attribute names
    default_accept =
      Extension.get_opt(
        resource,
        [:default_accept],
        :defaults,
        resource_attribute_names
      )

    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reject(&(&1.type == :read))
    |> Enum.reduce({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      accept =
        if action.accept == :all do
          case action.reject do
            # This is a contradiction but could be achieved by passing all attributes manually, too.
            # Is it worth throwing an error?
            :all -> []
            nil -> default_accept
            reject when is_list(reject) -> Enum.reject(default_accept, &(&1 in action.reject))
          end
        else
          if action.reject do
            Enum.reject(action.accept, &(&1 in action.reject))
          else
            default_accept
          end
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

  def after?(Ash.Resource.Transformers.BelongsToSourceField), do: true
  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(Ash.Resource.Transformers.CreateJoinRelationship), do: true
  def after?(_), do: false
end
