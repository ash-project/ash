defmodule Ash.Notifier.PubSub.Helpers do
  @moduledoc """
  Helper functions for PubSub notifier integration.
  """

  alias Ash.Notifier.PubSub

  @doc """
  Adds required loads from topic generators to a changeset or query.

  This function inspects all PubSub publications configured for the resource
  and action, extracts load requirements from topic generators that implement
  the `required_loads/0` callback, and adds them to the changeset/query.

  This ensures that when notifications are created, the data already has
  the required relationships loaded, preventing N+1 queries in bulk operations.
  """
  def add_topic_generator_loads(%Ash.Changeset{} = changeset) do
    add_loads_for_action(changeset, changeset.resource, changeset.action)
  end

  def add_topic_generator_loads(%Ash.Query{} = query) do
    # For queries, we might need to handle this differently
    # For now, return unchanged
    query
  end

  defp add_loads_for_action(changeset_or_query, resource, action) when not is_nil(action) do
    # Get all PubSub notifiers for this resource
    resource
    |> Ash.Resource.Info.notifiers()
    |> Enum.filter(&match?(Ash.Notifier.PubSub, &1))
    |> Enum.reduce(changeset_or_query, fn _notifier, acc ->
      # Get publications from the notifier
      resource
      |> PubSub.Info.publications()
      |> Enum.filter(&publication_matches?(&1, action))
      |> Enum.reduce(acc, &add_publication_loads/2)
    end)
  end

  defp add_loads_for_action(changeset_or_query, _resource, _action) do
    changeset_or_query
  end

  defp publication_matches?(%{action: pub_action}, %{name: action_name})
       when is_atom(pub_action) do
    pub_action == action_name
  end

  defp publication_matches?(%{type: pub_type}, %{type: action_type}) do
    pub_type == action_type
  end

  defp publication_matches?(%{type: pub_type, except: except}, %{
         type: action_type,
         name: action_name
       }) do
    pub_type == action_type && action_name not in except
  end

  defp publication_matches?(_, _), do: false

  defp add_publication_loads(publication, acc) do
    case publication.topic do
      {topic_generator, _opts} when is_atom(topic_generator) ->
        add_topic_generator_loads_from_module(acc, topic_generator)

      topic_generator when is_atom(topic_generator) ->
        add_topic_generator_loads_from_module(acc, topic_generator)

      _ ->
        # String, list, or function topics don't have load requirements
        acc
    end
  end

  defp add_topic_generator_loads_from_module(changeset_or_query, topic_generator) do
    if function_exported?(topic_generator, :required_loads, 0) do
      loads = topic_generator.required_loads()

      # Handle both map and list formats
      load_list =
        case loads do
          %{} = load_map ->
            load_map
            |> Map.values()
            |> List.flatten()
            |> Enum.uniq()

          load_list when is_list(load_list) ->
            List.flatten([load_list])
            |> Enum.uniq()

          single_load ->
            [single_load]
        end

      case changeset_or_query do
        %Ash.Changeset{} = changeset ->
          Ash.Changeset.load(changeset, load_list)

        %Ash.Query{} = query ->
          Ash.Query.load(query, load_list)
      end
    else
      changeset_or_query
    end
  end
end
