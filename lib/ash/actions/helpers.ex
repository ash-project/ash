defmodule Ash.Actions.Helpers do
  @moduledoc false
  require Logger

  def validate_calculation_load!(%Ash.Query{}, module) do
    raise """
    `#{inspect(module)}.load/3` returned a query.

    Returning a query from the `load/3` callback of a calculation is now deprecated.
    Instead, return the load statement itself, i.e instead of `Ash.Query.load(query, [...])`,
    just return `[...]`. This is so that Ash can examine the requirements of just this single
    calculation to ensure that all required values are present
    """
  end

  def validate_calculation_load!(other, _), do: other

  def warn_missed!(resource, action, result) do
    case Map.get(result, :resource_notifications, []) do
      empty when empty in [nil, []] ->
        :ok

      missed ->
        case Application.get_env(:ash, :missed_notifications, :ignore) do
          :ignore ->
            :ok

          :raise ->
            raise """
            Missed #{Enum.count(missed)} notifications in action #{inspect(resource)}.#{action.name}.

            This happens when the resources are in a transaction, and you did not pass
            `return_notifications?: true`. If you are in a changeset hook, you can simply
            return the notifications. If not, you can send the notifications using
            `Ash.Notifier.notify/1` once your resources are out of a transaction.
            """

          :warn ->
            {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

            Logger.warn("""
            Missed #{Enum.count(missed)} notifications in action #{inspect(resource)}.#{action.name}.

            This happens when the resources are in a transaction, and you did not pass
            `return_notifications?: true`. If you are in a changeset hook, you can simply
            return the notifications. If not, you can send the notifications using
            `Ash.Notifier.notify/1` once your resources are out of a transaction.

            #{Exception.format_stacktrace(stacktrace)}
            """)
        end
    end
  end

  def process_errors(changeset, [error]) do
    %{changeset | errors: []}
    |> Ash.Changeset.add_error(error)
    |> Map.get(:errors)
    |> case do
      [error] ->
        error

      errors ->
        errors
    end
  end

  def process_errors(changeset, errors) when is_list(errors) do
    %{changeset | errors: []}
    |> Ash.Changeset.add_error(errors)
    |> Map.get(:errors)
  end

  def process_errors(changeset, error), do: process_errors(changeset, [error])

  def select({:ok, results}, query) do
    {:ok, select(results, query)}
  end

  def select({:error, error}, _query) do
    {:error, error}
  end

  def select(results, query) when is_list(results) do
    Enum.map(results, &select(&1, query))
  end

  def select(nil, _), do: nil

  def select(result, %{select: nil}) do
    result
  end

  def select(result, %{resource: resource, select: select}) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.flat_map(fn attribute ->
      if attribute.always_select? || attribute.primary_key? || attribute.name in select do
        []
      else
        [attribute.name]
      end
    end)
    |> Enum.reduce(result, fn key, record ->
      Map.put(record, key, nil)
    end)
    |> Ash.Resource.Info.put_metadata(:selected, select)
  end

  def attributes_to_select(%{select: nil, resource: resource}) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.map(& &1.name)
  end

  def attributes_to_select(%{select: select, resource: resource}) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.flat_map(fn attribute ->
      if attribute.always_select? || attribute.primary_key? || attribute.name in select do
        [attribute.name]
      else
        []
      end
    end)
  end
end
