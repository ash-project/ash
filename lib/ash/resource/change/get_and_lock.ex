defmodule Ash.Resource.Change.GetAndLock do
  @moduledoc """
  Refetches the record being updated or destroyed, and locks it with the given type.
  """
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, context) do
    Ash.Changeset.before_action(changeset, fn changeset ->
      primary_key = Ash.Resource.Info.primary_key(changeset.resource)
      pkey_values = changeset.data |> Map.take(primary_key) |> Map.to_list()

      case Ash.get(
             changeset.resource,
             pkey_values,
             domain: changeset.domain,
             tracer: context.tracer,
             tenant: context.tenant,
             authorize?: false,
             lock: opts[:lock]
           ) do
        {:ok, record} ->
          %{changeset | data: record}

        {:error, error} ->
          Ash.Changeset.add_error(changeset, error)
      end
    end)
  end

  @impl true
  def atomic(_changeset, _, _) do
    :ok
  end
end
