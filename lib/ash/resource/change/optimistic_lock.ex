defmodule Ash.Resource.Change.OptimisticLock do
  @moduledoc """
  Performs an optimistic lock on the changeset.

  See `Ash.Resource.Change.Builtins.optimistic_lock/1` for more.
  """
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, _context) do
    Ash.Changeset.filter(
      changeset,
      {opts[:attribute], [eq: Map.get(changeset.data, opts[:attribute])]}
    )
  end

  @impl true
  def atomic(changeset, opts, _context) do
    case changeset.data do
      %Ash.Changeset.OriginalDataNotAvailable{} ->
        {:not_atomic,
         "Cannot use `optimistic_lock` when updating a query, because we must know the previous version."}

      data ->
        {:atomic,
         %{
           opts[:attribute] =>
             expr(
               if ^ref(opts[:attribute]) == ^Map.get(data, opts[:attribute]) do
                 ^ref(opts[:attribute]) + 1
               else
                 error(Ash.Error.Changes.StaleRecord, %{
                   resource: changeset.resource,
                   filters: %{
                     ^opts[:attribute] => ^Map.get(data, opts[:attribute])
                   }
                 })
               end
             )
         }}
    end
  end
end
