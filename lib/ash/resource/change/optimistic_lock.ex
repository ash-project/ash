defmodule Ash.Resource.Change.OptimisticLock do
  @moduledoc """
  Apply an "optimistic lock" on a record being updated or destroyed.

  ## What is Optimistic Locking?

  Optimistic Locking is the process of only allowing an update to occur if the version of a record that you have in memory is the same as the version in the database.
  Otherwise, an error is returned. On success, it increments the version while performing the action.

  Optimistic locking may used for two primary purposes:

  ### User Experience

  For example, if a user is editing a form that contains `State` and `County` fields, and they change the `County`, while another user has used the form to change the `State`, you could end up with a mismatch between `State` and `County`.

  With optimistic locking, the user will instead get an error message that the record has been changed since they last looked.

  ### Concurrency Safety

  Optimistic locking can make actions safe to run concurrently even if they can't be performed in a single query (atomically), by returning an error if the resource in the data layer does not have the same version as the one being edited.

  This tells the user that they need to reload and try again.
  """
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, context) do
    changeset
    |> Ash.Changeset.filter({opts[:attribute], [eq: Map.get(changeset.data, opts[:attribute])]})
    |> Ash.Resource.Change.Increment.change([amount: 1, attribute: opts[:attribute]], context)
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
                   resource: ^inspect(changeset.resource),
                   filter: %{
                     ^opts[:attribute] => ^Map.get(data, opts[:attribute])
                   }
                 })
               end
             )
         }}
    end
  end
end
