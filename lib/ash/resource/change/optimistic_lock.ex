defmodule Ash.Resource.Change.OptimisticLock do
  @moduledoc """
  Performs an optimistic lock on the changeset.

  See `Ash.Resource.Change.Builtins.optimistic_lock/1` for more.
  """
  use Ash.Resource.Change

  def change(changeset, opts, _context) do
    Ash.Changeset.filter(changeset, %{
      opts[:attribute] => Map.get(changeset.data, opts[:attribute])
    })
  end

  def atomic(_changeset, _opts, _context) do
    :ok
  end
end
