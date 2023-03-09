defmodule Ash.Resource.Change.AfterTransaction do
  @moduledoc false
  use Ash.Resource.Change

  @doc false
  @spec change(Ash.Changeset.t(), keyword, Ash.Resource.Change.context()) :: Ash.Changeset.t()
  def change(changeset, opts, _) do
    Ash.Changeset.after_transaction(changeset, opts[:callback], prepend?: opts[:prepend?])
  end
end
