defmodule Ash.Resource.Change.BeforeTransaction do
  @moduledoc false
  use Ash.Resource.Change

  @doc false
  @spec change(Ash.Changeset.t(), keyword, Ash.Resource.Change.context()) :: Ash.Changeset.t()
  def change(changeset, opts, context) do
    Ash.Changeset.before_transaction(
      changeset,
      fn changeset ->
        opts[:callback].(changeset, context)
      end,
      append?: opts[:append?]
    )
  end
end
