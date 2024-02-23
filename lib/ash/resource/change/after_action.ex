defmodule Ash.Resource.Change.AfterAction do
  @moduledoc false
  use Ash.Resource.Change

  @doc false
  @spec change(Ash.Changeset.t(), keyword, Ash.Resource.Change.context()) :: Ash.Changeset.t()
  def change(changeset, opts, context) do
    Ash.Changeset.after_action(
      changeset,
      fn changeset, result ->
        opts[:callback].(changeset, result, context)
      end,
      prepend?: opts[:prepend?]
    )
  end
end
