defmodule Ash.Resource.Change.BeforeAction do
  @moduledoc false
  use Ash.Resource.Change

  @doc false
  @spec change(Ash.Changeset.t(), keyword, Ash.Resource.Change.context()) :: Ash.Changeset.t()
  def change(changeset, opts, _) do
    Ash.Changeset.before_action(changeset, opts[:callback], append?: opts[:append?])
  end
end
