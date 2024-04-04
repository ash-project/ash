defmodule Ash.Resource.Change.UpdateChange do
  @moduledoc false
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, _) do
    Ash.Changeset.before_action(
      changeset,
      &Ash.Changeset.update_change(&1, opts[:attribute], opts[:function])
    )
  end
end
