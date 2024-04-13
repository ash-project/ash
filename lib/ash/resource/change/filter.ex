defmodule Ash.Resource.Change.Filter do
  @moduledoc false
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, _) do
    Ash.Changeset.filter(changeset, opts[:filter])
  end

  @impl true
  def atomic(changeset, opts, context) do
    {:ok, change(changeset, opts, context)}
  end
end
