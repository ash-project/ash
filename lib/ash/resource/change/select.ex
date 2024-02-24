defmodule Ash.Resource.Change.Select do
  @moduledoc false
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, _) do
    if opts[:ensure?] do
      Ash.Changeset.ensure_selected(changeset, opts[:target] || [])
    else
      Ash.Changeset.select(changeset, opts[:target] || [])
    end
  end

  @impl true
  def atomic(changeset, opts, _context) do
    if opts[:ensure?] do
      {:ok, Ash.Changeset.ensure_selected(changeset, opts[:target] || [])}
    else
      {:ok, Ash.Changeset.select(changeset, opts[:target] || [])}
    end
  end
end
