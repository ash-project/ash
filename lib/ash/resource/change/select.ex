defmodule Ash.Resource.Change.Select do
  @moduledoc false
  use Ash.Resource.Change

  def change(changeset, opts, _) do
    if opts[:ensure?] do
      Ash.Changeset.ensure_selected(changeset, opts[:target] || [])
    else
      Ash.Changeset.select(changeset, opts[:target] || [])
    end
  end
end
