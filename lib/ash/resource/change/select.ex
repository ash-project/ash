defmodule Ash.Resource.Change.Select do
  @moduledoc false
  use Ash.Resource.Change

  def change(changeset, opts, _) do
    Ash.Changeset.select(changeset, opts[:target] || [])
  end
end
