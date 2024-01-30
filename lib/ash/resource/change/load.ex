defmodule Ash.Resource.Change.Load do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset

  @impl true
  def change(changeset, opts, _context) do
    Changeset.load(changeset, opts[:target])
  end

  @impl true
  def atomic(changeset, opts, _context) do
    {:atomic, Changeset.load(changeset, opts[:target]), %{}}
  end
end
