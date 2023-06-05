defmodule Ash.Resource.Change.Load do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset

  def change(changeset, opts, _context) do
    Changeset.load(changeset, opts[:target])
  end
end
