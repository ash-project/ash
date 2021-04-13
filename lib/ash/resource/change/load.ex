defmodule Ash.Resource.Change.Load do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset

  def change(changeset, opts, _) do
    Changeset.after_action(changeset, fn changeset, result ->
      changeset.api.load(result, opts[:target])
    end)
  end
end
