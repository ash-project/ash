defmodule Ash.Resource.Change.Atomic do
  @moduledoc false
  use Ash.Resource.Change

  def change(changeset, opts, _) do
    Ash.Changeset.atomic_update(changeset, opts[:attribute], opts[:expr])
  end
end
