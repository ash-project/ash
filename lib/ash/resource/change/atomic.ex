defmodule Ash.Resource.Change.Atomic do
  @moduledoc false
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, _) do
    Ash.Changeset.atomic_update(changeset, opts[:attribute], opts[:expr])
  end

  @impl true
  def atomic(_changeset, opts, _context) do
    if opts[:cast_atomic?] do
      {:atomic, %{opts[:attribute] => opts[:expr]}}
    else
      {:atomic, %{opts[:attribute] => {:atomic, opts[:expr]}}}
    end
  end
end
