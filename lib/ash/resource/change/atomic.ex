defmodule Ash.Resource.Change.Atomic do
  @moduledoc false
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, _) do
    if opts[:cast_atomic?] do
      Ash.Changeset.atomic_update(changeset, opts[:attribute], opts[:expr])
    else
      Ash.Changeset.atomic_update(changeset, opts[:attribute], {:atomic, opts[:expr]})
    end
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
