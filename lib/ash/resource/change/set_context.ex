defmodule Ash.Resource.Change.SetContext do
  @moduledoc false

  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, _context) do
    context =
      case opts[:context] do
        {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->
          apply(m, f, [changeset | a])

        other ->
          {:ok, other}
      end

    case context do
      {:ok, context} ->
        Ash.Changeset.set_context(changeset, context)

      {:error, error} ->
        Ash.Changeset.add_error(changeset, error)
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    {:atomic, change(changeset, opts, context), %{}}
  end
end
