defmodule Ash.Test.TypeInference.PrivateChange do
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context), do: validate(changeset)

  defp validate(changeset), do: changeset.arguments.missing

  def capture_pair(items), do: Enum.reduce(items, &{&1, &2})
end
