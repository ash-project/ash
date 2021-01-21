defmodule Ash.Resource.Change.PreventChange do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset

  def init(opts) do
    case validate_field(opts[:field]) do
      :ok ->
        {:ok, opts}

      other ->
        other
    end
  end

  defp validate_field(nil), do: {:error, "field is required"}
  defp validate_field(value) when is_atom(value), do: :ok
  defp validate_field(other), do: {:error, "field is invalid: #{inspect(other)}"}

  def change(changeset, opts, _) do
    Changeset.before_action(changeset, fn changeset ->
      Changeset.clear_change(changeset, opts[:field])
    end)
  end
end
