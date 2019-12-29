defmodule Ash.Authorization.Check.SettingAttribute do
  use Ash.Authorization.Check, action_types: [:create, :update]

  @impl true
  def describe(opts) do
    "setting #{opts[:attribute_name]} to #{inspect(opts[:to])}"
  end

  @impl true
  def strict_check(_user, %{changeset: %Ecto.Changeset{} = changeset}, opts) do
    case Ecto.Changeset.fetch_change(changeset, opts[:attribute_name]) do
      {:ok, value} -> {:ok, value == opts[:to]}
      :error -> {:ok, false}
    end
  end
end
