defmodule Ash.Authorization.Check.SettingAttribute do
  use Ash.Authorization.Check, action_types: [:create, :update]

  @impl true
  def describe(opts) do
    case Keyword.fetch(opts, :to) do
      {:ok, should_equal} ->
        "setting #{opts[:attribute_name]} to #{inspect(should_equal)}"

      :error ->
        "setting #{opts[:attribute_name]}"
    end
  end

  @impl true
  def strict_check(_user, %{changeset: %Ecto.Changeset{} = changeset}, opts) do
    case Ecto.Changeset.fetch_change(changeset, opts[:attribute_name]) do
      {:ok, value} ->
        case Keyword.fetch(opts, :to) do
          {:ok, should_equal} ->
            {:ok, value == should_equal}

          :error ->
            {:ok, true}
        end

      :error ->
        {:ok, false}
    end
  end
end
