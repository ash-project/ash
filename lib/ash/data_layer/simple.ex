defmodule Ash.DataLayer.Simple do
  @moduledoc "A data layer that simply returns structs"

  def can?(_, :read), do: true
  def can?(_, :create), do: true
  def can?(_, :update), do: true
  def can?(_, :destroy), do: true
  def can?(_, _), do: false

  def create(_resource, changeset) do
    {:ok, Ash.Changeset.apply_attributes(changeset)}
  end

  def update(_resource, changeset) do
    {:ok, Ash.Changeset.apply_attributes(changeset)}
  end

  def destroy(_resource, _changeset) do
    :ok
  end
end
