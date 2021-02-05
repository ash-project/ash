defmodule Ash.Resource.Change.ManageRelationship do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset

  def init(opts) do
    {:ok, opts}
  end

  def change(changeset, opts, _) do
    case Changeset.fetch_argument(changeset, opts[:argument]) do
      {:ok, argument_value} ->
        Ash.Changeset.manage_relationship(
          changeset,
          opts[:relationship],
          argument_value,
          opts[:opts]
        )

      :error ->
        changeset
    end
  end
end
