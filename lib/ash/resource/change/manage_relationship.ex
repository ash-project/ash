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
        destination = Ash.Resource.Info.related(changeset.resource, opts[:relationship])
        argument_value = from_structs(argument_value, destination)

        manage_opts =
          opts[:opts]
          |> Kernel.||([])
          |> Keyword.put_new(:meta, [])
          |> Keyword.update!(:meta, &Keyword.put(&1, :id, opts[:argument]))

        Ash.Changeset.manage_relationship(
          changeset,
          opts[:relationship],
          argument_value,
          manage_opts
        )

      :error ->
        changeset
    end
  end

  defp from_structs(argument_value, destination) when is_list(argument_value) do
    Enum.map(argument_value, &from_structs(&1, destination))
  end

  defp from_structs(%destination{} = value, destination) do
    value
  end

  defp from_structs(%struct{} = value, _destination) do
    if Ash.Resource.Info.resource?(struct) do
      if Ash.Resource.Info.embedded?(struct) do
        attrs = struct |> Ash.Resource.Info.attributes() |> Enum.map(& &1.name)
        Map.take(value, attrs)
      else
        value
      end
    else
      value
    end
  end

  defp from_structs(other, _), do: other
end
