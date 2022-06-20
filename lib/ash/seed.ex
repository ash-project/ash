defmodule Ash.Seed do
  def seed!(%{__meta__: %{state: :loaded}} = input) do
    input
  end

  def seed!(%resource{} = input) do
    input =
      input
      |> Map.from_struct()
      |> Enum.reduce(%{}, fn
        {_, %Ash.NotLoaded{}}, acc ->
          acc

        {_, nil}, acc ->
          acc

        {key, value}, acc ->
          if Ash.Resource.Info.attribute(resource, key) ||
               Ash.Resource.Info.relationship(resource, key) do
            Map.put(acc, key, value)
          else
            acc
          end
      end)

    seed!(
      resource,
      input
    )
  end

  def seed!(records) when is_list(records) do
    Enum.map(records, &seed!/1)
  end

  def seed!(resource, input) when is_list(input) do
    Enum.map(input, &seed!(resource, &1))
  end

  def seed!(resource, %resource{} = input) do
    seed!(input)
  end

  def seed!(resource, %other{}) do
    raise "Cannot seed #{inspect(resource)} with an input of type #{inspect(other)}"
  end

  def seed!(resource, input) when is_map(input) do
    resource
    |> Ash.Changeset.new()
    |> change_attributes(input)
    |> change_relationships(input)
    |> Ash.Changeset.set_defaults(:create, true)
    |> create_via_data_layer()
    |> case do
      {:ok, result, _, _} ->
        result

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  defp create_via_data_layer(changeset) do
    Ash.Changeset.with_hooks(changeset, fn changeset ->
      Ash.DataLayer.create(changeset.resource, changeset)
    end)
  end

  defp change_attributes(changeset, input) do
    Enum.reduce(input, changeset, fn {key, value}, changeset ->
      case Ash.Resource.Info.attribute(changeset.resource, key) do
        nil ->
          changeset

        attribute ->
          Ash.Changeset.force_change_attribute(changeset, attribute.name, value)
      end
    end)
  end

  defp change_relationships(changeset, input) do
    Enum.reduce(input, changeset, fn {key, value}, changeset ->
      case Ash.Resource.Info.relationship(changeset.resource, key) do
        nil ->
          changeset

        %{
          type: :belongs_to,
          source_field: source_field,
          destination_field: destination_field,
          destination: destination,
          name: name
        } ->
          Ash.Changeset.around_action(changeset, fn changeset, callback ->
            related = seed!(destination, value)

            changeset
            |> Ash.Changeset.force_change_attribute(
              source_field,
              Map.get(related, destination_field)
            )
            |> callback.()
            |> case do
              {:ok, result, changeset, instructions} ->
                {:ok, Map.put(result, name, related), changeset, instructions}

              {:error, error} ->
                {:error, error}
            end
          end)

        %{
          type: :has_many,
          source_field: source_field,
          destination_field: destination_field,
          destination: destination,
          name: name
        } ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            related =
              value
              |> List.wrap()
              |> Enum.map(
                &update_or_seed!(
                  &1,
                  destination,
                  Map.get(result, source_field),
                  destination_field
                )
              )

            {:ok, Map.put(result, name, related)}
          end)

        %{
          type: :has_one,
          source_field: source_field,
          destination_field: destination_field,
          destination: destination,
          name: name
        } ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            if value do
              related =
                update_or_seed!(
                  value,
                  destination,
                  Map.get(result, source_field),
                  destination_field
                )

              {:ok, Map.put(result, name, related)}
            else
              {:ok, Map.put(result, name, nil)}
            end
          end)

        %{
          type: :many_to_many,
          source_field: source_field,
          source_field_on_join_table: source_field_on_join_table,
          destination_field_on_join_table: destination_field_on_join_table,
          join_relationship: join_relationship,
          destination_field: destination_field,
          destination: destination,
          through: through,
          name: name
        } ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            related = seed!(destination, List.wrap(value))

            through =
              Enum.map(related, fn related ->
                seed!(through, %{
                  source_field_on_join_table => Map.get(result, source_field),
                  destination_field_on_join_table => Map.get(related, destination_field)
                })
              end)

            {:ok, Map.merge(result, %{name => related, join_relationship => through})}
          end)
      end
    end)
  end

  defp update_or_seed!(
         %resource{} = record,
         resource,
         field_value,
         field
       ) do
    record = seed!(record)

    changeset =
      record
      |> Ash.Changeset.new()
      |> Ash.Changeset.force_change_attribute(field, field_value)

    case Ash.DataLayer.update(resource, changeset) do
      {:ok, result} ->
        result

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  defp update_or_seed!(input, resource, field_value, field) do
    seed!(
      resource,
      Map.put(input, field, field_value)
    )
  end
end
